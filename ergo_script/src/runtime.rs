//! Script runtime definitions.
//!
//! The runtime is responsible for evaluating AST expressions, producing values or errors.
//! Importantly, it tracks source locations for values and errors so that when an error occurs,
//! useful error information can be provided.

use super::ast::{
    expr_dependencies, pattern_dependencies, ArrayPattern, CmdPatT, CompiledExpression, Expr,
    Expression, MapPattern, MergeExpression, Pat, PatT, Pattern,
};
use crate::constants::{
    DIR_NAME, EXTENSION, PLUGIN_ENTRY, PRELUDE_ARG, WORKSPACE_FALLBACK_ARG, WORKSPACE_NAME,
};
use abi_stable::{
    rvec,
    std_types::{ROption, RResult},
};
use ergo_runtime::source::{FileSource, IntoSource, Source};
use ergo_runtime::Result as EResult;
use ergo_runtime::{
    metadata, types, ContextEnv, ContextExt, EvalResult, FunctionArguments, FunctionCall,
    ResultIterator, Runtime, ScriptEnv, UncheckedFunctionArguments,
};
use futures::future::{BoxFuture, FutureExt};
use grease::{
    bst::BstMap,
    depends, make_value, match_value,
    path::PathBuf,
    types::GreaseType,
    value::{Errored, IntoValue, TypedValue, Value},
};
use libloading as dl;
use log::{debug, trace};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::path;
use std::str::FromStr;

/// Script type indicating that the env should be returned as a map.
#[derive(Clone, Copy, Debug, GreaseType, Hash)]
pub struct ScriptEnvIntoMap;

impl From<ScriptEnvIntoMap> for TypedValue<ScriptEnvIntoMap> {
    fn from(v: ScriptEnvIntoMap) -> Self {
        TypedValue::constant(v)
    }
}

grease::grease_traits_fn! {
    ergo_runtime::grease_type_name!(traits, ScriptEnvIntoMap, "set expression");
    ergo_runtime::traits::ValueByContent::add_impl::<ScriptEnvIntoMap>(traits);
}

/// Look at the file contents to determine if the file is a plugin (dynamic library).
fn is_plugin(f: &path::Path) -> bool {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(f).expect("could not open file for reading");
    if cfg!(target_os = "macos") {
        let mut magic: [u8; 4] = [0; 4];
        if file.read_exact(&mut magic).is_err() {
            return false;
        }
        return &magic == &[0xfe, 0xed, 0xfa, 0xce]
            || &magic == &[0xfe, 0xed, 0xfa, 0xcf]
            || &magic == &[0xcf, 0xfa, 0xed, 0xfe]
            || &magic == &[0xce, 0xfa, 0xed, 0xfe]
            || &magic == &[0xca, 0xfe, 0xba, 0xbe];
    } else if cfg!(target_os = "windows") {
        use std::io::{Seek, SeekFrom};

        // DOS header
        let mut m1: [u8; 2] = [0; 2];
        if file.read_exact(&mut m1).is_err() {
            return false;
        }
        if &m1 != b"MZ" && &m1 != b"ZM" {
            return false;
        }

        // PE header offset
        if file.seek(SeekFrom::Start(0x3c)).is_err() {
            return false;
        }
        let mut offset: [u8; 4] = [0; 4];
        if file.read_exact(&mut offset).is_err() {
            return false;
        }
        let offset = u32::from_ne_bytes(offset);

        // PE header
        if file.seek(SeekFrom::Start(offset as _)).is_err() {
            return false;
        }
        let mut magic: [u8; 4] = [0; 4];
        if file.read_exact(&mut magic).is_err() {
            return false;
        }
        return &magic == b"PE\0\0";
    } else if cfg!(target_os = "linux") {
        let mut magic: [u8; 4] = [0; 4];
        if let Err(_) = file.read_exact(&mut magic) {
            return false;
        }
        return &magic == b"\x7fELF";
    } else {
        panic!("unsupported operating system");
    }
}

/// Load and execute a script given the function call context.
pub fn load_script<'a>(ctx: &'a mut FunctionCall) -> BoxFuture<'a, EvalResult> {
    async move {
        let target = ctx.args.peek();
        let source = ctx.call_site.clone();

        fn script_path_exists<'a, P: 'a + AsRef<path::Path>>(
            name: P,
            try_add_extension: bool,
        ) -> impl FnMut(&path::Path) -> Option<path::PathBuf> + 'a {
            move |path| {
                if try_add_extension {
                    if let Some(file_name) = name.as_ref().file_name() {
                        let mut p = file_name.to_owned();
                        p.push(".");
                        p.push(EXTENSION);
                        let path_with_extension = path.join(name.as_ref()).with_file_name(p);
                        if path_with_extension.exists() {
                            Some(path_with_extension)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
                .or_else(|| {
                    let path_exact = path.join(name.as_ref());
                    if path_exact.exists() {
                        Some(path_exact)
                    } else {
                        None
                    }
                })
                .and_then(|mut p| {
                    while p.is_dir() {
                        // Prefer dir script to workspace
                        let dir = p.join(DIR_NAME);
                        if dir.exists() {
                            p = dir;
                        } else {
                            let ws = p.join(WORKSPACE_NAME);
                            if ws.exists() {
                                p = ws
                            } else {
                                break;
                            }
                        }
                    }
                    if p.is_file() {
                        Some(p)
                    } else {
                        None
                    }
                })
            }
        }

        let mut load_path = None;
        let mut was_workspace = false;

        let to_load: Option<std::path::PathBuf> = match target {
            None => None,
            Some(target) => {
                match_value!(target.clone().unwrap() => {
                    types::String => |v| Some(<&str>::from(v.await?.as_ref()).into()),
                    PathBuf => |v| Some(v.await?.owned().into()),
                    => |_| None
                }).await?
            }
        };

        // If target is a string or path, try to find it in the load path
        let to_load = match to_load {
            Some(target_path) => {
                load_path = Some(target_path.clone());

                // Get current load path
                let paths: Vec<std::path::PathBuf> = ctx.current_load_path.clone().into_iter().map(|v| v.into()).collect();

                // Try to find path match in load paths
                paths
                    .iter()
                    .map(|v| v.as_path())
                    .find_map(script_path_exists(target_path, true))
            },
            None => None
        };

        // Look for workspace if path-based lookup failed
        let to_load = match to_load {
            Some(v) => {
                // Consume the target, as it has been used to resolve the module.
                ctx.args.next();
                Some(v)
            }
            None => {
                was_workspace = true;

                // If the current file is a workspace, allow it to load from parent workspaces.
                let (path_basis, check_for_workspace) = if let ROption::RSome(v) = &ctx.mod_path {
                    (v.clone().into(), true)
                } else {
                    (ctx.mod_dir(), false)
                };

                let within_workspace = check_for_workspace && path_basis.file_name().map(|v| v == WORKSPACE_NAME).unwrap_or(false);

                let mut ancestors = path_basis.ancestors().peekable();
                if within_workspace {
                    while let Some(v) = ancestors.peek().and_then(|a| a.file_name()) {
                        if v == WORKSPACE_NAME {
                            ancestors.next();
                        } else {
                            break;
                        }
                    }
                    // Skip one more to drop parent directory of top-most workspace, which would find
                    // the same workspace as the original.
                    ancestors.next();
                }

                ancestors.find_map(script_path_exists(WORKSPACE_NAME, false))
            }
        };

        let mut was_loading = false;
        let to_load = to_load.and_then(|p| {
            for l_path in ctx.loading.lock().iter() {
                if l_path.as_ref() == p {
                    was_loading = true;
                    break;
                }
            }
            if was_loading {
                None
            } else {
                Some(p)
            }
        });

        // Load if some module was found.
        if let Some(p) = to_load {
            let p = p.canonicalize().unwrap(); // unwrap because is_file() should guarantee that canonicalize will succeed

            // FIXME if multiple threads are calling load, they may load the same script more than
            // once. This could be changed into a cache of futures that yield the resulting value,
            // though we need to clone contexts and the like. This was already attempted once but some
            // recursive type errors also came up with async blocks.
            let cache = ctx.load_cache.clone();
            let loaded = {
                // Since we are in an async function, we need to access the cache in a somewhat odd
                // pattern.
                let cached = {
                    let cache = cache.lock();
                    let ret = cache.get(&PathBuf::from(p.clone())).cloned();
                    if ret.is_none() {
                        // Exclude path from loading in nested calls.
                        // This should be done prior to prelude loading in the case where we are loading the
                        // prelude.
                        ctx.loading.lock().push(PathBuf::from(p.clone()));
                    }
                    ret
                };

                match cached {
                    None => {
                        let plugin = is_plugin(&p);

                        // Only load prelude if this is not a workspace nor a plugin.
                        let load_prelude = p.file_name().unwrap() != WORKSPACE_NAME && !plugin; // unwrap because p must have a final component

                        let mod_path = p.parent().unwrap(); // unwrap because file must exist in some directory

                        // Load the prelude. The prelude may not exist (without error), however if it does
                        // exist it must evaluate to a map.
                        let prelude = if load_prelude {
                            // Find ancestor workspace, if any, and use it to get the prelude.
                            if let Some(p) = mod_path.ancestors().find_map(script_path_exists(WORKSPACE_NAME, false)) {
                                // If ancestor workspace is already being loaded, do not load the prelude.
                                if ctx.loading.lock().iter().any(|o| o.as_ref() == p) {
                                    None
                                } else {
                                    let call_site = ctx.call_site.clone();
                                    let mut fctx = FunctionCall::new(
                                        ctx,
                                        FunctionArguments::positional(vec![
                                            Source::builtin(PathBuf::from(p.to_owned()).into()),
                                        ]),
                                        call_site,
                                    );

                                    // Load workspace
                                    let ws = match load_script(&mut fctx).await {
                                        Ok(ws) => ws,
                                        Err(e) => {
                                            ctx.loading.lock().pop();
                                            return Err(source.with("while running 'ergo [workspace] prelude' to load script").context_for_error(e));
                                        }
                                    };

                                    // Try to access prelude, but allow failures
                                    match Errored::ignore(apply_value(ctx, ws, FunctionArguments::new(
                                            vec![Source::builtin(types::String::from(PRELUDE_ARG).into())],
                                            Default::default()
                                        ).unchecked(), false)).await {
                                        Ok(v) => {
                                            // Prelude must be a map.
                                            let (v_source, v) = v.take();
                                            match_value!(v => {
                                                types::Map => |v| Some(v_source.with(v).await.transpose_ok()?.map(|v| v.owned().0)),
                                                => |_| Err(v_source.with(Error::CannotMerge(
                                                        "prelude did not evaluate to a map".into(),
                                                    )))?
                                            }).await?
                                        }
                                        Err(_) => {
                                            debug!("not loading prelude: workspace did not accept prelude argument");
                                            None
                                        }
                                    }
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        let result = if !plugin {
                            let mut script =
                                super::Script::load(Source::new(FileSource(p.clone())))?;
                            let mut top_level_env: ScriptEnv = Default::default();
                            if let Some(v) = prelude {
                                let (source, v) = v.take();
                                top_level_env.extend(
                                    v.into_iter()
                                        .map(|(k, v)| (k, Ok(source.clone().with(v)).into())),
                                );
                            }
                            script.top_level_env(top_level_env);
                            script.file_path(p.clone());
                            script.evaluate(ctx).await.into()
                        } else {
                            let lib = dl::Library::new(&p)?;
                            let f: dl::Symbol<
                                extern "C" fn(
                                    ergo_runtime::plugin::Context,
                                    &mut Runtime,
                                )
                                    -> RResult<Source<Value>, grease::Error>,
                            > = unsafe { lib.get(PLUGIN_ENTRY.as_bytes()) }?;
                            let result = f(ergo_runtime::plugin::Context::get(), ctx);
                            // Leak loaded libraries rather than storing them and dropping them in
                            // the context, as this can cause issues if the thread pool hasn't shut
                            // down.
                            // ctx.lifetime(lib);
                            std::mem::forget(lib);
                            result
                        };

                        let cache = ctx.load_cache.clone();
                        let mut cache = cache.lock();
                        cache.insert(PathBuf::from(p.clone()), result.clone());
                        ctx.loading.lock().pop();
                        result
                    }
                    Some(v) => v,
                }
                .into_result()?
            };

            let loaded_context = source.with(format!("loaded from '{}'", p.display()));

            // If resolved to an ancestor workspace, run arguments on
            // the result of `[loaded] command`.
            let loaded = if was_workspace {
                apply_value(ctx, loaded, FunctionArguments::new(
                        vec![Source::builtin(types::String::from(WORKSPACE_FALLBACK_ARG).into())],
                        Default::default()
                        ).unchecked(), false).await?
            } else {
                loaded
            };

            // If there are remaining arguments apply them immediately.
            if !ctx.args.is_empty() {
                let args = std::mem::take(&mut ctx.args);

                apply_value(ctx, loaded, args.unchecked(), false).await
            } else {
                ctx.unused_arguments()?;
                Ok(loaded)
            }.map(|v| v.map(|v| loaded_context.imbue_error_context(v)))
        } else {
            Err(Error::LoadFailed {
                was_loading,
                was_workspace,
                load_path,
            }
            .into())
        }
    }
    .boxed()
}

/// Script runtime errors.
#[derive(Debug)]
pub enum Error {
    /// An indexing operation was attempted on a type that is not an array or map.
    InvalidIndex,
    /// No binding with the given value is available in the current environment.
    MissingBinding(Value),
    /// An index is missing within an array.
    MissingArrayIndex(usize),
    /// No index with the given value exists within a map.
    MissingMapIndex(Value),
    /// An integer index (for arrays) was expected.
    NonIntegerIndex,
    /// A type that cannot be indexed was used in an index expression.
    NonIndexableValue(Value),
    /// An expression is in call-position (had arguments) but is not callable.
    NonCallableExpression(Value),
    /// A merge expression cannot be evaluated.
    CannotMerge(String),
    /// A map pattern has too many rest patterns.
    PatternMapTooManyRest,
    /// A map pattern had unmatched keys.
    PatternMapExtraKeys(Value),
    /// A rest pattern in an array is undecidable.
    PatternArrayRestUndecidable,
    /// A pattern did not match a value.
    PatternMismatch(Value),
    /// A command pattern has too many non-positional patterns.
    PatternCommandTooManyNonPositional,
    /// A command does not accept a particular non-positional argument.
    UnexpectedNonPositionalArgument,
    /// No patterns in a match expression matched a value.
    MatchFailed(Value),
    /// A command does not accept non-positional arguments.
    NoNonPositionalArguments,
    /// Arguments to a function did not match the function definition.
    ArgumentMismatch,
    /// A load failed to find a module.
    LoadFailed {
        was_loading: bool,
        was_workspace: bool,
        load_path: Option<std::path::PathBuf>,
    },
    /// An error occured while evaluating a value.
    ValueError(grease::Error),
    /// An error occured while loading a script.
    ScriptLoadError(super::ast::Error),
    /// A generic error message.
    GenericError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            InvalidIndex => write!(f, "type is not an array or map; cannot index"),
            MissingBinding(_v) => write!(f, "binding is not available in the current environment"),
            MissingArrayIndex(_i) => write!(f, "array index does not exist"),
            MissingMapIndex(_v) => write!(f, "map index does not exist"),
            NonIntegerIndex => write!(f, "positive integer index expected"),
            NonIndexableValue(_v) => write!(f, "value cannot be indexed"),
            NonCallableExpression(_v) => write!(f, "cannot pass arguments to non-callable value"),
            CannotMerge(s) => write!(f, "cannot merge: {}", s),
            PatternMapTooManyRest => write!(f, "map pattern may only have one merge subpattern"),
            PatternMapExtraKeys(_) => write!(f, "map pattern doesn't match all values in map"),
            PatternArrayRestUndecidable => write!(f, "array merge pattern is undecidable"),
            PatternMismatch(_) => write!(f, "value could not be matched to pattern"),
            PatternCommandTooManyNonPositional => write!(
                f,
                "command patterns can only have one non-positional pattern"
            ),
            UnexpectedNonPositionalArgument => write!(
                f,
                "the function does not accept this non-positional argument",
            ),
            MatchFailed(_) => write!(f, "no patterns matched the value"),
            NoNonPositionalArguments => {
                write!(f, "the function does not accept non-positional arguments")
            }
            ArgumentMismatch => write!(f, "argument mismatch in command"),
            LoadFailed {
                was_loading,
                was_workspace,
                load_path,
            } => match (was_loading,was_workspace,load_path) {
                (true,true,Some(v)) => write!(
                    f,
                    "'{}' resolved to a workspace that was in the process of loading (circular dependency avoided)", v.display()
                ),
                (true,false,Some(v)) => write!(
                    f,
                    "'{}' resolved to a path that was in the process of loading (circular dependency avoided)", v.display()),
                (true,true,None) => write!(f, "load resolved to a workspace that was in the process of loading (circular dependency avoided)"),
                (true,false,None) => write!(f, "load resolved to a path that was in the process of loading (circular dependency avoided)"),
                (false,_,Some(v)) => write!(f, "'{}' failed to load", v.display()),
                (false,true,None) => write!(f, "failed to load (no workspace found)"),
                (false,_,None) => write!(f, "failed to load")
            },
            ValueError(e) => write!(f, "{}", e),
            ScriptLoadError(e) => write!(f, "{}", e),
            GenericError(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ValueError(e) => Some(e.as_ref()),
            Error::ScriptLoadError(e) => Some(e),
            _ => None,
        }
    }
}

impl From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        std::io::Error::new(std::io::ErrorKind::Other, e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        e.to_string().into()
    }
}

impl From<grease::Error> for Error {
    fn from(e: grease::Error) -> Self {
        Error::ValueError(e)
    }
}

impl From<super::ast::Error> for Error {
    fn from(e: super::ast::Error) -> Self {
        Error::ScriptLoadError(e)
    }
}

impl From<&'_ str> for Error {
    fn from(s: &str) -> Self {
        Self::from(s.to_owned())
    }
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::GenericError(s)
    }
}

#[derive(Clone, Debug)]
struct PatternValues {
    pub literal: Value,
    pub binding: Value,
}

impl PatternValues {
    pub fn new(literal: Value, binding: Value) -> Self {
        PatternValues { literal, binding }
    }

    pub fn singular(v: Value) -> Self {
        Self::new(v.clone(), v)
    }
}

type PatCompiled = PatT<grease::value::Id, EvalResult>;

/// Get dependencies suitable for PatCompiled.
fn pattern_dependencies_comp(pat: &PatCompiled) -> grease::value::Dependencies {
    pattern_dependencies(
        pat,
        |id| depends![id],
        |e| match e {
            Ok(v) => depends![**v],
            Err(_) => depends![],
        },
    )
}

/// Get all bindings in a compiled pattern.
///
/// The returned tuple contains the binding key and a value appropriate to use as a dependency of
/// the binding position within the pattern.
fn pattern_bindings(pat: &PatCompiled) -> Vec<(Source<Value>, grease::value::Dependencies)> {
    use grease::value::Dependencies;
    let mut bindings = Vec::new();
    let mut remaining = vec![(pat, Dependencies::new())];
    while let Some((pat, deps)) = remaining.pop() {
        use Pattern::*;
        let src = pat.source();
        match pat.as_ref().unwrap() {
            Binding(Ok(sv)) => bindings.push((src.with(sv.clone().unwrap()), deps)),
            Array(pats) => {
                let mut i = 0;
                for p in pats {
                    match p.as_ref().unwrap() {
                        ArrayPattern::Item(p) => remaining.push((p, deps.clone() + depends![i])),
                        ArrayPattern::Rest(p) => remaining.push((p, deps.clone() + depends![i])),
                    }
                    i += 1;
                }
            }
            Map(pats) => {
                for p in pats {
                    match p.as_ref().unwrap() {
                        MapPattern::Item(k, p) => remaining.push((p, deps.clone() + depends![k])),
                        MapPattern::Rest(p) => remaining.push((
                            p,
                            deps.clone() + depends![ergo_runtime::namespace_id!(ergo::rest)],
                        )),
                    }
                }
            }
            _ => (),
        }
    }

    bindings
}

/// Get all bindings in a pattern.
fn pattern_bindings_basic<L, B: Clone>(pat: &PatT<L, B>) -> Vec<Source<B>> {
    let mut bindings: Vec<Source<B>> = Vec::new();
    let mut remaining = vec![pat];
    while let Some(pat) = remaining.pop() {
        use Pattern::*;
        match pat.as_ref().unwrap() {
            Binding(v) => bindings.push(pat.source().with(v.clone())),
            Array(pats) => {
                for p in pats {
                    match p.as_ref().unwrap() {
                        ArrayPattern::Item(p) => remaining.push(p),
                        ArrayPattern::Rest(p) => remaining.push(p),
                    }
                }
            }
            Map(pats) => {
                for p in pats {
                    match p.as_ref().unwrap() {
                        MapPattern::Item(_, p) => remaining.push(p),
                        MapPattern::Rest(p) => remaining.push(p),
                    }
                }
            }
            _ => (),
        }
    }

    bindings
}

fn pattern_is_nested<L, B>(pat: &PatT<L, B>) -> bool {
    use Pattern::*;
    match &**pat {
        Array(_) | Map(_) => true,
        _ => false,
    }
}

fn compile_array_pattern<'a>(
    ctx: &'a mut Runtime,
    pat: Source<ArrayPattern<Expr, Expr>>,
) -> BoxFuture<'a, grease::Result<Source<ArrayPattern<grease::value::Id, EvalResult>>>> {
    async move {
        pat.map_async(|p| async {
            match p {
                ArrayPattern::Item(p) => compile_pattern(ctx, p).await.map(ArrayPattern::Item),
                ArrayPattern::Rest(p) => compile_pattern(ctx, p).await.map(ArrayPattern::Rest),
            }
        })
        .await
        .transpose_ok()
    }
    .boxed()
}

fn compile_pattern<'a>(
    ctx: &'a mut Runtime,
    pat: Pat,
) -> BoxFuture<'a, grease::Result<PatCompiled>> {
    async move {
        use Pattern::*;
        pat.map_async(|p| async move {
            Ok(match p {
                Any => Any,
                Literal(e) => Literal(Rt(e).evaluate_ext(ctx, true).await?.id()),
                Binding(e) => Binding(Rt(e).evaluate_ext(ctx, true).await),
                Array(pats) => {
                    let mut pats_new = Vec::new();
                    for p in pats {
                        pats_new.push(compile_array_pattern(ctx, p).await?);
                    }
                    Array(pats_new)
                }
                Map(pats) => {
                    let mut pats_new = Vec::new();
                    for p in pats {
                        pats_new.push(
                            p.map_async(|p| async {
                                match p {
                                    MapPattern::Item(k, p) => {
                                        let k = Rt(k).evaluate_ext(ctx, true).await?.id();
                                        let p = compile_pattern(ctx, p).await?;
                                        Ok(MapPattern::Item(k, p))
                                    }
                                    MapPattern::Rest(p) => {
                                        compile_pattern(ctx, p).await.map(MapPattern::Rest)
                                    }
                                }
                            })
                            .await
                            .transpose_ok()?,
                        );
                    }
                    Map(pats_new)
                }
            })
        })
        .await
        .transpose_ok()
    }
    .boxed()
}

pub async fn apply_pattern(pat: PatCompiled, val: Option<Source<Value>>) -> EResult<ScriptEnv> {
    let mut ret = ScriptEnv::new();
    let mut errs = Vec::new();

    _apply_pattern(
        &mut ret,
        &mut errs,
        pat,
        val.map(|v| v.map(PatternValues::singular)),
    )
    .await;
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(grease::Error::aggregate(errs))
    }
}

fn _apply_pattern<'a>(
    env: &'a mut ScriptEnv,
    errs: &'a mut Vec<grease::Error>,
    pat: PatCompiled,
    val: Option<Source<PatternValues>>,
) -> BoxFuture<'a, ()> {
    async move {
        use Pattern::*;
        let (source, pat) = pat.take();
        let desc = std::mem::discriminant(&pat);
        match pat {
            Any => (),
            Literal(id) => {
                if let Some(val) = val {
                    if id != val.literal.id() {
                        errs.push(
                            source
                                .with(Error::PatternMismatch(val.unwrap().literal))
                                .into(),
                        );
                    }
                }
            }
            Binding(key) => match key {
                Err(e) => errs.push(source.with("within binding").context_for_error(e)),
                Ok(key) => {
                    env.insert(
                        key.unwrap(),
                        match val {
                            Some(v) => Ok(v.map(|v| v.binding)),
                            None => Err(grease::Error::aborted()),
                        }
                        .into(),
                    );
                }
            },
            Array(inner) => {
                let orig_val = val.clone();
                let vals: EResult<Vec<Option<Source<PatternValues>>>> = {
                    let val = match val {
                        Some(v) => {
                            let (vsource, v) = v.take();
                            let as_array = match_value!(v.binding => {
                                types::Array => |v| match v.await {
                                    Ok(v) => {
                                        let vsource = vsource.clone();
                                        v.owned()
                                            .0
                                            .into_iter()
                                            .map(move |v| vsource.clone().with(v))
                                    }
                                    Err(e) => {
                                        Err(vsource.with(Error::ValueError(e)))?
                                    }
                                },
                                => |v| {
                                    let err = source.clone().with(Error::PatternMismatch(v)).into();
                                    Err(
                                        vsource.with("value definition").context_for_error(err),
                                    )?
                                }
                            });
                            as_array.await
                        }
                        None => Err(grease::Error::aborted()),
                    };
                    val.map(|v| {
                        v.enumerate()
                            .map(|(i, v)| {
                                Some(v.map(|v| {
                                    PatternValues::new(
                                        v.clone(),
                                        v.set_dependencies(depends![
                                            orig_val.as_ref().unwrap().as_ref().unwrap().binding,
                                            desc,
                                            i
                                        ]),
                                    )
                                }))
                            })
                            .collect()
                    })
                };

                let vals = match vals {
                    Err(e) => {
                        errs.push(e);
                        std::iter::repeat(None).take(inner.len()).collect()
                    }
                    Ok(v) => v,
                };

                match pattern_array(&inner, &vals).await {
                    Ok(mut n_env) => {
                        env.append(&mut n_env);
                    }
                    Err(n_errs) => {
                        errs.push({
                            let mut err: grease::Error = match &orig_val {
                                None => source
                                    .clone()
                                    .with(Error::ValueError("pattern could not be matched".into()))
                                    .into(),
                                Some(orig_val) => orig_val
                                    .source()
                                    .with("value being matched")
                                    .context_for_error(
                                        source
                                            .clone()
                                            .with(Error::PatternMismatch(orig_val.binding.clone()))
                                            .into(),
                                    ),
                            };
                            for e in n_errs {
                                err = err.with_context(e);
                            }
                            err
                        });
                    }
                }
            }
            Map(inner) => {
                let mut val = match val {
                    Some(v) => {
                        let orig = v.clone();
                        let (vsource, v) = v.take();
                        let as_map = match_value!(v.binding => {
                            types::Map => |v| match v.await {
                                Ok(v) => Some((orig, v.owned().0)),
                                Err(e) => {
                                    errs.push(vsource.with(Error::ValueError(e)).into());
                                    None
                                }
                            },
                            => |v| {
                                let err = source.clone().with(Error::PatternMismatch(v)).into();
                                errs.push(vsource.with("value definition").context_for_error(err));
                                None
                            }
                        });
                        match as_map.await {
                            Err(e) => {
                                errs.push(e);
                                None
                            }
                            Ok(v) => v,
                        }
                    }
                    None => None,
                };
                let mut rest_pattern = None;
                let mut matched_keys = BTreeMap::new();
                for i in inner {
                    let (isource, i) = i.take();
                    let itemdesc = std::mem::discriminant(&i);
                    match i {
                        MapPattern::Item(key, pat) => {
                            dbg!(&grease::u128::U128::from(key));
                            let result = match val.as_mut() {
                                Some((orig_value, m)) => match dbg!(m).remove(dbg!(&key)) {
                                    Some(v) => {
                                        let keydep = depends![key];
                                        matched_keys.insert(key, isource.clone());
                                        Some(isource.with(PatternValues::new(
                                            v.clone(),
                                            v.set_dependencies(depends![
                                                orig_value.as_ref().binding,
                                                desc,
                                                itemdesc,
                                                keydep
                                            ]),
                                        )))
                                    }
                                    None => {
                                        let (vsource, v) = orig_value.clone().take();
                                        let mut err = source
                                            .clone()
                                            .with(Error::PatternMismatch(v.binding))
                                            .into();
                                        err = isource.with("missing key").context_for_error(err);
                                        if let Some(src) = matched_keys.get(&key) {
                                            err = src
                                                .clone()
                                                .with("key previously matched here")
                                                .context_for_error(err);
                                        }
                                        errs.push(
                                            vsource.with("value definition").context_for_error(err),
                                        );
                                        None
                                    }
                                },
                                None => None,
                            };
                            _apply_pattern(env, errs, pat, result).await;
                        }
                        MapPattern::Rest(pat) => {
                            if let Some((src, _)) = rest_pattern.replace((isource.clone(), pat)) {
                                errs.push(src.with("previous definition").context_for_error(
                                    isource.with(Error::PatternMapTooManyRest).into(),
                                ));
                            }
                        }
                    }
                }

                // Match rest pattern with remaining values
                if let Some((src, rest)) = rest_pattern {
                    _apply_pattern(
                        env,
                        errs,
                        rest,
                        val.map(move |(_, m)| {
                            src.with(PatternValues::singular(types::Map(m).into_value_no_doc()))
                        }),
                    )
                    .await
                } else {
                    match val {
                        Some((orig_value, m)) => {
                            if !m.is_empty() {
                                let (vsource, v) = orig_value.take();
                                errs.push(vsource.with("value definition").context_for_error(
                                    source.with(Error::PatternMapExtraKeys(v.binding)).into(),
                                ));
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
    }
    .boxed()
}

fn is_fixed_point<L, B>(pat: &Pattern<L, B>) -> bool {
    match pat {
        Pattern::Any | Pattern::Binding(_) => false,
        _ => true,
    }
}

async fn pattern_array(
    pats: &[Source<ArrayPattern<grease::value::Id, EvalResult>>],
    vals: &[Option<Source<PatternValues>>],
) -> Result<ScriptEnv, Vec<grease::Error>> {
    let mut ret = ScriptEnv::new();
    let mut errs = Vec::new();

    _pattern_array(&mut ret, &mut errs, pats, vals).await;
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(errs)
    }
}

fn _pattern_array<'a>(
    env: &'a mut ScriptEnv,
    errs: &'a mut Vec<grease::Error>,
    pats: &'a [Source<ArrayPattern<grease::value::Id, EvalResult>>],
    vals: &'a [Option<Source<PatternValues>>],
) -> BoxFuture<'a, ()> {
    async move {
        let mut vali = 0;
        let mut pati = 0;
        loop {
            let pat = if let Some(i) = pats.get(pati) {
                pati += 1;
                i
            } else {
                if vali != vals.len() {
                    errs.push(
                        pats.into_source()
                            .with(Error::ValueError(
                                "not enough patterns to match with value items".into(),
                            ))
                            .into(),
                    );
                }
                break;
            };
            let (psource, pat) = pat.as_ref().take();
            match pat {
                ArrayPattern::Item(p) => {
                    if let Some(v) = vals.get(vali) {
                        vali += 1;
                        _apply_pattern(env, errs, p.clone(), v.clone()).await;
                    } else {
                        errs.push(
                            psource
                                .with(Error::ValueError("no nested value matches pattern".into()))
                                .into(),
                        );
                        break;
                    }
                }
                ArrayPattern::Rest(p) => {
                    let rest_end =
                        pattern_array_rest(env, errs, &pats[pati..], &vals[vali..]).await;
                    _apply_pattern(
                        env,
                        errs,
                        p.clone(),
                        rest_end.map(|vs| {
                            vs.into_source().map(|vs| {
                                let mut bindings = rvec![];
                                let mut literals = rvec![];
                                for p in vs.into_iter().map(Source::unwrap) {
                                    bindings.push(p.binding);
                                    literals.push(p.literal);
                                }
                                PatternValues::new(
                                    types::Array(literals).into_value(),
                                    types::Array(bindings).into_value(),
                                )
                            })
                        }),
                    )
                    .await;
                    break;
                }
            }
        }
    }
    .boxed()
}

fn pattern_array_rest<'a>(
    env: &'a mut ScriptEnv,
    errs: &'a mut Vec<grease::Error>,
    pats: &'a [Source<ArrayPattern<grease::value::Id, EvalResult>>],
    vals: &'a [Option<Source<PatternValues>>],
) -> BoxFuture<'a, Option<Vec<Source<PatternValues>>>> {
    async move {
        let pat = if let Some(i) = pats.first() {
            i
        } else {
            return vals.into_iter().cloned().collect();
        };
        let (psource, pat) = pat.as_ref().take();
        match pat {
            ArrayPattern::Item(p) => {
                if is_fixed_point(&*p) {
                    // If we have a fixed point pattern, try to match at different locations
                    let mut vali = 0;
                    while vali < vals.len() {
                        match pattern_array(&pats, &vals[vali..]).await {
                            Ok(mut n_env) => {
                                env.append(&mut n_env);
                                return vals[..vali].into_iter().cloned().collect();
                            }
                            Err(_) => (),
                        }
                        vali += 1;
                    }
                    errs.push(
                        psource
                            .with(Error::ValueError(
                                "no value matched the given pattern".into(),
                            ))
                            .into(),
                    );
                    None
                } else {
                    // Go forward to try to find a fixed point
                    match pattern_array_rest(env, errs, &pats[1..], &vals[..]).await {
                        None => None,
                        Some(values) => {
                            if values.is_empty() {
                                errs.push(
                                    psource
                                        .with(Error::ValueError(
                                            "no value to match with pattern".into(),
                                        ))
                                        .into(),
                                );
                                None
                            } else {
                                let mut remaining = values.len() - 1;
                                let mut ret = Vec::with_capacity(remaining);
                                let mut val = None;
                                for v in values {
                                    if remaining > 0 {
                                        ret.push(v);
                                        remaining -= 1;
                                    } else {
                                        val = Some(v);
                                    }
                                }
                                _apply_pattern(env, errs, p.clone(), Some(val.unwrap())).await;
                                Some(ret)
                            }
                        }
                    }
                }
            }
            ArrayPattern::Rest(_) => {
                errs.push(psource.with(Error::PatternArrayRestUndecidable).into());
                None
            }
        }
    }
    .boxed()
}

pub async fn apply_command_pattern(
    pat: CmdPatT<grease::value::Id, EvalResult>,
    mut args: FunctionArguments,
) -> Result<ScriptEnv, Vec<grease::Error>> {
    let pat = pat.unwrap();
    let kw = std::mem::take(&mut args.non_positional);
    let vals: Vec<_> = args.map(|v| Some(v.map(PatternValues::singular))).collect();

    // Partition non-positional/positional argument patterns
    let mut pos_args = Vec::new();
    let mut non_pos_args = Vec::new();
    for p in pat {
        let (psource, p) = p.take();
        if let ArrayPattern::Rest(p) = p {
            if let Pattern::Map(_) = &*p {
                non_pos_args.push(p);
            } else {
                pos_args.push(psource.with(ArrayPattern::Rest(p)));
            }
        } else {
            pos_args.push(psource.with(p));
        }
    }

    // If more than one non-positional pattern, error
    if non_pos_args.len() > 1 {
        let vals = non_pos_args.get(1..).unwrap();
        let mut err: grease::Error = non_pos_args
            .as_slice()
            .into_source()
            .with(Error::PatternCommandTooManyNonPositional)
            .into();
        for a in vals {
            err = a
                .source()
                .with("extra non-positional argument")
                .context_for_error(err);
        }
        Err(vec![err])
    } else {
        // Get non-positional pattern bindings
        let non_pos_bindings = if let Some(non_pos_arg) = non_pos_args.into_iter().next() {
            apply_pattern(
                non_pos_arg,
                Some(kw.into_source().map(|kw| {
                    types::Map(
                        kw.into_iter()
                            .map(|(k, v)| (k.unwrap(), v.unwrap()))
                            .collect(),
                    )
                    .into_value_no_doc()
                })),
            )
            .await
        } else if !kw.is_empty() {
            Err(kw
                .into_iter()
                .map(|(k, v)| {
                    (k, v)
                        .into_source()
                        .with(Error::UnexpectedNonPositionalArgument)
                        .into()
                })
                .collect())
        } else {
            Ok(ScriptEnv::default())
        };
        let non_pos_bindings = non_pos_bindings.map_err(|e| vec![e]);
        // Merge with positional pattern bindings, if applicable
        let pos_bindings = pattern_array(&pos_args, &vals).await;
        match (non_pos_bindings, pos_bindings) {
            (Ok(mut a), Ok(mut b)) => {
                a.append(&mut b);
                Ok(a)
            }
            (Err(mut a), Err(mut b)) => {
                a.append(&mut b);
                Err(a)
            }
            (a, b) => a.and(b),
        }
    }
}

fn lookup(ctx: &Runtime, k: Value) -> EResult<Value> {
    trace!("looking up '{}' in environment", k.id());
    match ctx.env_get(&k) {
        None => Err(Error::MissingBinding(k).into()),
        Some(value) => {
            let value = value?;
            trace!(
                "found match in environment for '{}': {}",
                k.id(),
                value.id()
            );
            Ok(value.clone().unwrap())
        }
    }
}

/// Apply the value to the given arguments.
///
/// If `env_lookup` is true and `v` is a `String`, it will be looked up in the environment.
pub fn apply_value(
    ctx: &mut Runtime,
    v: Source<Value>,
    args: UncheckedFunctionArguments,
    env_lookup: bool,
) -> BoxFuture<EvalResult> {
    async move {
        let v_source = v.source();

        let vctx = ctx.clone();

        v.map_async(|v| async {
            let v = if env_lookup {
                v.type_and_then_value(
                    move |tp, v| async move {
                        match_value!(v, typed &tp => {
                            // Lookup string in environment, and apply result to remaining arguments
                            types::String => |k| lookup(&vctx, k.into()),
                            => |v| Ok(v)
                        })
                    },
                    depends![{ergo_runtime::namespace_id!(ergo::env_string_get)}],
                )
                .await?
            } else {
                v
            };

            let mut ctx = ctx.clone();

            let deps = depends![{ergo_runtime::namespace_id!(ergo::apply)}, ^&args];

            // Eagerly evaluate application
            v.and_then_value(move |v| async move {
                match_value!(v => {
                    types::Function => |val| {
                        let mut fcallctx = FunctionCall::new(&mut ctx, args.into(), v_source);
                        let f = val.await?;
                        let f = f.as_ref();
                        let ret = f.call(&mut fcallctx).await;
                        if ret.is_err() {
                            fcallctx.args.clear();
                        }
                        // TODO is it appropriate to lose this source info?
                        ret.map(Source::unwrap)?
                    }
                    types::Array => |val| {
                        let mut fcallctx = FunctionCall::new(&mut ctx, args.into(), v_source);
                        let ind = fcallctx.args.next().ok_or("no index provided")?;
                        fcallctx.unused_arguments()?;

                        let ind = fcallctx.source_value_as::<types::String>(ind).await?.await.transpose_ok()?;
                        ind.map_async(|index| async move { match usize::from_str(index.as_ref()) {
                            Err(_) => Err(fcallctx.call_site.with(Error::NonIntegerIndex).into()),
                            Ok(ind) => val.await.and_then(|v| v.0.get(ind).cloned().ok_or_else(|| Error::MissingArrayIndex(ind).into()))
                        }
                        }).await.transpose_err_with_context("while indexing array")?
                    },
                    types::Map => |val| {
                        let mut fcallctx = FunctionCall::new(&mut ctx, args.into(), v_source);
                        let ind = fcallctx.args.next().ok_or("no key provided")?;
                        fcallctx.unused_arguments()?;

                        ind.map_async(|index| async move {
                                val.await.and_then(|v| v.0.get(&index).cloned().ok_or_else(|| Error::MissingMapIndex(index).into()))
                        }).await.transpose_err_with_context("while indexing map")?
                    },
                    => |v| {
                        Err(Error::NonCallableExpression(v))?
                    }
                }).await
            }, deps)
            .await
        })
        .await
        .map(|v| v.map_err(|e| e.error()))
        .transpose_with_context("while applying value")
    }
    .boxed()
}

/// Compiles environment accesses into the expression.
fn compile_env_into_expr(ctx: &mut Runtime, local_env: Vec<Expr>, expr: Expr) -> EResult<Expr> {
    type LocalEnv = BTreeSet<Expr>;

    struct Ctx<'a> {
        ctx: &'a mut Runtime,
        local_env: Vec<LocalEnv>,
    }

    impl<'a> Ctx<'a> {
        pub fn new(ctx: &'a mut Runtime) -> Self {
            Ctx {
                ctx,
                local_env: vec![Default::default()],
            }
        }

        /// Get the current scoped environment.
        fn env_current(&mut self) -> &mut LocalEnv {
            self.local_env
                .last_mut()
                .expect("invalid env access; no environment stack")
        }

        /// Insert a binding into the current scoped environment.
        pub fn env_insert(&mut self, e: Expr) {
            self.env_current().insert(e);
        }

        /// Remove a binding from the current scoped environment.
        pub fn env_remove(&mut self, e: &Expr) {
            self.env_current().remove(e);
        }

        /// Get a binding from the current environment.
        pub fn env_get(
            &'a self,
            e: &'a Expr,
        ) -> EResult<Option<std::result::Result<&'a Source<Value>, &'a grease::Error>>> {
            if self.local_env.iter().rev().any(|m| m.contains(e)) {
                Ok(None)
            } else if let Expression::Compiled(v) = e.as_ref().unwrap() {
                match &v.value {
                    Ok(v) => self
                        .ctx
                        .env_get(v.as_ref().unwrap())
                        .map(|v| Ok(Some(v)))
                        .unwrap_or_else(|| {
                            Err(Error::MissingBinding(v.as_ref().unwrap().clone()).into())
                        }),
                    Err(e) => Ok(Some(Err(e))),
                }
            } else {
                Err(Error::MissingBinding(types::Unit.into_value().into()).into())
            }
        }

        /// Call the given function in a new, scoped environment.
        pub fn env_scoped<F, R>(&mut self, f: F) -> R
        where
            F: FnOnce(&mut Self) -> R,
        {
            self.local_env.push(Default::default());
            let r = f(self);
            self.local_env.pop();
            r
        }
    }

    fn do_expr(ctx: &mut Ctx, expr: Expr) -> EResult<Expr> {
        do_expr_ext(ctx, expr, false)
    }

    fn do_expr_ext(ctx: &mut Ctx, expr: Expr, ignore_string_env_match: bool) -> EResult<Expr> {
        use Expression::*;
        let src = expr.source();
        expr.map(move |e| -> EResult<_> {
            Ok(match e {
                Empty => Compiled(Ok(src.with(types::Unit.into_value().into())).into()),
                Expression::String(s) => {
                    let s: Value = types::String::from(s).into();
                    let ret = Compiled(Ok(src.clone().with(s)).into());

                    if let (true, Ok(Some(_))) = (
                        ctx.ctx.lint && !ignore_string_env_match,
                        ctx.env_get(&src.clone().with(ret.clone())),
                    ) {
                        ctx.ctx.log.warn(format!(
                            "{}",
                            src.with(
                                "string literal matches a binding in scope; did you mean to index?"
                            )
                        ));
                    }
                    ret
                }
                Array(es) => Array(
                    es.into_iter()
                        .map(|mut me| {
                            me.expr = do_expr(ctx, me.expr.clone())?;
                            Ok(me)
                        })
                        .collect_result()?,
                ),
                Set(pat, e) => {
                    let pat = do_pat(ctx, *pat)?;
                    let expr = do_expr(ctx, *e)?;
                    for b in pattern_bindings_basic(&pat) {
                        ctx.env_insert(b.unwrap());
                    }
                    Set(pat.into(), expr.into())
                }
                Unset(var) => {
                    let var = do_expr(ctx, *var)?;
                    ctx.env_remove(&var);
                    Unset(var.into())
                }
                Get(var) => {
                    let var = do_expr_ext(ctx, *var, true)?;

                    let new_var = ctx.env_get(&var)?;

                    match new_var {
                        None => Get(var.into()),
                        Some(v) => Compiled(v.map(|o| o.clone()).map_err(|e| e.clone()).into()),
                    }
                }
                Command(cmd, args) => {
                    let cmd = {
                        let new_cmd = do_expr_ext(ctx, *cmd, true)?;
                        if let Compiled(CompiledExpression { value: Ok(v), .. }) =
                            new_cmd.as_ref().unwrap()
                        {
                            if v.grease_type_immediate() == Some(&types::String::grease_type()) {
                                if let Some(value) = ctx.env_get(&new_cmd)? {
                                    new_cmd.source().with(Compiled(CompiledExpression::new(
                                        value.map(|v| v.clone()).map_err(|e| e.clone()),
                                        true,
                                    )))
                                } else {
                                    new_cmd
                                }
                            } else {
                                new_cmd
                            }
                        } else {
                            new_cmd
                        }
                    };
                    let args = args
                        .into_iter()
                        .map(|mut me| {
                            me.expr = do_expr(ctx, me.expr.clone())?;
                            Ok(me)
                        })
                        .collect_result()?;
                    Command(cmd.into(), args)
                }
                Block(es) => ctx
                    .env_scoped(|ctx| {
                        Ok(Block(
                            es.into_iter()
                                .map(|mut me| {
                                    me.expr = do_expr(ctx, me.expr.clone())?;
                                    Ok(me)
                                })
                                .collect_result()?,
                        ))
                    })
                    .map_err(|e: grease::Error| e)?,
                Function(pat, e) => {
                    let pat = pat
                        .map(|ps| {
                            ps.into_iter()
                                .map(|sap| {
                                    sap.map(|ap| {
                                        Ok(match ap {
                                            ArrayPattern::Item(p) => {
                                                ArrayPattern::Item(do_pat(ctx, p)?)
                                            }
                                            ArrayPattern::Rest(p) => {
                                                ArrayPattern::Rest(do_pat(ctx, p)?)
                                            }
                                        })
                                    })
                                    .transpose_ok()
                                })
                                .collect_result::<Vec<_>>()
                        })
                        .transpose_ok()?;
                    // Aggregate all bindings to be added to the local env.
                    let mut binds = Vec::new();
                    for p in pat.iter() {
                        match p.as_ref().unwrap() {
                            ArrayPattern::Item(p) => binds.extend(pattern_bindings_basic(p)),
                            ArrayPattern::Rest(p) => binds.extend(pattern_bindings_basic(p)),
                        }
                    }
                    let e = ctx.env_scoped(move |ctx| {
                        for b in binds {
                            ctx.env_insert(b.unwrap());
                        }
                        ctx.env_scoped(move |ctx| do_expr(ctx, *e))
                    })?;
                    Function(pat, e.into())
                }
                Match(val, pats) => {
                    let val = do_expr(ctx, *val)?;
                    let pats = pats
                        .into_iter()
                        .map(|(p, e)| {
                            let p = do_pat(ctx, p)?;
                            let binds = pattern_bindings_basic(&p);
                            let e = ctx.env_scoped(move |ctx| {
                                for b in binds {
                                    ctx.env_insert(b.unwrap());
                                }
                                ctx.env_scoped(move |ctx| do_expr(ctx, e))
                            })?;
                            Ok((p, e))
                        })
                        .collect_result()?;
                    Match(val.into(), pats)
                }
                If(cond, if_true, if_false) => {
                    let cond = do_expr(ctx, *cond)?;
                    let if_true = do_expr(ctx, *if_true)?;
                    let if_false = if_false.map(|e| do_expr(ctx, *e)).transpose()?;
                    If(cond.into(), if_true.into(), if_false.map(Box::from))
                }
                Force(v) => Force(do_expr(ctx, *v)?.into()),
                DocComment(s, v) => {
                    let mut v = do_expr(ctx, *v)?;
                    if let Compiled(CompiledExpression {
                        value: Ok(value), ..
                    }) = &mut *v
                    {
                        value.set_metadata(&metadata::Doc, types::String::from(s).into());
                        v.unwrap()
                    } else {
                        DocComment(s, v.into())
                    }
                }
                Compiled(v) => Compiled(v),
            })
        })
        .transpose_with_context("in expression")
    }

    fn do_pat(ctx: &mut Ctx, pat: Pat) -> EResult<Pat> {
        use Pattern::*;
        pat.map(|p| -> EResult<_> {
            Ok(match p {
                Any => Any,
                Literal(e) => Literal(do_expr_ext(ctx, e, true)?),
                Binding(e) => Binding(do_expr_ext(ctx, e, true)?),
                Array(ps) => Array(
                    ps.into_iter()
                        .map(|sap| {
                            sap.map(|ap| {
                                Ok(match ap {
                                    ArrayPattern::Item(p) => ArrayPattern::Item(do_pat(ctx, p)?),
                                    ArrayPattern::Rest(p) => ArrayPattern::Rest(do_pat(ctx, p)?),
                                })
                            })
                            .transpose_ok()
                        })
                        .collect_result()?,
                ),
                Map(ps) => Map(ps
                    .into_iter()
                    .map(|smp| {
                        smp.map(|mp| {
                            Ok(match mp {
                                MapPattern::Item(k, p) => MapPattern::Item(k, do_pat(ctx, p)?),
                                MapPattern::Rest(p) => MapPattern::Rest(do_pat(ctx, p)?),
                            })
                        })
                        .transpose_ok()
                    })
                    .collect_result()?),
            })
        })
        .transpose_with_context("in pattern")
    }

    let mut ctx = Ctx::new(ctx);
    for e in local_env {
        ctx.env_insert(e);
    }
    ctx.env_scoped(move |ctx| do_expr(ctx, expr))
}

pub struct Rt<T>(pub T);

impl<T> std::ops::Deref for Rt<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> std::ops::DerefMut for Rt<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Rt<Expression> {
    pub fn evaluate<'a>(
        self,
        ctx: &'a mut Runtime,
        string_env_match: Option<Source<()>>,
    ) -> BoxFuture<'a, EResult<Value>> {
        async move {

        use Expression::*;
        match self.0 {
            Empty => Ok(types::Unit.into_value().into()),
            Expression::String(s) => {
                let s: Value = types::String::from(s).into();
                if let (Some(src), Some(_)) = (string_env_match, ctx.env_get(&s)) {
                    ctx.log.warn(format!("{}", src.with("string literal is in the environment; did you mean to index?")));
                }
                Ok(s)
            },
            Array(es) => {
                let all_vals = {
                    let mut results = Vec::new();
                    for e in es {
                        results.push(Rt(e).evaluate(ctx).await)
                    }
                    results.collect_result::<Vec<_>>()?
                };

                enum ArrayVal {
                    Single(Value),
                    Merge(TypedValue<types::Array>)
                }

                impl From<&ArrayVal> for grease::value::Dependency {
                    fn from(av: &ArrayVal) -> Self {
                        match av {
                            ArrayVal::Single(v) => grease::value::Dependency::Value(v.clone()),
                            ArrayVal::Merge(v) => grease::value::Dependency::Value(v.clone().into()),
                        }
                    }
                }

                let mut vals = Vec::new();
                let mut errs = Vec::new();
                let mut has_merge = false;
                for v in all_vals {
                    let (_merge_source, (merge, val)) = v.take();
                    if merge {
                        match ctx.source_value_as::<types::Array>(val).await {
                            Err(e) => errs.push(e),
                            Ok(val) => {
                                has_merge = true;
                                vals.push(ArrayVal::Merge(val.unwrap()))
                            }
                        }
                    } else {
                        vals.push(ArrayVal::Single(val.unwrap()));
                    }
                }

                if !errs.is_empty() {
                    Err(grease::Error::aggregate(errs))
                } else if !has_merge {
                    Ok(types::Array(vals.into_iter()
                            .map(|v| match v { ArrayVal::Single(v) => v, _ => panic!("array logic error") })
                            .collect()).into_value())
                } else {
                    Ok(make_value!([depends![^@vals]] {
                        let mut ret = Vec::new();
                        let mut errs = Vec::new();
                        for v in vals {
                            match v {
                                ArrayVal::Single(v) => ret.push(v),
                                ArrayVal::Merge(v) => match v.await {
                                    Err(e) => errs.push(e),
                                    Ok(v) => ret.extend(v.owned().0),
                                }
                            }
                        }

                        if !errs.is_empty() {
                            Err(grease::Error::aggregate(errs))
                        } else {
                            Ok(types::Array(ret.into()))
                        }
                    }).into())
                }
            }
            Set(pat, e) => {
                let e_source = e.source();
                let data = Rt(*e).evaluate(ctx).await;
                let (mut ret, v) = match data {
                    Err(e) => (Err(e), None),
                    Ok(v) => (Ok(ScriptEnvIntoMap.into_value().into()), Some(v)),
                };
                let pat_compiled = compile_pattern(ctx, *pat).await?;
                if pattern_is_nested(&pat_compiled) {
                    // Nested patterns should have all bindings evaluated later to avoid
                    // immediately evaluating any values to check for array/map.
                    let bindings = pattern_bindings(&pat_compiled);
                    let v = v.unwrap_or(e_source.with(make_value!({ let ret: Result<(),_> = Err(grease::Error::aborted()); ret }).into()));
                    let deps = depends![^pattern_dependencies_comp(&pat_compiled), *v];
                    let pat_result = apply_pattern(pat_compiled, Some(v)).shared();
                    ctx.env_extend(bindings.into_iter().map(|(key, d)| {
                        let (source, k) = key.take();
                        let pat_result = pat_result.clone();
                        let deps = deps.clone() + d;
                        (k.clone(), RResult::ROk(source.with(Value::dyn_new(async move {
                            let bindings = pat_result.await?;
                            Ok(bindings.get(&k).expect("internal pattern matching assertion failed")
                                .clone()
                                .into_result()?
                                .unwrap()
                                .into_any_value())
                        }, deps))))
                    }));
                    // Add pat_result as something forced by the return value so that if the user forces
                    // the return value it will force pattern matching.
                    ret = match ret {
                        Ok(_) => Ok(make_value!({
                                pat_result.await?;
                                Ok(ScriptEnvIntoMap)
                            }).into()),
                        otherwise => otherwise
                    };
                } else {
                    ctx.env_extend(apply_pattern(pat_compiled, v).await?);
                }
                ret
            }
            Unset(var) => {
                let var = Rt(*var).evaluate(ctx).await?.unwrap();
                ctx.env_remove(&var);
                Ok(ScriptEnvIntoMap.into_value().into())
            }
            Get(var) => {
                let var = Rt(*var).evaluate_ext(ctx, true).await?;

                // Lookup in environment
                let (var_source, var) = var.take();
                lookup(ctx, var).map_err(|e| var_source.with(e).into_grease_error())
            }
            Command(cmd, args) => {
                let cmd_lookup = !cmd.is_compiled_from_env();
                let f = Rt(*cmd).evaluate_ext(ctx, true).await?;
                let args = {
                    let mut results = Vec::new();
                    for arg in args.iter() {
                        results.push(Rt(arg.clone()).evaluate(ctx).await);
                    }
                    results.collect_result::<Vec<_>>()?
                };
                let mut errs = Vec::new();
                let mut vals = Vec::new();
                let mut kw_vals = BTreeMap::new();
                for a in args {
                    let (_merge_source, (merge, val)) = a.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match_value!(val => {
                            types::Array => |val| {
                                vals.extend(val.await?.owned().0.into_iter().map(|v| val_source.clone().with(v)));
                            },
                            types::Map => |val| {
                                kw_vals.extend(val.await?.owned().0.into_iter().map(|(k,v)| (val_source.clone().with(k), val_source.clone().with(v))));
                            },
                            => |_| {
                                errs.push(
                                    val_source.with(Error::CannotMerge("non-array/map value".into())).into(),
                                );
                            }
                        }).await?;
                    } else {
                        vals.push(val);
                    }
                }

                if !errs.is_empty() {
                    return Err(grease::Error::aggregate(errs));
                }

                apply_value(
                    ctx,
                    f,
                    FunctionArguments::new(vals, kw_vals).unchecked(),
                    cmd_lookup,
                )
                .await
                .map(Source::unwrap) // TODO: use the source?
            }
            Block(_) => panic!("Block expression must be evaluated at a higher level"),
            Function(pat, e) => {
                // Pre-compile patterns
                let (pat_source, pat) = pat.take();
                let mut pats = Vec::new();
                let mut pat_deps = grease::value::Dependencies::default();
                let mut pat_bindings = Vec::new();
                for p in pat {
                    let p_compiled = compile_array_pattern(ctx, p).await?;
                    pat_deps += depends![std::mem::discriminant(&p_compiled)];
                    match p_compiled.as_ref().unwrap() {
                        ArrayPattern::Item(p) => {
                            pat_deps += pattern_dependencies_comp(p);
                            pat_bindings.extend(pattern_bindings_basic(p).into_iter().map(|v| v.map(|v| Expression::Compiled(v.into()))));
                        },
                        ArrayPattern::Rest(p) => {
                            pat_deps += pattern_dependencies_comp(p);
                            pat_bindings.extend(pattern_bindings_basic(p).into_iter().map(|v| v.map(|v| Expression::Compiled(v.into()))));
                        }
                    }
                    pats.push(p_compiled);
                }
                let pat = pat_source.with(pats);

                let e = compile_env_into_expr(ctx, pat_bindings, *e)?;

                let deps = depends![^pat_deps, ^expr_dependencies(&e)];
                let ctx = ctx.empty();
                Ok(types::Function::new(move |fctx| {
                    let e = e.clone();
                    let pat = pat.clone();
                    let mut ctx = ctx.clone();
                    async move {
                        let src = e.source();

                        let args = std::mem::take(&mut fctx.args);

                        match apply_command_pattern(pat.clone(), args).await {
                            Ok(bindings) => {
                                ctx.env_scoped(bindings, |ctx| {
                                    Rt(e).evaluate(ctx).boxed()
                                })
                                .await
                                .0
                            }
                            Err(errs) => Err(grease::Error::from(
                                src.with(Error::ArgumentMismatch),
                            )
                            .with_context(grease::Error::aggregate(errs))),
                        }
                    }
                    .boxed()
                }, deps, None)
                .into())
            }
            Match(val, pats) => {
                let val = Rt(*val).evaluate(ctx).await?;

                // Pre-compile patterns and expressions
                let mut ps = Vec::new();
                let mut pat_deps = depends![];
                for (p,e) in pats {
                    let p = compile_pattern(ctx, p).await?;
                    pat_deps += pattern_dependencies_comp(&p);
                    let pat_bindings = pattern_bindings_basic(&p).into_iter().map(|v| v.map(|v| Expression::Compiled(v.into()))).collect();
                    let e = compile_env_into_expr(ctx, pat_bindings, e)?;
                    pat_deps += expr_dependencies(&e);
                    ps.push((p,e));
                }
                let pats = ps;

                let deps = depends![*val, ^pat_deps];
                let mut ctx = ctx.empty();
                Ok(Value::dyn_new(async move {
                    for (p, e) in pats {
                        if let Ok(bindings) = apply_pattern(p, Some(val.clone())).await {
                            return Ok(ctx
                                .env_scoped(bindings, |ctx| Rt(e).evaluate(ctx).boxed())
                                .await
                                .0
                                .map(Source::unwrap)?
                                .into_any_value());
                        }
                    }

                    Err(val.source().with(Error::MatchFailed(val.unwrap())).into())
                }, deps))
            }
            If(cond, if_true, if_false) => {
                let cond = Rt(*cond).evaluate(ctx).await?;
                let if_true = compile_env_into_expr(ctx, Default::default(), *if_true)?;
                let if_false = if_false.map(|v| compile_env_into_expr(ctx, Default::default(), *v)).transpose()?;

                let to_sourced = ctx.into_sourced::<bool>(cond);
                let cond = to_sourced.await?.unwrap();

                let mut deps = depends![cond, ^expr_dependencies(&if_true)];

                if let Some(v) = &if_false {
                    deps += expr_dependencies(v);
                }

                let mut ctx = ctx.empty();
                Ok(Value::dyn_new(async move {
                    let c = cond.await?;
                    Ok(if *c.as_ref() {
                        Rt(if_true).evaluate(&mut ctx).await?.unwrap().into_any_value()
                    } else {
                        match if_false {
                            Some(e) => Rt(e).evaluate(&mut ctx).await?.unwrap().into_any_value(),
                            None => types::Unit.into_value().into_any_value()
                        }
                    })
                }, deps))
            }
            Force(val) => {
                let val = Rt(*val).evaluate(ctx).await?;

                val.map_async(|val| async {
                    // If the value is a dynamic value, get the inner value.
                    // Otherwise return the value by its (shallow) content.
                    match val.dyn_value().await? {
                        Ok(inner) => Ok(inner),
                        Err(val) => ctx.value_by_content(val, false).await
                    }
                }).await.transpose_err_with_context("in forced value")
            }
            DocComment(s, val) => {
                let mut val = Rt(*val).evaluate(ctx).await?.unwrap();
                val.set_metadata(&metadata::Doc, types::String::from(s).into());
                Ok(val)
            }
            Compiled(v) => v.value.map(Source::unwrap)
        }
    }.boxed()
    }
}

impl Rt<Source<Expression>> {
    pub async fn evaluate(self, ctx: &mut Runtime) -> EvalResult {
        self.evaluate_ext(ctx, false).await
    }

    pub async fn evaluate_ext(
        self,
        ctx: &mut Runtime,
        ignore_string_env_match: bool,
    ) -> EvalResult {
        let (source, expr) = self.0.take();
        if let Expression::Block(es) = expr {
            Rt(source.with(es)).evaluate(ctx).await
        } else {
            Rt(expr)
                .evaluate(
                    ctx,
                    if !ctx.lint || ignore_string_env_match {
                        None
                    } else {
                        Some(source.clone())
                    },
                )
                .await
                .map(|v| {
                    source.clone().with(
                        source
                            .with("while evaluating value returned by this expression")
                            .imbue_error_context(v),
                    )
                })
        }
    }
}

impl Rt<Source<MergeExpression>> {
    pub async fn evaluate(self, ctx: &mut Runtime) -> EResult<Source<(bool, Source<Value>)>> {
        let (source, val) = self.0.take();
        let merge = val.merge;
        Rt(val.expr)
            .evaluate(ctx)
            .await
            .map(move |e| source.with((merge, e)))
    }
}

impl Rt<Source<Vec<Source<MergeExpression>>>> {
    pub fn evaluate<'a>(self, ctx: &'a mut Runtime) -> BoxFuture<'a, EvalResult> {
        async move {
            let (self_source, this) = self.0.take();

            // Evaluate in a new scope
            let (val, scope) = ctx
                .env_scoped(Default::default(), |ctx| {
                    let self_source = self_source.clone();
                    async move {
                        let mut results = Vec::new();
                        for e in this {
                            results.push(match Rt(e).evaluate(ctx).await {
                                Err(e) => Err(e),
                                Ok(merge) => {
                                    let (merge_source, (merge, val)) = merge.take();
                                    if merge {
                                        // TODO make block map merge non-eagerly evaluated
                                        let (val_source, val) =
                                            ctx.source_value_as::<types::Map>(val).await?.take();
                                        for (k, v) in val.await?.owned().0 {
                                            ctx.env_insert(
                                                k.into(),
                                                Ok(val_source.clone().with(v)),
                                            );
                                        }
                                        Ok(merge_source.with(ScriptEnvIntoMap.into_value()))
                                    } else {
                                        Ok(val)
                                    }
                                }
                            });
                        }
                        results
                            .collect_result::<Vec<_>>()?
                            .into_iter()
                            .last()
                            .map(Ok)
                            .unwrap_or(Ok(self_source.with(ScriptEnvIntoMap.into_value())))
                    }
                    .boxed()
                })
                .await;

            // Possibly return the env scope as a map
            let env_map = scope
                .into_iter()
                .map(|(k, v)| v.map(move |v| (k, v.unwrap())).into_result())
                .collect_result::<BstMap<_, _>>();

            // Return result based on final value.
            match val {
                Err(e) => Err(e),
                Ok(val) => {
                    let (source, val) = val.take();
                    match val.grease_type_immediate() {
                        Some(tp) if tp == &ScriptEnvIntoMap::grease_type() => {
                            env_map.map(|ret| self_source.with(types::Map(ret).into_value(ctx)))
                        }
                        _ => Ok(source.with(val)),
                    }
                }
            }
        }
        .boxed()
    }
}
