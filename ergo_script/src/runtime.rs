//! Script runtime definitions.
//!
//! The runtime is responsible for evaluating AST expressions, producing values or errors.
//! Importantly, it tracks source locations for values and errors so that when an error occurs,
//! useful error information can be provided.

use super::ast::{Expr, Expression};
use crate::constants::{
    DIR_NAME, EXTENSION, PLUGIN_ENTRY, PRELUDE_ARG, WORKSPACE_FALLBACK_ARG, WORKSPACE_NAME,
};
use abi_stable::std_types::{ROption, RResult};
use ergo_runtime::error::BindError;
use ergo_runtime::source::{FileSource, Source};
use ergo_runtime::Result as EResult;
use ergo_runtime::{
    metadata, traits, types, Arguments, ContextExt, EvalResult, ResultIterator, Runtime, ScriptEnv,
    UncheckedArguments,
};
use futures::future::{BoxFuture, FutureExt};
use grease::{
    bst::BstMap,
    depends, match_value,
    path::PathBuf,
    types::GreaseType,
    value::{Errored, IntoValue, TypedValue, Value},
};
use libloading as dl;
use log::{debug, trace};
use std::collections::BTreeMap;
use std::fmt;
use std::path;

/// Script type for bind expressions.
#[derive(Clone, Copy, Debug, GreaseType, Hash)]
pub struct BindExpr;

impl From<BindExpr> for TypedValue<BindExpr> {
    fn from(v: BindExpr) -> Self {
        TypedValue::constant(v)
    }
}

grease::grease_traits_fn! {
    ergo_runtime::grease_type_name!(traits, BindExpr, "bind expression");
    ergo_runtime::traits::ValueByContent::add_impl::<BindExpr>(traits);
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

/// Load and execute a script given the function call arguments.
pub fn load_script<'a>(ctx: &'a mut Runtime, args: Source<Arguments>) -> BoxFuture<'a, EvalResult> {
    async move {
        let (source, args) = args.take();
        let mut args = args.unchecked();
        let target = args.peek();

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
                args.next();
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
                                    let load_args = Arguments::positional(vec![
                                        Source::builtin(PathBuf::from(p.to_owned()).into()),
                                    ]);

                                    // Load workspace
                                    let ws = match load_script(ctx, source.clone().with(load_args)).await {
                                        Ok(ws) => ws,
                                        Err(e) => {
                                            ctx.loading.lock().pop();
                                            return Err(source.with("while running 'ergo [workspace] prelude' to load script").context_for_error(e));
                                        }
                                    };

                                    // Try to access prelude, but allow failures
                                    match Errored::ignore(traits::bind(ctx, ws, source.clone().with(types::Index(
                                            Source::builtin(types::String::from(PRELUDE_ARG).into())
                                        ).into_value()))).await {
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

            let loaded_context = source.clone().with(format!("loaded from '{}'", p.display()));

            // If resolved to an ancestor workspace, run arguments on
            // the result of `[loaded]:command`.
            let loaded = if was_workspace {
                traits::bind(ctx, loaded, source.clone().with(types::Index(
                    Source::builtin(types::String::from(WORKSPACE_FALLBACK_ARG).into())
                ).into_value())).await?
            } else {
                loaded
            };

            // If there are remaining arguments apply them immediately.
            if !args.is_empty() {
                traits::bind(ctx, loaded, source.clone().with(types::Args { args }.into_value())).await
            } else {
                args.unused_arguments()?;
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
#[derive(Debug, PartialEq)]
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
    /// A binding was left unset.
    UnsetBinding,
    /// A load failed to find a module.
    LoadFailed {
        was_loading: bool,
        was_workspace: bool,
        load_path: Option<std::path::PathBuf>,
    },
    /// An error occured while evaluating a value.
    ValueError(ValueError),
    /// A generic error message.
    GenericError(String),
}

#[derive(Debug)]
pub struct ValueError(pub grease::Error);

impl PartialEq for ValueError {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
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
            UnsetBinding => write!(f, "binding left unset"),
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
            ValueError(e) => write!(f, "{}", e.0),
            GenericError(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ValueError(e) => Some(e.0.as_ref()),
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
        Error::ValueError(ValueError(e))
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

impl PartialEq<grease::Error> for Error {
    fn eq(&self, err: &grease::Error) -> bool {
        if let &[src] = grease::error::GreaseError::source(err).as_slice() {
            if let Some(e) = src.downcast_ref_external::<Self>() {
                return e == self;
            }
        }
        false
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Evaluator {
    warn_string_in_env: bool,
    warn_compile_missing_get: bool,
}

impl Default for Evaluator {
    fn default() -> Self {
        Evaluator {
            warn_string_in_env: true,
            warn_compile_missing_get: false,
        }
    }
}

#[allow(unused)]
impl Evaluator {
    pub fn warn_string_in_env(&self, warn_string_in_env: bool) -> Self {
        Evaluator {
            warn_string_in_env,
            ..*self
        }
    }

    pub fn warn_compile_missing_get(&self, warn_compile_missing_get: bool) -> Self {
        Evaluator {
            warn_compile_missing_get,
            ..*self
        }
    }

    fn lookup(self, ctx: &Runtime, k: Value) -> EResult<EResult<Source<Value>>> {
        trace!("looking up '{}' in environment", k.id());
        match ctx.env_get(&k) {
            None => Err(Error::MissingBinding(k).into()),
            Some(value) => Ok({
                trace!(
                    "found match in environment for '{}': {}",
                    k.id(),
                    if let Ok(v) = &value {
                        format!("{}", v.id())
                    } else {
                        "<errored>".to_owned()
                    }
                );
                value.clone()
            }),
        }
    }

    /// Compiles environment accesses into the expression (and checks for access validity).
    ///
    /// TODO merge this behavior with evaluate, with an Evaluator flag to prevent command
    /// application (and anything else with side effects).
    fn compile_env_into<'a>(
        self,
        ctx: &'a mut Runtime,
        expr: Expr,
    ) -> BoxFuture<'a, EResult<Expr>> {
        async move {
            use Expression::*;
            let warn_string_in_env = self.warn_string_in_env && ctx.lint;
            let src = expr.source();
            expr.map_async(|e| async {
                Ok(match e {
                    Empty => Compiled(types::Unit.into_value()),
                    BindAny => Compiled(
                        types::Unbound::new(
                            |_, _| async { Ok(types::Unit.into_value()) }.boxed(),
                            depends![ergo_runtime::namespace_id!(ergo::any)],
                            None,
                        )
                        .into(),
                    ),
                    String(s) => {
                        let s: Value = types::String::from(s).into();
                        if let (true, Some(_)) = (warn_string_in_env, ctx.env_get(&s)) {
                            ctx.log.warn(format!(
                                "{}",
                                src.with(
                                    "string literal is in the environment; did you mean to index?"
                                )
                            ));
                        }
                        Compiled(s)
                    }
                    Array(es) => {
                        ctx.env_scoped(|ctx| {
                            async move {
                                let mut r = Vec::new();
                                for e in es {
                                    r.push(self.compile_env_into(ctx, e).await);
                                }
                                r.into_iter().collect_result().map(Array)
                            }
                            .boxed()
                        })
                        .await
                        .0?
                    }
                    Map(es) => {
                        ctx.env_scoped(|ctx| {
                            async move {
                                let mut r = Vec::new();
                                for e in es {
                                    r.push(self.compile_env_into(ctx, e).await);
                                }
                                r.into_iter().collect_result().map(Map)
                            }
                            .boxed()
                        })
                        .await
                        .0?
                    }
                    Block(es) => {
                        ctx.env_scoped(|ctx| {
                            async move {
                                let mut r = Vec::new();
                                for e in es {
                                    r.push(self.compile_env_into(ctx, e).await);
                                }
                                r.into_iter().collect_result().map(Block)
                            }
                            .boxed()
                        })
                        .await
                        .0?
                    }
                    Function(bind, e) => {
                        let (bind, e) = ctx
                            .env_scoped(move |ctx| {
                                async move {
                                    let bind = ctx
                                        .env_set_to_current(|ctx| self.compile_env_into(ctx, *bind))
                                        .await?;
                                    self.compile_env_into(ctx, *e).await.map(|e| (bind, e))
                                }
                                .boxed()
                            })
                            .await
                            .0?;
                        Function(bind.into(), e.into())
                    }
                    Bind(bind, e) => {
                        let e = self.compile_env_into(ctx, *e).await?;
                        let bind = ctx
                            .env_set_to_current(|ctx| self.compile_env_into(ctx, *bind))
                            .await?;
                        Bind(bind.into(), e.into())
                    }
                    Get(v) => {
                        let v = self
                            .warn_string_in_env(false)
                            .compile_env_into(ctx, *v)
                            .await?;
                        let (v_source, v) = v.take();
                        match v {
                            Compiled(inner) => {
                                let result = self.lookup(ctx, inner.clone());
                                match result {
                                    Ok(Ok(v)) => Compiled(v.unwrap()),
                                    Ok(Err(e)) => {
                                        if Error::UnsetBinding == e {
                                            Get(v_source.with(Compiled(inner)).into())
                                        } else {
                                            return Err(e);
                                        }
                                    }
                                    Err(_) => {
                                        if ctx.lint && self.warn_compile_missing_get {
                                            ctx.log.warn(format!(
                                                "{}",
                                                v_source
                                                    .clone()
                                                    .with("dynamically-determined capture value")
                                            ));
                                        }
                                        Get(v_source.with(Compiled(inner)).into())
                                    }
                                }
                            }
                            v => Get(v_source.with(v).into()),
                        }
                    }
                    Set(v) => {
                        let v = self
                            .warn_string_in_env(false)
                            .compile_env_into(ctx, *v)
                            .await?;
                        if let Compiled(k) = &*v {
                            ctx.env_insert(k.clone(), Err(Error::UnsetBinding.into()));
                        }
                        Set(v.into())
                    }
                    BindEqual(a, b) => {
                        let a = self.compile_env_into(ctx, *a).await?;
                        let b = self.compile_env_into(ctx, *b).await?;
                        BindEqual(a.into(), b.into())
                    }
                    Index(a, b) => {
                        let a = self.compile_env_into(ctx, *a).await?;
                        let b = self.compile_env_into(ctx, *b).await?;
                        Index(a.into(), b.into())
                    }
                    Command(cmd, args) => {
                        ctx.env_scoped(|ctx| {
                            async move {
                                let cmd = self.compile_env_into(ctx, *cmd).await?;

                                let mut res = Vec::new();
                                for arg in args {
                                    res.push(self.compile_env_into(ctx, arg).await);
                                }
                                res.into_iter()
                                    .collect_result()
                                    .map(|args| Command(cmd.into(), args))
                            }
                            .boxed()
                        })
                        .await
                        .0?
                    }
                    BindCommand(cmd, args) => {
                        ctx.env_scoped(|ctx| {
                            async move {
                                let cmd = self.compile_env_into(ctx, *cmd).await?;

                                let mut res = Vec::new();
                                for arg in args {
                                    res.push(self.compile_env_into(ctx, arg).await);
                                }
                                res.into_iter()
                                    .collect_result()
                                    .map(|args| BindCommand(cmd.into(), args))
                            }
                            .boxed()
                        })
                        .await
                        .0?
                    }
                    If(cond, if_true, if_false) => {
                        let cond = self.compile_env_into(ctx, *cond).await?;
                        let if_true = self.compile_env_into(ctx, *if_true).await?;
                        let if_false = match if_false {
                            None => None,
                            Some(e) => Some(self.compile_env_into(ctx, *e).await?),
                        };
                        If(cond.into(), if_true.into(), if_false.map(Box::from))
                    }
                    IfBind(cond, if_true, if_false) => {
                        let cond = self.compile_env_into(ctx, *cond).await?;
                        let if_true = self.compile_env_into(ctx, *if_true).await?;
                        let if_false = match if_false {
                            None => None,
                            Some(e) => Some(self.compile_env_into(ctx, *e).await?),
                        };
                        IfBind(cond.into(), if_true.into(), if_false.map(Box::from))
                    }
                    Force(v) => Force(self.compile_env_into(ctx, *v).await?.into()),
                    Merge(v) => Merge(self.compile_env_into(ctx, *v).await?.into()),
                    DocComment(s, v) => {
                        let mut v = self.compile_env_into(ctx, *v).await?;
                        if let Compiled(value) = &mut *v {
                            value.set_metadata(&metadata::Doc, types::String::from(s).into());
                            v.unwrap()
                        } else {
                            DocComment(s, v.into())
                        }
                    }
                    Compiled(v) => Compiled(v),
                })
            })
            .await
            .transpose_with_context("in expression")
        }
        .boxed()
    }

    fn command_args<'a>(
        self,
        ctx: &'a mut Runtime,
        cmd: Expr,
        args: Vec<Expr>,
    ) -> BoxFuture<'a, grease::Result<(Source<Value>, UncheckedArguments)>> {
        async move {
            let cmd = self.warn_string_in_env(false).evaluate(ctx, cmd).await?;

            // Open new scope to collect args and differentiate kw args
            let (args, kw_args) = ctx.env_scoped(move |ctx| {
                async move {
                    let mut results = Vec::new();
                    for arg in args {
                        match self.evaluate(ctx, arg).await {
                            Err(e) => results.push(Err(e)),
                            Ok(v) => {
                                let (v_source, v) = v.take();
                                let mut push_original = false;
                                match_value!(peek v.clone() => {
                                    types::Merge => |inner| {
                                        let (inner_source, inner) = inner.await?.0.clone().take();
                                        match_value!(inner => {
                                            types::Array => |val| {
                                                match val.await {
                                                    Err(e) => results.push(Err(inner_source.with("while merging array").context_for_error(e))),
                                                    Ok(arr) => results.extend(arr.owned().0.into_iter().map(|v| Ok(v_source.clone().with(v)))),
                                                }
                                            },
                                            types::Map => |val| {
                                                match val.await {
                                                    Err(e) => results.push(Err(inner_source.with("while merging map").context_for_error(e))),
                                                    Ok(m) => ctx.env_set_to_current(move |ctx| async move { ctx.env_extend(m.owned().0.into_iter().map(|(k,v)| {
                                                        (k, Ok(inner_source.clone().with(v)).into())
                                                    })) }.boxed()).await
                                                }
                                            },
                                            types::Unbound => |_| push_original = true,
                                            => |v| {
                                                let name = ctx.type_name(v.grease_type_immediate().unwrap()).await?;
                                                results.push(Err(inner_source.with(format!("cannot merge value with type {}", name)).into_grease_error()));
                                            }
                                        }).await?
                                    },
                                    BindExpr => |_| (),
                                    => |_| push_original = true
                                }).await?;
                                if push_original {
                                    results.push(Ok(v_source.with(v)));
                                }
                            }
                        }
                    }
                    results.collect_result::<Vec<_>>()
                }
                .boxed()
            }).await;

            let args = args?;
            let kw_args = kw_args.into_iter().map(|(k,vres)| vres.map(move |v| (v.source().with(k),v)).into_result()).collect_result::<BTreeMap<_,_>>()?;

            Ok((cmd, Arguments::new(args, kw_args).unchecked()))
        }.boxed()
    }

    fn evaluate_block<'a>(
        self,
        ctx: &'a mut Runtime,
        es: Vec<Expr>,
    ) -> BoxFuture<'a, EResult<(Source<Value>, ScriptEnv)>> {
        async move {
            let (result, scope) = ctx.env_scoped(move |ctx| {
                async move {
                    let mut results = Vec::new();
                    let mut rest_binding: Option<Source<()>> = None;
                    for e in es {
                        match self.evaluate(ctx, e).await {
                            Err(e) => results.push(Err(e)),
                            Ok(v) => {
                                let (v_source, v) = v.take();
                                match_value!(peek v => {
                                    types::Merge => |inner| {
                                        let (inner_source, inner) = inner.await?.0.clone().take();
                                        match_value!(inner => {
                                            types::Map => |val| {
                                                match val.await {
                                                    Err(e) => results.push(Err(inner_source.with("while merging map").context_for_error(e))),
                                                    Ok(m) => ctx.env_set_to_current(move |ctx| async move { ctx.env_extend(m.owned().0.into_iter().map(|(k,v)| {
                                                        (k, Ok(inner_source.clone().with(v)).into())
                                                    })) }.boxed()).await
                                                }
                                            },
                                            types::Unbound => |v| {
                                                if let Some(binding_source) = &rest_binding {
                                                    results.push(Err(binding_source.clone().with("previous rest binding").context_for_error(
                                                        inner_source.with("cannot have more than one rest binding in map")
                                                            .into_grease_error())
                                                    ));
                                                } else {
                                                    rest_binding = Some(inner_source.clone());
                                                    ctx.env_set_to_current(move |ctx| async move {
                                                        ctx.env_insert(types::BindRest.into_value(), Ok(inner_source.with(v.into())));
                                                    }.boxed()).await;
                                                }
                                            },
                                            => |v| {
                                                let name = ctx.type_name(v.grease_type_immediate().unwrap()).await?;
                                                results.push(Err(inner_source.with(format!("cannot merge value with type {}", name)).into_grease_error()));
                                            }
                                        }).await?
                                    },
                                    => |v| results.push(Ok(v_source.with(v)))
                                }).await?;
                            }
                        }
                    }
                    Ok(results.collect_result::<Vec<_>>()?
                        .into_iter()
                        .last()
                        .unwrap_or(Source::builtin(types::Unit.into_value())))
                }
                .boxed()
            }).await;
            result.map(move |v| (v, scope))
        }.boxed()
    }

    pub fn evaluate<'a>(self, ctx: &'a mut Runtime, e: Expr) -> BoxFuture<'a, EvalResult> {
        async move {
            let src = e.source();
            let rsrc = src.clone();
            let warn_string_in_env = self.warn_string_in_env && ctx.lint;
            self.warn_string_in_env(true);
            e.map_async(|e| async {
                use Expression::*;
                match e {
                    Empty => Ok(types::Unit.into_value()),
                    BindAny => {
                        Ok(types::Unbound::new(|_, _| async { Ok(BindExpr.into_value()) }.boxed(),
                            depends![ergo_runtime::namespace_id!(ergo::any)], None).into())
                    },
                    String(s) => {
                        let s: Value = types::String::from(s).into();
                        if let (true, Some(_)) = (warn_string_in_env, ctx.env_get(&s)) {
                            ctx.log.warn(format!("{}", src.with("string literal is in the environment; did you mean to index?")));
                        }
                        Ok(s)
                    },
                    Array(es) => {
                        ctx.env_scoped(move |ctx| {
                            async move {
                                let mut results = Vec::new();
                                for e in es {
                                    match self.evaluate(ctx, e).await {
                                        Err(e) => results.push(Err(e)),
                                        Ok(v) => {
                                            let v = v.unwrap();
                                            let mut push_original = false;
                                            match_value!(peek v.clone() => {
                                                types::Merge => |inner| {
                                                    let (inner_source, inner) = inner.await?.0.clone().take();
                                                    match_value!(inner => {
                                                        types::Array => |val| {
                                                            match val.await {
                                                                Err(e) => results.push(Err(inner_source.with("while merging array").context_for_error(e))),
                                                                Ok(arr) => results.extend(arr.owned().0.into_iter().map(Ok)),
                                                            }
                                                        },
                                                        types::Map => |val| {
                                                            match val.await {
                                                                Err(e) => results.push(Err(inner_source.with("while merging map").context_for_error(e))),
                                                                Ok(m) => ctx.env_set_to_current(move |ctx| async move { ctx.env_extend(m.owned().0.into_iter().map(|(k,v)| {
                                                                    (k, Ok(inner_source.clone().with(v)).into())
                                                                }))}.boxed()).await
                                                            }
                                                        },
                                                        types::Unbound => |_| push_original = true,
                                                        => |v| {
                                                            let name = ctx.type_name(v.grease_type_immediate().unwrap()).await?;
                                                            results.push(Err(inner_source.with(format!("cannot merge value with type {}", name)).into_grease_error()));
                                                        }
                                                    }).await?
                                                },
                                                => |_| push_original = true
                                            }).await?;
                                            if push_original {
                                                results.push(Ok(v));
                                            }
                                        }
                                    }
                                }
                                Ok(types::Array(results.collect_result::<Vec<_>>()?.into()).into_value())
                            }
                            .boxed()
                        })
                        .await.0
                    }
                    Map(es) => {
                        // Return the env scope as a map.
                        self.evaluate_block(ctx, es).await?.1
                            .into_iter()
                            .map(|(k, v)| v.map(move |v| (k, v.unwrap())).into_result())
                            .collect_result::<BstMap<_, _>>()
                            .map(|ret| types::Map(ret).into_value(ctx))
                    }
                    Block(es) => Ok(self.evaluate_block(ctx, es).await?.0.unwrap()),
                    Function(bind, e) => {
                        let (bind, e) = ctx
                            .env_scoped(move |ctx| {
                                async move {
                                    let bind = ctx.env_set_to_current(|ctx| self.compile_env_into(ctx, *bind)).await?;
                                    self.compile_env_into(ctx, *e).await.map(move |e| (bind, e))
                                }
                                .boxed()
                            })
                            .await
                            .0?;

                        let deps = depends![^&*bind, ^&*e];

                        Ok(types::Unbound::new(move |ctx, v| {
                            let bind = bind.clone();
                            let e = e.clone();
                            let mut ctx = ctx.empty();
                            async move {
                                ctx.env_scoped(move |ctx| {
                                    async move {
                                        let bind = ctx.env_set_to_current(|ctx| self.evaluate(ctx, bind)).await?;
                                        traits::bind(ctx, bind, v).await.map_err(BindError::wrap)?;
                                        self.evaluate(ctx, e).await.map_err(BindError::unwrap)
                                    }.boxed()
                                }).await.0.map(Source::unwrap)
                            }
                            .boxed()
                        }, deps, None).into())
                    }
                    Bind(bind, e) => {
                        // evaluate `e` first, so that any bindings in `bind` aren't in the
                        // environment
                        let result_e = self.evaluate(ctx, *e).await.map_err(BindError::unwrap);
                        let bind = ctx.env_set_to_current(|ctx| self.evaluate(ctx, *bind)).await?;

                        let result = match result_e {
                            Err(e) => Err(e),
                            Ok(e) => traits::bind(ctx, bind, e).await.map_err(BindError::wrap)
                        };

                        // Replace unset values with aborted errors.
                        let mut had_unset = false;
                        ctx.env_current(|env| {
                            for (_,v) in env.iter_mut() {
                                if let RResult::RErr(e) = v {
                                    if Error::UnsetBinding == *e {
                                        *v = RResult::RErr(grease::Error::aborted());
                                        had_unset = true;
                                    }
                                }
                            }
                        });

                        // If the result is a success, error on unset bindings.
                        result.and_then(|ret| {
                            if had_unset {
                                Err(ret.with(Error::UnsetBinding).into())
                            } else {
                                Ok(BindExpr.into_value())
                            }
                        })
                    }
                    Get(v) => {
                        let v = self.warn_string_in_env(false).evaluate(ctx, *v).await?;

                        // Lookup in environment
                        let (v_source, v) = v.take();
                        self.lookup(ctx, v).and_then(|r| r)
                            .map_err(|e| v_source.with("while retrieving binding").context_for_error(e))
                            .map(Source::unwrap)
                    }
                    Set(v) => {
                        let k = self.warn_string_in_env(false).evaluate(ctx, *v).await?.unwrap();
                        ctx.env_insert(k.clone(), Err(Error::UnsetBinding.into()));
                        let env = ctx.env_to_set();
                        Ok(types::Unbound::new(move |_, v| {
                            let env = env.clone();
                            let k = k.clone();
                            async move {
                                env.lock().insert(k, Ok(v).into());
                                Ok(types::Unit.into_value())
                            }.boxed()
                        }, depends![ergo_runtime::namespace_id!(ergo::set)], None).into())
                    }
                    BindEqual(a, b) => {
                        let a = self.evaluate(ctx, *a).await?;
                        let b = self.evaluate(ctx, *b).await?;
                        let deps = depends![*a, *b];

                        let a = traits::delay_bind(ctx, a).await?;
                        let b = traits::delay_bind(ctx, b).await?;
                        Ok(types::Unbound::new(move |_, v| {
                            let a = a.clone();
                            let b = b.clone();
                            async move {
                                a.bind(v.clone()).await?;
                                b.bind(v).await?;
                                Ok(types::Unit.into_value())
                            }.boxed()
                        }, deps, None).into())
                    }
                    Index(a, b) => {
                        let a = self.evaluate(ctx, *a).await?;
                        let b = self.evaluate(ctx, *b).await?;

                        traits::bind(ctx, a, src.with(types::Index(b).into_value())).await
                            .map(Source::unwrap)
                    }
                    Command(cmd, args) => {
                        let (cmd, args) = self.command_args(ctx, *cmd, args).await?;

                        traits::bind(ctx, cmd, src.with(types::Args { args }.into_value())).await
                            .map(Source::unwrap)
                    }
                    BindCommand(cmd, args) => {
                        let (cmd, args) = self.command_args(ctx, *cmd, args).await?;

                        traits::bind(ctx, cmd, src.with(types::BindArgs { args }.into_value())).await
                            .map(Source::unwrap)
                    }
                    IfBind(cond_bind, if_true, if_false) => {
                        ctx.env_scoped(move |ctx| async move {
                            let result = match Errored::ignore(self.evaluate(ctx, *cond_bind)).await {
                                Ok(_) => true,
                                Err(e) => if BindError::only_bind_errors(&e) {
                                    false
                                } else {
                                    return Err(e);
                                }
                            };

                            if result {
                                self.evaluate(ctx, *if_true).await.map(Source::unwrap)
                            } else {
                                match if_false {
                                    Some(e) => self.evaluate(ctx, *e).await.map(Source::unwrap),
                                    None => Ok(types::Unit.into_value())
                                }
                            }
                        }.boxed()).await.0
                    }
                    If(cond, if_true, if_false) => {
                        // TODO should this be env_scoped?
                        let cond = self.evaluate(ctx, *cond).await?;
                        let to_sourced = ctx.into_sourced::<bool>(cond);
                        let cond = to_sourced.await?.unwrap();

                        let if_true = self.compile_env_into(ctx, *if_true).await?;
                        let if_false = match if_false {
                            None => None,
                            Some(e) => Some(self.compile_env_into(ctx, *e).await?)
                        };

                        let mut deps = depends![cond, ^&*if_true];

                        if let Some(v) = &if_false {
                            deps += (&**v).into();
                        }

                        let mut ctx = ctx.empty();
                        Ok(Value::dyn_new(async move {
                            let c = cond.await?;
                            Ok(if *c.as_ref() {
                                self.evaluate(&mut ctx, if_true).await?.unwrap().into_any_value()
                            } else {
                                match if_false {
                                    Some(e) => self.evaluate(&mut ctx, e).await?.unwrap().into_any_value(),
                                    None => types::Unit.into_value().into_any_value()
                                }
                            })
                        }, deps))
                    }
                    Force(val) => {
                        let val = self.evaluate(ctx, *val).await?;

                        val.map_async(|val| async {
                            // If the value is a dynamic value, get the inner value.
                            // Otherwise return the value by its (shallow) content.
                            match val.dyn_value().await? {
                                Ok(inner) => Ok(inner),
                                Err(val) => ctx.value_by_content(val, false).await
                            }
                        }).await.transpose_err_with_context("in forced value")
                    }
                    Merge(val) => {
                        let val = self.evaluate(ctx, *val).await?;

                        Ok(types::Merge(val).into_value())
                    }
                    DocComment(s, val) => {
                        let mut val = self.evaluate(ctx, *val).await?.unwrap();
                        val.set_metadata(&metadata::Doc, types::String::from(s).into());
                        Ok(val)
                    }
                    Compiled(v) => Ok(v)
                }
            })
            .await
            .transpose_with_context("in expression")
            .map(|v| v.map(|v| rsrc
                        .with("while evaluating value returned by this expression")
                        .imbue_error_context(v)
            ))
        }
        .boxed()
    }
}
