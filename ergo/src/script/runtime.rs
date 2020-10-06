//! Script runtime definitions.
//!
//! The runtime is responsible for evaluating AST expressions, producing values or errors.
//! Importantly, it tracks source locations for values and errors so that when an error occurs,
//! useful error information can be provided.
//!
//! This module defines grease::Plan for expressions, where the output is as described above.

use super::ast::{ArrayPattern, CmdPat, Expression, MapPattern, MergeExpression, Pat, Pattern};
use crate::constants::{
    app_dirs, LOAD_PATH_BINDING, SCRIPT_DIR_NAME, SCRIPT_EXTENSION, SCRIPT_PRELUDE_NAME,
    SCRIPT_WORKSPACE_NAME, WORKING_DIRECTORY_BINDING,
};
use abi_stable::{rvec, std_types::RResult};
use ergo_runtime::source::{FileSource, IntoSource, Source};
use ergo_runtime::Result as SoResult;
use ergo_runtime::{
    source_value_as, types, ContextEnv, EvalResult, FunctionArguments, FunctionCall,
    ResultIterator, Runtime, ScriptEnv, UncheckedFunctionArguments,
};
use futures::future::{BoxFuture, FutureExt};
use grease::{
    bst::BstMap,
    depends, match_value,
    path::PathBuf,
    types::GreaseType,
    value::{IntoValue, Value},
};
use libloading as dl;
use log::{debug, trace};
use std::collections::BTreeMap;
use std::fmt;
use std::path;
use std::str::FromStr;

/// Script type indicating that the env should be returned as a map.
#[derive(Clone, Copy, Debug, GreaseType, Hash)]
pub struct ScriptEnvIntoMap;

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

/// Add the LOAD_PATH_BINDING to the environment, with the working directory and
/// any applicable system directories.
pub fn add_load_path(env: &mut ScriptEnv, work_dir: &Value) {
    let mut vals = rvec![work_dir.clone()];

    // Add neighboring share directories when running in a [prefix]/bin directory.
    let mut neighbor_dir = std::env::current_exe().ok().and_then(|path| {
        path.parent().and_then(|parent| {
            if parent.file_name() == Some("bin".as_ref()) {
                let path = parent
                    .parent()
                    .expect("must have parent directory")
                    .join("share")
                    .join(crate::constants::PROGRAM_NAME)
                    .join("lib");
                if path.exists() {
                    Some(path)
                } else {
                    None
                }
            } else {
                None
            }
        })
    });

    // If the neighbor directory is somewhere in the home directory, it should be added prior to the local
    // data app dir.
    if let (Some(dir), Some(user_dirs)) = (&neighbor_dir, &directories::UserDirs::new()) {
        if dir.starts_with(user_dirs.home_dir()) {
            vals.push(PathBuf::from(neighbor_dir.take().unwrap()).into());
        }
    }

    // Add local data app dir.
    if let Some(proj_dirs) = app_dirs() {
        let path = proj_dirs.data_local_dir().join("lib");
        if path.exists() {
            vals.push(PathBuf::from(path).into());
        }
    }

    // If the neighbor directory wasn't added, it should be added now, after the local data app dir.
    if let Some(dir) = neighbor_dir {
        vals.push(PathBuf::from(dir).into());
    }

    env.insert(
        LOAD_PATH_BINDING.into(),
        Ok(Source::builtin(types::Array(vals).into())).into(),
    );
}

/// Load and execute a script given the function call context.
pub fn load_script<'a>(ctx: &'a mut FunctionCall) -> BoxFuture<'a, EvalResult> {
    async move {
        let target = ctx.args.peek().ok_or("no load target provided")?;
        let mut source = target.source();

        fn script_path_exists<'a, P: 'a + AsRef<path::Path>>(
            name: P,
            try_add_extension: bool,
        ) -> impl FnMut(&path::Path) -> Option<path::PathBuf> + 'a {
            move |path| {
                if try_add_extension {
                    if let Some(file_name) = name.as_ref().file_name() {
                        let mut p = file_name.to_owned();
                        p.push(".");
                        p.push(SCRIPT_EXTENSION);
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
                        let dir = p.join(SCRIPT_DIR_NAME);
                        if dir.exists() {
                            p = dir;
                        } else {
                            let ws = p.join(SCRIPT_WORKSPACE_NAME);
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

        // If target is a string or path, try to find it in the load path
        let tp = target.grease_type();
        let to_load = if *tp == types::String::grease_type() || *tp == PathBuf::grease_type() {
            let target_path: std::path::PathBuf = match_value!(target.clone().unwrap() => {
                types::String => |v| <&str>::from(v.await?.as_ref()).into(),
                PathBuf => |v| v.await?.owned().into(),
                => |_| panic!("unexpected value type")
            });

            load_path = Some(target_path.to_owned());

            // Get current load path
            let loadpath = match ctx.env_get(LOAD_PATH_BINDING) {
                Some(v) => {
                    let v = v.map(|r| r.clone()).map_err(|e| e.clone())?;
                    let source = v.source();
                    source_value_as!(v.clone(), types::Array, ctx)?
                        .unwrap()
                        .await?
                        .owned()
                        .0
                        .into_iter()
                        .map(|v| source.clone().with(v))
                        .collect()
                }
                None => vec![],
            };

            // Get paths from load path
            let mut paths: Vec<std::path::PathBuf> = Vec::new();
            for path in loadpath {
                paths.push(
                    source_value_as!(path, PathBuf, ctx)?
                        .unwrap()
                        .await?
                        .owned()
                        .into_pathbuf(),
                );
            }

            // Try to find path match in load paths
            paths
                .iter()
                .map(|v| v.as_path())
                .find_map(script_path_exists(target_path, true))
        } else {
            None
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
                source = Source::builtin(());

                // If the current file is a workspace, allow it to load parent workspaces.
                let loading_workspace = ctx
                    .loading
                    .last()
                    .map(|p| p.clone().into_pathbuf().file_name().unwrap() == SCRIPT_WORKSPACE_NAME)
                    .unwrap_or(false);

                let mod_path = source_value_as!(
                    ctx.env_get(WORKING_DIRECTORY_BINDING)
                        .expect("working directory unset")
                        .map_err(|e| e.clone())?
                        .clone(),
                    PathBuf,
                    ctx
                )?
                .unwrap()
                .await?;
                let mod_path = mod_path.as_ref().as_ref();

                let mut ancestors = mod_path.ancestors().peekable();
                if loading_workspace {
                    while let Some(v) = ancestors.peek().and_then(|a| a.file_name()) {
                        if v == SCRIPT_WORKSPACE_NAME {
                            ancestors.next();
                        } else {
                            break;
                        }
                    }
                    // Skip one more to drop parent directory of top-most workspace, which would find
                    // the same workspace as the original.
                    ancestors.next();
                }

                ancestors.find_map(script_path_exists(SCRIPT_WORKSPACE_NAME, false))
            }
        };

        let mut was_loading = false;
        let to_load = to_load.and_then(|p| {
            for l_path in ctx.loading.iter() {
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
                        ctx.loading.push(PathBuf::from(p.clone()));
                    }
                    ret
                };

                match cached {
                    None => {
                        let plugin = is_plugin(&p);

                        // Only load prelude if this is not a workspace nor a plugin.
                        let load_prelude = p.file_name().unwrap() != SCRIPT_WORKSPACE_NAME && !plugin; // unwrap because p must have a final component

                        let mod_path = p.parent().unwrap(); // unwrap because file must exist in some directory

                        // Load the prelude. The prelude may not exist (without error), however if it does
                        // exist it must evaluate to a map.
                        let prelude = if load_prelude {
                            let call_site = ctx.call_site.clone();
                            let mut fctx = FunctionCall::new(
                                ctx,
                                FunctionArguments::positional(vec![Source::builtin(
                                    types::String::from(SCRIPT_PRELUDE_NAME.to_owned()).into(),
                                )]),
                                call_site,
                            );

                            // New scope with working directory set to that of the script, so that
                            // prelude loading is relative to that directory.
                            let mut env = ScriptEnv::default();
                            env.insert(
                                WORKING_DIRECTORY_BINDING.into(),
                                Ok(Source::builtin(PathBuf::from(mod_path.to_owned()).into()))
                                    .into(),
                            );
                            match fctx.env_scoped(env, load_script).await.0 {
                                Ok(v) => {
                                    let (v_source, v) = v.take();
                                    match_value!(v => {
                                        () => |_| None,
                                        types::Map => |v| Some(v_source.with(v).await.transpose_ok()?.map(|v| v.owned().0)),
                                        => |_| return Err(v_source.with(Error::CannotMerge(
                                                "prelude did not evaluate to a map".into(),
                                            )).into())
                                    })
                                },
                                Err(e) => {
                                    fctx.args.clear();
                                    fn has_ignored_error(
                                        e: &(dyn std::error::Error + 'static),
                                    ) -> bool {
                                        if let Some(e) = grease::value::error::downcast_ref::<Error>(e) {
                                            if let Error::LoadFailed { .. } = e {
                                                debug!("not loading prelude: load failed");
                                                true
                                            } else {
                                                false
                                            }
                                        }
                                        else if let Some(_) = grease::value::error::downcast_ref::<Source<ergo_runtime::error::UnexpectedPositionalArguments>>(e) {
                                            debug!("not loading prelude: workspace did not accept prelude argument");
                                            true
                                        }
                                        else {
                                            e
                                            .source()
                                            .map(has_ignored_error)
                                            .unwrap_or(false)
                                        }
                                    }
                                    if has_ignored_error(e.error_ref()) {
                                        None
                                    } else {
                                        ctx.loading.pop();
                                        return Err(e);
                                    }
                                }
                            }
                        } else {
                            None
                        };

                        let mod_path: Value = PathBuf::from(mod_path.to_owned()).into();

                        // Add initial load path binding first; the prelude may override it.
                        let mut top_level_env: ScriptEnv = Default::default();
                        add_load_path(&mut top_level_env, &mod_path);

                        if !plugin {
                            if let Some(v) = prelude {
                                let (source, v) = v.take();
                                top_level_env.extend(
                                    v.into_iter()
                                        .map(|(k, v)| (k, Ok(source.clone().with(v)).into())),
                                );
                            }
                        }

                        // Add mod path binding last; it should not be overriden.
                        top_level_env.insert(
                            WORKING_DIRECTORY_BINDING.into(),
                            Ok(Source::builtin(mod_path)).into(),
                        );

                        let result = if !plugin {
                            let mut script =
                                super::Script::load(Source::new(FileSource(p.clone())))?;
                            script.top_level_env(top_level_env);
                            script.evaluate(ctx).await.into()
                        } else {
                            let lib = dl::Library::new(&p)?;
                            let f: dl::Symbol<
                                extern "C" fn(
                                    &mut Runtime,
                                )
                                    -> RResult<Source<Value>, grease::value::Error>,
                            > = unsafe { lib.get(b"_ergo_plugin") }?;
                            let result = f(ctx);
                            ctx.lifetime(lib);
                            result
                        };

                        let cache = ctx.load_cache.clone();
                        let mut cache = cache.lock();
                        cache.insert(PathBuf::from(p.clone()), result.clone());
                        ctx.loading.pop();
                        result
                    }
                    Some(v) => v,
                }
                .into_result()?
            };

            match loaded.unwrap().typed::<types::Function>() {
                Ok(f) => {
                    let args = std::mem::take(&mut ctx.args);
                    apply_value(ctx, source.with(f.into()), args.unchecked(), false).await
                },
                Err(v) => {
                    ctx.unused_arguments()?;
                    Ok(source.with(v))
                }
            }
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
    /// No binding with the given name is available in the current environment.
    MissingBinding(String),
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
    UnexpectedNonPositionalArgument(String),
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
    ValueError(grease::value::Error),
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
            MissingBinding(s) => write!(f, "'{}' is not available in the current environment", s),
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
            UnexpectedNonPositionalArgument(s) => write!(
                f,
                "the function does not accept a non-positional argument with key '{}'",
                s
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

impl From<grease::value::Error> for Error {
    fn from(e: grease::value::Error) -> Self {
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

pub async fn apply_pattern(
    ctx: &mut Runtime,
    pat: Pat,
    val: Option<Source<Value>>,
) -> SoResult<ScriptEnv> {
    let mut ret = ScriptEnv::new();
    let mut errs = Vec::new();

    _apply_pattern(
        ctx,
        &mut ret,
        &mut errs,
        pat,
        val.map(|v| v.map(PatternValues::singular)),
    )
    .await;
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(grease::value::Error::aggregate(errs))
    }
}

fn _apply_pattern<'a>(
    ctx: &'a mut Runtime,
    env: &'a mut ScriptEnv,
    errs: &'a mut Vec<grease::value::Error>,
    pat: Pat,
    val: Option<Source<PatternValues>>,
) -> BoxFuture<'a, ()> {
    async move {
        use Pattern::*;
        let (source, pat) = pat.take();
        let desc = std::mem::discriminant(&pat);
        match pat {
            Any => (),
            Literal(e) => match (Rt(e).evaluate(ctx).await, val) {
                (Ok(result), Some(val)) => {
                    if *result != (*val).literal {
                        errs.push(
                            result.source().with("value definition").context_for_error(
                                source
                                    .with(Error::PatternMismatch((*result).clone()))
                                    .into(),
                            ),
                        );
                    }
                }
                _ => (),
            },
            Binding(name) => {
                env.insert(
                    name.into(),
                    match val {
                        Some(v) => Ok(v.map(|v| v.binding)),
                        None => Err("errored value".into()),
                    }
                    .into(),
                );
            }
            Array(inner) => {
                let mut orig_val = None;
                let vals: Vec<Option<Source<PatternValues>>> = {
                    let val = match val {
                        Some(v) => {
                            orig_val = Some(v.clone());
                            let (vsource, v) = v.take();
                            match v.binding.typed::<types::Array>() {
                                Ok(v) => match v.await {
                                    Ok(v) => {
                                        let vsource = vsource.clone();
                                        Some(
                                            v.owned()
                                                .0
                                                .into_iter()
                                                .map(move |v| vsource.clone().with(v)),
                                        )
                                    }
                                    Err(e) => {
                                        errs.push(vsource.with(Error::ValueError(e)).into());
                                        None
                                    }
                                },
                                Err(o) => {
                                    let err = source.clone().with(Error::PatternMismatch(o)).into();
                                    errs.push(
                                        vsource.with("value definition").context_for_error(err),
                                    );
                                    None
                                }
                            }
                        }
                        None => None,
                    };
                    match val {
                        None => std::iter::repeat(None).take(inner.len()).collect(),
                        Some(v) => v
                            .enumerate()
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
                            .collect(),
                    }
                };

                match pattern_array(ctx, &inner, &vals).await {
                    Ok(mut n_env) => {
                        env.append(&mut n_env);
                    }
                    Err(n_errs) => {
                        errs.push({
                            let mut err: grease::value::Error = match &orig_val {
                                None => source
                                    .clone()
                                    .with(Error::ValueError("pattern count not be matched".into()))
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
                        match v.binding.typed::<types::Map>() {
                            Ok(v) => match v.await {
                                Ok(v) => Some((orig, v.owned().0)),
                                Err(e) => {
                                    errs.push(vsource.with(Error::ValueError(e)).into());
                                    None
                                }
                            },
                            Err(o) => {
                                let err = source.clone().with(Error::PatternMismatch(o)).into();
                                errs.push(vsource.with("value definition").context_for_error(err));
                                None
                            }
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
                            let result = match val.as_mut() {
                                Some((orig_value, m)) => match m.remove(key.as_str()) {
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
                                        err = isource
                                            .with(format!("missing key '{}'", key))
                                            .context_for_error(err);
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
                            _apply_pattern(ctx, env, errs, pat, result).await;
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
                        ctx,
                        env,
                        errs,
                        rest,
                        val.map(move |(_, m)| {
                            src.with(PatternValues::singular(types::Map(m).into()))
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

fn is_fixed_point(pat: &Pattern) -> bool {
    use Pattern::*;
    match pat {
        Any | Binding(_) => false,
        _ => true,
    }
}

async fn pattern_array(
    ctx: &mut Runtime,
    pats: &[Source<ArrayPattern>],
    vals: &[Option<Source<PatternValues>>],
) -> Result<ScriptEnv, Vec<grease::value::Error>> {
    let mut ret = ScriptEnv::new();
    let mut errs = Vec::new();

    _pattern_array(ctx, &mut ret, &mut errs, pats, vals).await;
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(errs)
    }
}

fn _pattern_array<'a>(
    ctx: &'a mut Runtime,
    env: &'a mut ScriptEnv,
    errs: &'a mut Vec<grease::value::Error>,
    pats: &'a [Source<ArrayPattern>],
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
                        _apply_pattern(ctx, env, errs, p.clone(), v.clone()).await;
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
                        pattern_array_rest(ctx, env, errs, &pats[pati..], &vals[vali..]).await;
                    _apply_pattern(
                        ctx,
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
    ctx: &'a mut Runtime,
    env: &'a mut ScriptEnv,
    errs: &'a mut Vec<grease::value::Error>,
    pats: &'a [Source<ArrayPattern>],
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
                        match pattern_array(ctx, &pats, &vals[vali..]).await {
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
                    match pattern_array_rest(ctx, env, errs, &pats[1..], &vals[..]).await {
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
                                _apply_pattern(ctx, env, errs, p.clone(), Some(val.unwrap())).await;
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
    ctx: &mut Runtime,
    pat: CmdPat,
    mut args: FunctionArguments,
) -> Result<ScriptEnv, Vec<grease::value::Error>> {
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
        let mut err: grease::value::Error = non_pos_args
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
                ctx,
                non_pos_arg,
                Some(kw.into_source().map(|kw| {
                    types::Map(kw.into_iter().map(|(k, v)| (k, v.unwrap())).collect()).into_value()
                })),
            )
            .await
        } else if !kw.is_empty() {
            Err(kw
                .into_iter()
                .map(|(k, v)| {
                    v.with(Error::UnexpectedNonPositionalArgument(k.into_string()))
                        .into()
                })
                .collect())
        } else {
            Ok(ScriptEnv::default())
        };
        let non_pos_bindings = non_pos_bindings.map_err(|e| vec![e]);
        // Merge with positional pattern bindings, if applicable
        let pos_bindings = pattern_array(ctx, &pos_args, &vals).await;
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

        v.map_async(|v| async {
            let v = if env_lookup {
                match v.typed::<types::String>() {
                    Ok(val) => {
                        let s = val.await?.owned();
                        trace!("looking up '{}' in environment", s);
                        // Lookup string in environment, and apply result to remaining arguments
                        match ctx.env_get(&s) {
                            Some(value) => {
                                let value = value?;
                                trace!("found match in environment for '{}': {}", s, value.id());
                                value.clone().unwrap()
                            }
                            None => {
                                return Err(Error::MissingBinding(s.into_string()).into());
                            }
                        }
                    }
                    Err(v) => v,
                }
            } else {
                v
            };

            match_value!(v => {
                types::Function => |val| {
                    let f = val.await?;
                    let f = f.as_ref();
                    let mut fcallctx = FunctionCall::new(ctx, args.into(), v_source);
                    let ret = f.call(&mut fcallctx).await;
                    if ret.is_err() {
                        fcallctx.args.clear();
                    }
                    ret
                },
                => |v| {
                    Err(Error::NonCallableExpression(v).into())
                }
            })
        })
        .await
        .map(|v| v.map_err(|e| e.error()))
        .transpose_err()
        .map_err(|e| e.into())
    }
    .boxed()
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
    pub fn evaluate<'a>(self, ctx: &'a mut Runtime) -> BoxFuture<'a, SoResult<Value>> {
        async move {
        use Expression::*;
        match self.0 {
            Empty => Ok(().into_value().into()),
            Expression::String(s) => Ok(types::String::from(s).into_value().into()),
            Index(v,ind) => {
                // ind should evaluate to a string
                let ind = source_value_as!(Rt(*ind).evaluate(ctx).await?, types::String, ctx)?.await.transpose_ok()?;

                let lookup = |ctx: &mut Runtime, v: grease::value::Ref<types::String>| {
                    let s = v.as_ref();
                    trace!("looking up '{}' in environment", s);
                    match ctx.env_get(s) {
                        None => Err(Error::MissingBinding(s.as_str().into()).into()),
                        Some(value) => {
                            let value = value?;
                            trace!("found match in environment for '{}': {}", s, value.id());
                            Ok(value.clone().unwrap())
                        }
                    }
                };

                match v {
                    None => {
                        // Lookup in environment
                        lookup(ctx, ind.unwrap())
                    },
                    Some(v) => {
                        let v = Rt(*v).evaluate(ctx).await?.unwrap();

                        // If a string, first lookup in environment
                        let v = match v.typed::<types::String>() {
                            Err(v) => v,
                            Ok(s) => lookup(ctx, s.await?)?
                        };

                        match_value!(v => {
                            types::Array => |val| {
                                ind.map_async(|index| async move { match usize::from_str(index.as_ref()) {
                                    Err(_) => Err(Error::NonIntegerIndex.into()),
                                    Ok(ind) => val.await.map(|v| v.0.get(ind).cloned()
                                        .unwrap_or(().into()))
                                }
                                }).await.transpose_err().map_err(|e| e.into_grease_error())
                            },
                            types::Map => |val| {
                                ind.map_async(|index| async move {
                                        val.await.map(|v| v.0.get(index.as_ref()).cloned()
                                            .unwrap_or(().into()))
                                }).await.transpose_err().map_err(|e| e.into_grease_error())
                            },
                            => |v| Err(Error::NonIndexableValue(v).into())
                        })
                    }
                }
            },
            Array(es) => {
                let all_vals = {
                    let mut results = Vec::new();
                    for e in es {
                        results.push(Rt(e).evaluate(ctx).await)
                    }
                    results.collect_result::<Vec<_>>()?
                };
                let mut vals = Vec::new();
                let mut errs = Vec::new();
                for v in all_vals {
                    let (_merge_source, (merge, val)) = v.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match val.typed::<types::Array>() {
                            Ok(val) => vals.extend(val.await?.owned().0),
                            Err(_) => {
                                errs.push(
                                    val_source
                                        .with(Error::CannotMerge("non-array value".into()))
                                        .into(),
                                );
                            }
                        }
                    } else {
                        vals.push(val.unwrap());
                    }
                }

                if errs.is_empty() {
                    Ok(types::Array(vals.into()).into_value())
                } else {
                    Err(grease::value::Error::aggregate(errs))
                }
            }
            Set(pat, e) => {
                let data = Rt(*e).evaluate(ctx).await;
                let (ok_ret, data) = match data {
                    Err(e) => (Err(e.clone()), None),
                    Ok(v) => (Ok(ScriptEnvIntoMap.into_value().into()), Some(v)),
                };
                match apply_pattern(ctx, *pat, data).await {
                    Ok(env) => {
                        ctx.env_extend(env);
                        ok_ret.into()
                    }
                    Err(e) => Err(e),
                }
            }
            Unset(var) => {
                ctx.env_remove(var.as_str());
                Ok(ScriptEnvIntoMap.into_value().into())
            }
            Command(cmd, args) => {
                let f = Rt(*cmd).evaluate(ctx).await?;
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
                                kw_vals.extend(val.await?.owned().0.into_iter().map(|(k,v)| (k.into_string(), val_source.clone().with(v))));
                            },
                            => |_| {
                                errs.push(
                                    val_source.with(Error::CannotMerge("non-array/map value".into())).into(),
                                );
                            }
                        });
                    } else {
                        vals.push(val);
                    }
                }

                if !errs.is_empty() {
                    return Err(grease::value::Error::aggregate(errs));
                }

                apply_value(
                    ctx,
                    f,
                    FunctionArguments::new(vals, kw_vals).unchecked(),
                    true,
                )
                .await
                .map(Source::unwrap) // TODO: don't do this
            }
            Block(_) => panic!("Block expression must be evaluated at a higher level"),
            Function(pat, e) => {
                let env = ctx.env_flatten();
                let mut env_deps = grease::value::Dependencies::default();
                for (k,v) in env.iter() {
                    env_deps += depends![k];
                    if let RResult::ROk(v) = v {
                        env_deps += depends![**v];
                    }
                }
                let deps = depends![^env_deps, pat, e];
                Ok(types::Function::new(move |ctx| {
                    let e = e.clone();
                    let pat = pat.clone();
                    let env = env.clone();
                    async move {
                        let src = e.source();

                        ctx.substituting_env(rvec![env.clone()], |ctx| {
                            async move {
                                let args = std::mem::take(&mut ctx.args);
                                match apply_command_pattern(ctx, pat.clone(), args).await {
                                    Ok(bindings) => {
                                        ctx.env_scoped(bindings, |ctx| {
                                            Rt(e.as_ref().clone()).evaluate(ctx).boxed()
                                        })
                                        .await
                                        .0
                                    }
                                    Err(errs) => Err(grease::value::Error::from(
                                        src.with(Error::ArgumentMismatch),
                                    )
                                    .with_context(grease::value::Error::aggregate(errs))),
                                }
                            }
                            .boxed()
                        })
                        .await
                    }
                    .boxed()
                }, deps)
                .into())
            }
            Match(val, pats) => {
                let val = Rt(*val).evaluate(ctx).await?;

                for (p, e) in pats {
                    if let Ok(env) = apply_pattern(ctx, p, Some(val.clone())).await {
                        return ctx
                            .env_scoped(env, |ctx| Rt(e).evaluate(ctx).boxed())
                            .await
                            .0
                            .map(Source::unwrap); // TODO: don't do this
                    }
                }

                Err(Error::MatchFailed(val.unwrap()).into())
            }
        }
    }.boxed()
    }
}

impl Rt<Source<Expression>> {
    pub async fn evaluate(self, ctx: &mut Runtime) -> EvalResult {
        let (source, expr) = self.0.take();
        if let Expression::Block(es) = expr {
            Rt(source.with(es)).evaluate(ctx).await
        } else {
            Rt(expr).evaluate(ctx).await.map(|v| {
                //v.map(|v| {
                source.clone().with(
                    source
                        .with("while evaluating value returned by this expression")
                        .imbue_error_context(v),
                )
                //})
            })
        }
    }
}

impl Rt<Source<MergeExpression>> {
    pub async fn evaluate(self, ctx: &mut Runtime) -> SoResult<Source<(bool, Source<Value>)>> {
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
                                        let (val_source, val) = val.take();
                                        match val.typed::<types::Map>() {
                                            Ok(val) => match val.await {
                                                Ok(val) => {
                                                    for (k, v) in val.owned().0 {
                                                        ctx.env_insert(
                                                            k.into(),
                                                            Ok(val_source.clone().with(v)),
                                                        );
                                                    }
                                                    Ok(merge_source
                                                        .with(ScriptEnvIntoMap.into_value()))
                                                }
                                                Err(e) => Err(val_source.with(e.error()).into()),
                                            },
                                            Err(_) => Err(val_source
                                                .with(Error::CannotMerge("non-map value".into()))
                                                .into()),
                                        }
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
            val.and_then(|val| {
                let (source, val) = val.take();
                match val.typed::<ScriptEnvIntoMap>() {
                    Ok(_) => env_map.map(|ret| self_source.with(types::Map(ret).into_value())),
                    Err(v) => Ok(source.with(v)),
                }
            })
        }
        .boxed()
    }
}
