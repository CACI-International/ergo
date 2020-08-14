//! Script runtime definitions.
//!
//! The runtime is responsible for evaluating AST expressions, producing values or errors.
//! Importantly, it tracks source locations for values and errors so that when an error occurs,
//! useful error information can be provided.
//!
//! This module defines grease::Plan for expressions, where the output is as described above.

use super::ast::{ArrayPattern, CmdPat, Expression, MapPattern, MergeExpression, Pat, Pattern};
use crate::constants::{
    LOAD_PATH_BINDING, SCRIPT_EXTENSION, SCRIPT_PRELUDE_NAME, SCRIPT_WORKSPACE_NAME,
    WORKING_DIRECTORY_BINDING,
};
use abi_stable::rvec;
use grease::{
    bst::BstMap,
    depends, match_value,
    path::PathBuf,
    runtime::{Context, Plan, SplitInto},
    types::GreaseType,
    value::{IntoValue, Value},
};
use log::trace;
use so_runtime::source::{FileSource, IntoSource, Source};
use so_runtime::Result as SoResult;
use so_runtime::{
    types, ContextEnv, EvalResult, FunctionArguments, FunctionCall, ResultIterator, Runtime,
    ScriptEnv, UncheckedFunctionArguments,
};
use std::collections::BTreeMap;
use std::fmt;
use std::path;
use std::str::FromStr;

/// Script type indicating that the env should be returned as a map.
#[derive(Clone, Copy, Debug, GreaseType, Hash)]
pub struct ScriptEnvIntoMap;

macro_rules! script_value_as {
    ($val:expr , $ty:ty , $err:expr) => {{
        let (source, v) = $val.take();
        match v.typed::<$ty>() {
            Err(_) => Err(grease::value::Error::from(
                source.with(grease::value::Error::from($err).error()),
            )),
            Ok(v) => v.get().map_err(|e| source.with(e.error()).into()),
        }
    }};
}

macro_rules! script_value_sourced_as {
    ($val:expr , $ty:ty , $err:expr) => {{
        let (source, v) = $val.take();
        match v.typed::<$ty>() {
            Err(_) => Err(grease::value::Error::from(
                source.with(grease::value::Error::from($err).error()),
            )),
            Ok(v) => match v.get() {
                Ok(v) => Ok(source.with(v)),
                Err(e) => Err(source.with(e.error()).into()),
            },
        }
    }};
}

pub fn load_script(ctx: &mut Context<FunctionCall>) -> EvalResult {
    let target = ctx.args.peek().ok_or("no load target provided")?;
    let mut source = target.source();

    fn script_path_exists<'a, P: 'a + AsRef<path::Path>>(
        name: P,
        try_add_extension: bool,
    ) -> impl FnMut(&path::Path) -> Option<path::PathBuf> + 'a {
        move |path| {
            if try_add_extension {
                let mut p = name.as_ref().file_name().unwrap().to_owned();
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
                    p = p.join(SCRIPT_WORKSPACE_NAME);
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
            types::String => |v| <&str>::from(v.get()?.as_ref()).into(),
            PathBuf => |v| v.get()?.owned().into(),
            => |_| panic!("unexpected value type")
        });

        load_path = Some(target_path.to_owned());

        // Get current load path
        let loadpath = match ctx.env_get(LOAD_PATH_BINDING) {
            Some(v) => {
                let v = v.map(|r| r.clone()).map_err(|e| e.clone())?;
                let source = v.source();
                script_value_as!(
                    v.clone(),
                    types::Array,
                    format!("'{}' must be an array", LOAD_PATH_BINDING)
                )?
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
                script_value_as!(
                    path,
                    PathBuf,
                    format!("'{}' element must be a path", LOAD_PATH_BINDING)
                )?
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

            let mod_path = script_value_as!(
                ctx.env_get(WORKING_DIRECTORY_BINDING)
                    .expect("working directory unset")
                    .map_err(|e| e.clone())?
                    .clone(),
                PathBuf,
                format!("'{}' must be a path", WORKING_DIRECTORY_BINDING)
            )?;
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
        if !ctx.load_cache.contains_key(&PathBuf::from(p.clone())) {
            // Only load prelude if this is not a workspace.
            let load_prelude = p.file_name().unwrap() != SCRIPT_WORKSPACE_NAME; // unwrap because p must have a final component

            // Exclude path from loading in nested calls.
            // This should be done prior to prelude loading in the case where we are loading the
            // prelude.
            ctx.loading.push(PathBuf::from(p.clone()));

            let mod_path = p.parent().unwrap(); // unwrap because file must exist in some directory

            // Load the prelude. The prelude may not exist (without error), however if it does
            // exist it must evaluate to a map.
            let prelude = if load_prelude {
                ctx.env_insert(
                    WORKING_DIRECTORY_BINDING.into(),
                    Ok(Source::builtin(PathBuf::from(mod_path.to_owned()).into())),
                );
                let prelude_load = <Context<FunctionCall> as SplitInto<Context<Runtime>>>::swap_map(
                    ctx,
                    (
                        FunctionArguments::positional(vec![Source::builtin(
                            types::String::from(SCRIPT_PRELUDE_NAME.to_owned()).into(),
                        )]),
                        ctx.call_site.clone(),
                    ),
                    |ctx| match load_script(ctx) {
                        Ok(v) => Ok(v),
                        Err(e) => {
                            ctx.args.clear();
                            Err(e)
                        }
                    },
                );
                match prelude_load {
                    Err(e) => {
                        fn has_load_failed_error(e: &(dyn std::error::Error + 'static)) -> bool {
                            match e.downcast_ref::<Error>() {
                                Some(e) => {
                                    if let Error::LoadFailed { .. } = e {
                                        true
                                    } else {
                                        false
                                    }
                                }
                                None => e.source().map(has_load_failed_error).unwrap_or(false),
                            }
                        }
                        None
                        /* TODO
                        if has_load_failed_error(e.error_ref()) {
                            None
                        } else {
                            ctx.loading.pop();
                            return Err(e);
                        }
                        */
                    }
                    Ok(v) => Some(
                        v.map(|v| match v.typed::<types::Map>() {
                            Ok(v) => Ok(v.get()?.owned().0),
                            Err(_) => Err(Error::CannotMerge(
                                "prelude did not evaluate to a map".into(),
                            )),
                        })
                        .transpose()?,
                    ),
                }
            } else {
                None
            };

            let mut script = super::Script::load(Source::new(FileSource(p.clone())))?;

            let mod_path: Value = PathBuf::from(mod_path.to_owned()).into();

            // Add initial load path binding first; the prelude may override it.
            let mut top_level_env: ScriptEnv = Default::default();
            top_level_env.insert(
                LOAD_PATH_BINDING.into(),
                Ok(Source::builtin(
                    types::Array(rvec![mod_path.clone()]).into(),
                ))
                .into(),
            );
            if let Some(v) = prelude {
                let (source, v) = v.take();
                top_level_env.extend(
                    v.into_iter()
                        .map(|(k, v)| (k, Ok(source.clone().with(v)).into())),
                );
            }
            // Add mod path binding last; it should not be overriden.
            top_level_env.insert(
                WORKING_DIRECTORY_BINDING.into(),
                Ok(Source::builtin(mod_path)).into(),
            );
            script.top_level_env(top_level_env);
            let result = ctx.split_map(|ctx: &mut Context<Runtime>| script.plan(ctx));
            ctx.load_cache
                .insert(PathBuf::from(p.clone()), result.into());
            ctx.loading.pop();
        }

        let loaded = ctx
            .load_cache
            .get(&PathBuf::from(p))
            .unwrap()
            .clone()
            .into_result()?; // unwrap because prior statement ensures the key exists

        let args = std::mem::take(&mut ctx.args);
        ctx.split_map(move |ctx| {
            apply_value(ctx, source.with(loaded.unwrap()), args.unchecked(), false)
        })
    } else {
        Err(Error::LoadFailed {
            was_loading,
            was_workspace,
            load_path,
        }
        .into())
    }
}

/// Script runtime errors.
#[derive(Debug)]
pub enum Error {
    /// An integer index (for arrays) was expected.
    NonIntegerIndex,
    /// An index was not present.
    MissingIndex(String),
    /// An indexing operation was attempted on a type that is not an array or map.
    InvalidIndex,
    /// An expression is in call-position (had arguments) but is not callable.
    NonCallableExpression(Value),
    /// No binding with the given name is available in the current environment.
    MissingBinding(String),
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
            NonIntegerIndex => write!(f, "positive integer index expected"),
            MissingIndex(s) => write!(f, "index missing: {}", s),
            InvalidIndex => write!(f, "type is not an array or map; cannot index"),
            NonCallableExpression(_) => write!(f, "cannot pass arguments to non-callable value"),
            MissingBinding(s) => write!(f, "'{}' is not available in the current environment", s),
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

/// Apply the value to the given arguments.
///
/// If `env_lookup` is true and `v` is a `ScriptString`, it will be looked up in the environment.
pub fn apply_value(
    ctx: &mut Context<Runtime>,
    v: Source<Value>,
    mut args: UncheckedFunctionArguments,
    env_lookup: bool,
) -> EvalResult {
    let v_source = v.source();

    v.map(|v| {
        let v = if env_lookup {
            match v.typed::<types::String>() {
                Ok(val) => {
                    let s = val.get()?.owned();
                    trace!("looking up '{}' in environment", s);
                    // Lookup string in environment, and apply result to remaining arguments
                    match ctx.inner.env_get(&s) {
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

        if args.peek().is_none() {
            return Ok(v_source.with(v.into()));
        }

        match_value!(v => {
            types::Array => |val| {
                let index = script_value_sourced_as!(args.next().unwrap(), types::String, "index must be a string")?;
                let val = index.map(|index| match usize::from_str(index.as_ref()) {
                    Err(_) => Err(Error::NonIntegerIndex),
                    Ok(ind) => val.get()?.0.get(ind).cloned()
                        .ok_or(Error::MissingIndex(index.owned().into()))
                }).transpose()?;

                let (source,val) = val.take();

                apply_value(ctx, Source::from((v_source,source)).with(val), args, false)
            },
            types::Map => |val| {
                let index = script_value_sourced_as!(args.next().unwrap(), types::String, "index must be a string")?;

                let val = index.map(|index|
                        val.get()?.0.get(index.as_ref()).cloned()
                            .ok_or(Error::MissingIndex(index.owned().into()))
                    )
                    .transpose()?;

                let (source,val) = val.take();

                apply_value(ctx, Source::from((v_source,source)).with(val), args, false)
            },
            types::Function => |val| {
                Rt(val.get()?.as_ref()).plan_join(ctx, (args.into(), v_source))
            },
            => |v| {
                Err(Error::NonCallableExpression(v).into())
            }
        })
    })
    .map(|v| v.map_err(|e| e.error()))
    .transpose_err()
    .map_err(|e| e.into())
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

pub fn apply_pattern(
    ctx: &mut Context<Runtime>,
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
    );
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(grease::value::Error::aggregate(errs))
    }
}

fn _apply_pattern(
    ctx: &mut Context<Runtime>,
    env: &mut ScriptEnv,
    errs: &mut Vec<grease::value::Error>,
    pat: Pat,
    val: Option<Source<PatternValues>>,
) {
    use Pattern::*;
    let (source, pat) = pat.take();
    let desc = std::mem::discriminant(&pat);
    match pat {
        Any => (),
        Literal(e) => match (Rt(e).plan(ctx), val) {
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
            let val = val.and_then(|v| {
                orig_val = Some(v.clone());
                let (vsource, v) = v.take();
                match v.binding.typed::<types::Array>() {
                    Ok(v) => match v.get() {
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
                        errs.push(vsource.with("value definition").context_for_error(err));
                        None
                    }
                }
            });
            let vals: Vec<Option<Source<PatternValues>>> = match val {
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
            };

            match pattern_array(ctx, &inner, &vals) {
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
            let mut val = val.and_then(|v| {
                let orig = v.clone();
                let (vsource, v) = v.take();
                match v.binding.typed::<types::Map>() {
                    Ok(v) => match v.get() {
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
            });
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
                        _apply_pattern(ctx, env, errs, pat, result);
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
                    val.map(move |(_, m)| src.with(PatternValues::singular(types::Map(m).into()))),
                )
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

fn is_fixed_point(pat: &Pattern) -> bool {
    use Pattern::*;
    match pat {
        Any | Binding(_) => false,
        _ => true,
    }
}

fn pattern_array(
    ctx: &mut Context<Runtime>,
    pats: &[Source<ArrayPattern>],
    vals: &[Option<Source<PatternValues>>],
) -> Result<ScriptEnv, Vec<grease::value::Error>> {
    let mut ret = ScriptEnv::new();
    let mut errs = Vec::new();

    _pattern_array(ctx, &mut ret, &mut errs, pats, vals);
    if errs.is_empty() {
        Ok(ret)
    } else {
        Err(errs)
    }
}

fn _pattern_array(
    ctx: &mut Context<Runtime>,
    env: &mut ScriptEnv,
    errs: &mut Vec<grease::value::Error>,
    pats: &[Source<ArrayPattern>],
    vals: &[Option<Source<PatternValues>>],
) {
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
                    _apply_pattern(ctx, env, errs, p.clone(), v.clone());
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
                let rest_end = pattern_array_rest(ctx, env, errs, &pats[pati..], &vals[vali..]);
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
                );
                break;
            }
        }
    }
}

fn pattern_array_rest(
    ctx: &mut Context<Runtime>,
    env: &mut ScriptEnv,
    errs: &mut Vec<grease::value::Error>,
    pats: &[Source<ArrayPattern>],
    vals: &[Option<Source<PatternValues>>],
) -> Option<Vec<Source<PatternValues>>> {
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
                    match pattern_array(ctx, &pats, &vals[vali..]) {
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
                pattern_array_rest(ctx, env, errs, &pats[1..], &vals[..]).and_then(|values| {
                    if values.is_empty() {
                        errs.push(
                            psource
                                .with(Error::ValueError("no value to match with pattern".into()))
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
                        _apply_pattern(ctx, env, errs, p.clone(), Some(val.unwrap()));
                        Some(ret)
                    }
                })
            }
        }
        ArrayPattern::Rest(_) => {
            errs.push(psource.with(Error::PatternArrayRestUndecidable).into());
            None
        }
    }
}

pub fn apply_command_pattern(
    ctx: &mut Context<Runtime>,
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
        let pos_bindings = pattern_array(ctx, &pos_args, &vals);
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

impl Plan<FunctionCall> for Rt<&'_ types::Function> {
    type Output = EvalResult;

    fn plan(self, ctx: &mut Context<FunctionCall>) -> Self::Output {
        let ret = self.call(ctx);
        if ret.is_err() {
            ctx.args.clear();
        }
        ret
    }
}

impl Plan<Runtime> for Rt<Expression> {
    type Output = SoResult<Value>;

    fn plan(self, ctx: &mut Context<Runtime>) -> Self::Output {
        use Expression::*;
        match self.0 {
            Empty => Ok(().into_value().into()),
            Expression::String(s) => Ok(types::String::from(s).into_value().into()),
            Array(es) => {
                let all_vals = es
                    .into_iter()
                    .map(|e| Rt(e).plan(ctx))
                    .collect_result::<Vec<_>>()?;
                let mut vals = Vec::new();
                let mut errs = Vec::new();
                for v in all_vals {
                    let (_merge_source, (merge, val)) = v.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match val.typed::<types::Array>() {
                            Ok(val) => vals.extend(val.get()?.owned().0),
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
                let data = Rt(*e).plan(ctx);
                let (ok_ret, data) = match data {
                    Err(e) => (Err(e.clone()), None),
                    Ok(v) => (Ok(ScriptEnvIntoMap.into_value().into()), Some(v)),
                };
                match apply_pattern(ctx, *pat, data) {
                    Ok(env) => {
                        ctx.env_extend(env);
                        ok_ret.into()
                    }
                    Err(e) => Err(e),
                }
            }
            Unset(var) => {
                ctx.inner.env_remove(var.as_str());
                Ok(ScriptEnvIntoMap.into_value().into())
            }
            Command(cmd, args) => {
                let f = Rt(*cmd).plan(ctx)?;
                let args = args
                    .into_iter()
                    .map(|a| Rt(a).plan(ctx))
                    .collect_result::<Vec<_>>()?;

                let mut errs = Vec::new();
                let mut vals = Vec::new();
                let mut kw_vals = BTreeMap::new();
                for a in args {
                    let (_merge_source, (merge, val)) = a.take();
                    if merge {
                        let (val_source, val) = val.take();
                        match_value!(val => {
                            types::Array => |val| {
                                vals.extend(val.get()?.owned().0.into_iter().map(|v| val_source.clone().with(v)));
                            },
                            types::Map => |val| {
                                kw_vals.extend(val.get()?.owned().0.into_iter().map(|(k,v)| (k.into_string(), val_source.clone().with(v))));
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
                .map(Source::unwrap) // TODO: don't do this
            }
            Block(_) => panic!("Block expression must be evaluated at a higher level"),
            Function(pat, e) => {
                let env = ctx.env_flatten();
                Ok(types::Function::new(move |ctx| {
                    let src = e.source();

                    ctx.substituting_env(rvec![env.clone()], |ctx| {
                        let args = std::mem::take(&mut ctx.args);
                        match ctx.split_map(|ctx| apply_command_pattern(ctx, pat.clone(), args)) {
                            Ok(bindings) => {
                                ctx.env_scoped(bindings, |ctx| {
                                    Rt(e.as_ref().clone()).plan_split(ctx)
                                })
                                .0
                            }
                            Err(errs) => Err(grease::value::Error::from(
                                src.with(Error::ArgumentMismatch),
                            )
                            .with_context(grease::value::Error::aggregate(errs))),
                        }
                    })
                })
                .into_value())
            }
            /*
            If(cond, t, f) => {
                let cond = match cond.plan(ctx) {
                    Eval::Value(v) => v.unwrap(),
                    Eval::Error => return Ok(Eval::Error),
                };

                let to_bool: Option<grease::IntoTyped<bool>> = ctx.traits.get(&cond);
                let cond = match to_bool {
                    Some(t) => {
                        let v = t.into_typed(cond).get()?;
                        *v
                    }
                    None => true,
                };
                Ok(if cond { t.plan(ctx) } else { f.plan(ctx) }.map(Source::unwrap))
            }
            */
            Match(val, pats) => {
                let val = Rt(*val).plan(ctx)?;

                for (p, e) in pats {
                    if let Ok(env) = apply_pattern(ctx, p, Some(val.clone())) {
                        return ctx
                            .env_scoped(env, |ctx| Rt(e).plan(ctx))
                            .0
                            .map(Source::unwrap); // TODO: don't do this
                    }
                }

                Err(Error::MatchFailed(val.unwrap()).into())
            }
        }
    }
}

impl Plan<FunctionCall> for Rt<Source<&'_ types::Function>> {
    type Output = EvalResult;

    fn plan(self, ctx: &mut Context<FunctionCall>) -> Self::Output {
        let (source, f) = self.0.take();
        Rt(f).plan(ctx).map(|v| {
            source.clone().with(
                source
                    .with("while evaluating value returned by this function call")
                    .imbue_error_context(v.unwrap()),
            )
        })
    }
}

impl Plan<Runtime> for Rt<Source<Expression>> {
    type Output = EvalResult;

    fn plan(self, ctx: &mut Context<Runtime>) -> Self::Output {
        let (source, expr) = self.0.take();
        if let Expression::Block(es) = expr {
            Rt(source.with(es)).plan(ctx)
        } else {
            Rt(expr).plan(ctx).map(|v| {
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

impl Plan<Runtime> for Rt<Source<MergeExpression>> {
    type Output = SoResult<Source<(bool, Source<Value>)>>;

    fn plan(self, ctx: &mut Context<Runtime>) -> Self::Output {
        let (source, val) = self.0.take();
        let merge = val.merge;
        Rt(val.expr).plan(ctx).map(move |e| source.with((merge, e)))
    }
}

impl Plan<Runtime> for Rt<Source<Vec<Source<MergeExpression>>>> {
    type Output = EvalResult;

    fn plan(self, ctx: &mut Context<Runtime>) -> Self::Output {
        let (self_source, this) = self.0.take();

        // Evaluate in a new scope
        let (val, scope) = ctx.env_scoped(Default::default(), |ctx| {
            this.into_iter()
                .map(|e| {
                    Rt(e).plan(ctx).map(|merge| {
                        let (merge_source, (merge, val)) = merge.take();
                        if merge {
                            let (val_source, val) = val.take();
                            match val.typed::<types::Map>() {
                                Ok(val) => match val.get() {
                                    Ok(val) => {
                                        for (k, v) in val.owned().0 {
                                            ctx.env_insert(
                                                k.into(),
                                                Ok(val_source.clone().with(v)),
                                            );
                                        }
                                        Ok(merge_source.with(ScriptEnvIntoMap.into_value()))
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
                    })
                })
                .collect_result::<Vec<_>>()?
                .into_iter()
                .last()
                .unwrap_or(Ok(self_source.clone().with(ScriptEnvIntoMap.into_value())))
        });

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
}
