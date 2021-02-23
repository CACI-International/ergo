//! Script runtime definitions.
//!
//! The runtime is responsible for evaluating AST expressions, producing values or errors.
//! Importantly, it tracks source locations for values and errors so that when an error occurs,
//! useful error information can be provided.

use super::ast::{DocCommentPart, Expr, Expression};
use ergo_runtime::error::PatternError;
use ergo_runtime::source::Source;
use ergo_runtime::Result as EResult;
use ergo_runtime::{
    metadata, traits, types, Arguments, ContextExt, EvalResult, ResultIterator, Runtime, ScriptEnv,
    UncheckedArguments,
};
use futures::future::{BoxFuture, FutureExt};
use grease::{
    bst::BstMap,
    depends, match_value,
    types::GreaseType,
    value::{Errored, IntoValue, TypedValue, Value},
};
use log::trace;
use std::collections::BTreeMap;
use std::fmt;

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
    /// A load failed to find a module.
    LoadFailed {
        was_loading: bool,
        load_path: std::path::PathBuf,
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
            LoadFailed {
                was_loading,
                load_path,
            } => match (was_loading,load_path) {
                (true,v) => write!(
                    f,
                    "'{}' resolved to a path that was in the process of loading (circular dependency avoided)", v.display()),
                (false,v) => write!(f, "'{}' failed to load", v.display()),
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
    bind_allows_pattern_errors: bool,
}

impl Default for Evaluator {
    fn default() -> Self {
        Evaluator {
            warn_string_in_env: true,
            warn_compile_missing_get: false,
            bind_allows_pattern_errors: false,
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

    pub fn bind_allows_pattern_errors(&self, bind_allows_pattern_errors: bool) -> Self {
        Evaluator {
            bind_allows_pattern_errors,
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
                                    Ok(v) => {
                                        let v = v?;
                                        if types::Unset::is_unset(&v) {
                                            Get(v_source.with(Compiled(inner)).into())
                                        } else {
                                            Compiled(v.unwrap())
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
                            ctx.env_insert(
                                k.clone(),
                                Ok(v.source().with(types::Unset::new().into())),
                            );
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
                    DocComment(parts, v) => {
                        let mut v = self.compile_env_into(ctx, *v).await?;
                        let new_parts = ctx
                            .env_scoped(|ctx| {
                                async move {
                                    let mut r = Vec::new();
                                    for p in parts {
                                        match p {
                                            DocCommentPart::String(_) => r.push(Ok(p)),
                                            DocCommentPart::Expression(es) => {
                                                let mut new_es = Vec::new();
                                                for e in es {
                                                    new_es
                                                        .push(self.compile_env_into(ctx, e).await);
                                                }
                                                r.push(
                                                    new_es
                                                        .into_iter()
                                                        .collect_result()
                                                        .map(DocCommentPart::Expression),
                                                );
                                            }
                                        }
                                    }
                                    r.into_iter().collect_result()
                                }
                                .boxed()
                            })
                            .await
                            .0?;
                        DocComment(new_parts, v.into())
                    }
                    Compiled(v) => Compiled(v),
                })
            })
            .await
            .map(|s| s.map_err(|e: grease::Error| e))
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
            let mut expr_type: Option<&'static str> = None;
            let warn_string_in_env = self.warn_string_in_env && ctx.lint;
            let bind_allows_pattern_errors = self.bind_allows_pattern_errors;
            self.bind_allows_pattern_errors(false);
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
                        expr_type = Some("array");
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
                        expr_type = Some("map");
                        // Return the env scope as a map, removing any Unset values.
                        self.evaluate_block(ctx, es).await?.1
                            .into_iter()
                            .filter_map(|(k, v)| {
                                if v.as_ref().map(|v| types::Unset::is_unset(v)).unwrap_or(false) {
                                    None
                                } else {
                                    Some(v.map(move |v| (k, v.unwrap())).into_result())
                                }
                            })
                            .collect_result::<BstMap<_, _>>()
                            .map(|ret| types::Map(ret).into_value(ctx))
                    }
                    Block(es) => {
                        expr_type = Some("block");
                        Ok(self.evaluate_block(ctx, es).await?.0.unwrap())
                    },
                    Function(bind, e) => {
                        expr_type = Some("function");
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
                                        traits::bind(ctx, bind, v).await?;
                                        self.evaluate(ctx, e).await
                                    }.boxed()
                                }).await.0.map(Source::unwrap)
                            }
                            .boxed()
                        }, deps, None).into())
                    }
                    Bind(bind, e) => {
                        // evaluate `e` first, so that any bindings in `bind` aren't in the
                        // environment
                        // Don't immediately return an error from e so that we always introduce
                        // bindings from `bind`, whether they are set or not.
                        let result_e = self.evaluate(ctx, *e).await.map_err(PatternError::unwrap);
                        let bind = ctx.env_set_to_current(|ctx| self.evaluate(ctx, *bind)).await?;

                        let result = match result_e {
                            Err(e) => Err(e),
                            Ok(e) => {
                                let bind_result = traits::bind(ctx, bind, e).await;
                                if bind_allows_pattern_errors {
                                    bind_result
                                } else {
                                    bind_result.map_err(PatternError::unwrap)
                                }
                            }
                        };

                        result.and_then(|_| Ok(BindExpr.into_value()))
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
                        let (k_source, k) = self.warn_string_in_env(false).evaluate(ctx, *v).await?.take();
                        ctx.env_insert(k.clone(), Ok(k_source.clone().with(k_source.with("left unset here")
                                .imbue_error_context(types::Unset::new().into()))));
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
                        expr_type = Some("call");
                        let (cmd, args) = self.command_args(ctx, *cmd, args).await?;

                        traits::bind(ctx, cmd, src.with(types::Args { args }.into_value())).await
                            .map(Source::unwrap)
                    }
                    BindCommand(cmd, args) => {
                        expr_type = Some("pattern call");
                        let (cmd, args) = self.command_args(ctx, *cmd, args).await?;

                        traits::bind(ctx, cmd, src.with(types::PatternArgs { args }.into_value())).await
                            .map(Source::unwrap)
                    }
                    IfBind(cond_bind, if_true, if_false) => {
                        ctx.env_scoped(move |ctx| async move {
                            let result = match Errored::ignore(self.bind_allows_pattern_errors(true).evaluate(ctx, *cond_bind)).await {
                                Ok(_) => true,
                                Err(e) => if PatternError::only_pattern_errors(&e) {
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
                        let to_sourced = ctx.into_sourced::<types::Bool>(cond);
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
                            Ok(if c.as_ref().0 {
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
                        expr_type = Some("forced");
                        let val = self.evaluate(ctx, *val).await?.unwrap();

                        // If the value is a dynamic value, get the inner value.
                        // Otherwise return the value by its (shallow) content.
                        match val.dyn_value().await? {
                            Ok(inner) => Ok(inner),
                            Err(val) => ctx.value_by_content(val, false).await
                        }
                    }
                    Merge(val) => {
                        let val = self.evaluate(ctx, *val).await?;

                        Ok(types::Merge(val).into_value())
                    }
                    DocComment(parts, val) => {
                        let mut val = self.evaluate(ctx, *val).await?;
                        let doc_val = val.clone();
                        let doc = ctx.env_scoped(|ctx| async move {
                            // Insert special `self` keyword representing the value being
                            // documented.
                            ctx.env_set_to_current(|ctx| async move {
                                ctx.env_insert(types::String::from("self").into(), Ok(doc_val));
                            }.boxed()).await;
                            let mut doc = std::string::String::new();
                            for p in parts {
                                match p {
                                    DocCommentPart::String(s) => doc.push_str(&s),
                                    DocCommentPart::Expression(es) => {
                                        // Evaluate as a block, displaying the final value and
                                        // merging the scope into the doc comment scope.
                                        let (val, scope) = self.evaluate_block(ctx, es).await?;
                                        ctx.env_set_to_current(|ctx| async move { ctx.env_extend(scope); }.boxed()).await;
                                        // Only add to string if the last value wasn't a bind
                                        // expression (to support using a block to only add
                                        // bindings).
                                        if val.grease_type_immediate() != Some(&BindExpr::grease_type()) {
                                            doc.push_str(&ctx.display(val.unwrap()).await?);
                                        }
                                    }
                                }
                            }
                            Result::<_, grease::Error>::Ok(doc)
                        }.boxed()).await.0?;
                        val.set_metadata(&metadata::Doc, types::String::from(doc).into());
                        Ok(val.unwrap())
                    }
                    Compiled(v) => Ok(v)
                }
            })
            .await
            .transpose_with_context(format!("in {}expression", expr_type.map(|s| format!("{} ", s)).unwrap_or_default()))
            .map(|v| {
                v.map(|v| rsrc
                    .with(format!("while evaluating value returned by {} expression", expr_type.unwrap_or("this")))
                    .imbue_error_context(v))
            })
        }
        .boxed()
    }
}
