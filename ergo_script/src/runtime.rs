//! Script runtime definitions.
//!
//! The runtime is responsible for evaluating AST expressions, producing values or errors.
//! Importantly, it tracks source locations for values and errors so that when an error occurs,
//! useful error information can be provided.

use super::ast::{self, CaptureKey, DocCommentPart, Expr, Expression};
use ergo_runtime::error::PatternError;
use ergo_runtime::source::Source;
use ergo_runtime::Result as EResult;
use ergo_runtime::{
    metadata, traits, types, Arguments, ContextExt, EvalResult, EvalResultAbi, ResultIterator,
    Runtime, ScriptEnv,
};
use futures::future::FutureExt;
use grease::{
    depends, make_value, match_value,
    types::GreaseType,
    value::{AnyValue, Errored, IntoValue, TypedValue, Value},
};
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

#[derive(Clone, Debug)]
pub enum Capture {
    Expr(Expression),
    Evaluated(Source<Value>),
}

impl From<Expression> for Capture {
    fn from(e: Expression) -> Self {
        Capture::Expr(e)
    }
}

impl From<Source<Value>> for Capture {
    fn from(res: Source<Value>) -> Self {
        Capture::Evaluated(res)
    }
}

impl From<&Capture> for grease::value::Dependency {
    fn from(capture: &Capture) -> Self {
        match capture {
            Capture::Expr(e) => e.into(),
            Capture::Evaluated(v) => v.as_ref().into(),
        }
    }
}

impl From<&Capture> for grease::value::Dependencies {
    fn from(capture: &Capture) -> Self {
        depends![grease::value::Dependency::from(capture)]
    }
}

// The use of a BTreeMap is important so that dependencies can incorporate the relative capture key
// order such that dependencies that swap values will end up differently (without depending on the
// capture key values themselves, which may change from unrelated code).
pub type Captures = BTreeMap<CaptureKey, Capture>;

fn capture_depends(captures: &Captures) -> grease::value::Dependencies {
    depends![^captures.values()]
}

fn capture_subset(captures: &Captures, subset: &ast::CaptureSet) -> Captures {
    let mut ret = Captures::new();
    let mut to_check: Vec<_> = captures
        .keys()
        .filter(|k| subset.contains(**k))
        .copied()
        .collect();
    while let Some(k) = to_check.pop() {
        if !ret.contains_key(&k) {
            if let Some(v) = captures.get(&k) {
                ret.insert(k.clone(), v.clone());
                if let Capture::Expr(e) = v {
                    if let Some(set) = e.captures() {
                        to_check.extend(set.iter());
                    }
                }
            }
        }
    }
    ret
}

/*
/// Script type for errors.
#[derive(Clone, Copy, Debug, GreaseType, Hash, abi_stable::StableAbi)]
#[repr(C)]
pub struct EvalError(pub grease::Error);

impl From<EvalError> for TypedValue<EvalError> {
    fn from(v: EvalError) -> Self {
        TypedValue::constant(v)
    }
}

grease::grease_traits_fn! {
    ergo_runtime::grease_type_name!(traits, EvalError, "error");
    ergo_runtime::traits::ValueByContent::add_impl::<EvalError>(traits);
}
*/

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

    pub fn fill_unbound(
        &self,
        ctx: &mut Runtime,
        captures: &mut Captures,
    ) -> Result<(), grease::Error> {
        for (_, c) in captures.iter_mut() {
            if let Capture::Expr(e) = c {
                // Gets of strings must match up with the global environment
                if let Some(get) = e.as_ref::<ast::Get>() {
                    if let Some(s) = (*get.value).as_ref::<ast::String>() {
                        match ctx.env_get(&types::String::from(s.0.as_str()).into_value()) {
                            Some(r) => *c = Capture::Evaluated(r?),
                            None => {
                                return Err(get
                                    .value
                                    .source()
                                    .with("unbound value")
                                    .into_grease_error())
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub async fn eval_captures(&self, ctx: &mut Runtime, captures: &mut Captures) -> EResult<()> {
        // TODO improve efficiency of completed capture detection
        let mut completed_captures = ast::CaptureSet::default();
        for (k, c) in captures.iter() {
            if let Capture::Evaluated(_) = c {
                completed_captures.insert(*k);
            }
        }

        loop {
            let mut changes = Vec::new();
            for (k, c) in captures.iter() {
                if let Capture::Expr(e) = c {
                    let evaluate = e
                        .captures()
                        .map(|caps| completed_captures.contains_set(caps))
                        .unwrap_or(true);
                    if evaluate {
                        let v = self.evaluate(ctx, Source::builtin(e.clone()), captures);
                        changes.push((
                            *k,
                            v.map_async(|v| async move {
                                EResult::Ok(match v.dyn_value().await? {
                                    Ok(v) => v,
                                    Err(v) => v,
                                })
                            })
                            .await
                            .transpose_ok()?,
                        ));
                    }
                }
            }
            if changes.is_empty() {
                break;
            } else {
                for (k, c) in changes {
                    completed_captures.insert(k);
                    captures.insert(k, Capture::Evaluated(c));
                }
            }
        }
        Ok(())
    }

    /// Evaluate block items.
    ///
    /// Returns the vector of normal expressions in the block, the env formed from all bindings
    /// within the block, and whether the block ended with a normal expression.
    async fn evaluate_block_items(
        self,
        ctx: &mut Runtime,
        items: Vec<ast::BlockItem>,
        mut captures: Captures,
        mutually_exclusive: bool,
    ) -> EResult<(Vec<Source<Value>>, ScriptEnv, bool)> {
        let (res,scope_env) = ctx.env_scoped(|ctx| async move {
            let mut results = Vec::new();
            let mut exclusive_env = ScriptEnv::new();
            let mut last_normal = false;
            let mut had_unbound = false;
            for i in items {
                match i {
                    ast::BlockItem::Expr(e) => {
                        last_normal = true;
                        results.push(Ok(self.evaluate(ctx, e, &captures)));
                    }
                    ast::BlockItem::Merge(e) => {
                        let (val_source, val) = self.evaluate(ctx, e, &captures).take();
                        match_value!(val => {
                            types::Array => |val| {
                                match val.await {
                                    Err(e) => results.push(Err(val_source.with("while merging array").context_for_error(e))),
                                    Ok(arr) => {
                                        let arr = arr.owned().0;
                                        if !arr.is_empty() {
                                            last_normal = true;
                                        }
                                        results.extend(arr.into_iter().map(|v| Ok(val_source.clone().with(v))));
                                    }
                                }
                            },
                            types::Map => |val| {
                                last_normal = false;
                                match val.await {
                                    Err(e) => results.push(Err(val_source.with("while merging map").context_for_error(e))),
                                    Ok(map) => {
                                        let items = map.owned().0.into_iter().map(move |(k,v)| (k, Ok(val_source.clone().with(v)).into()));
                                        if mutually_exclusive {
                                            exclusive_env.extend(items.map(|(k,v)| (k,(v,None.into()).into())));
                                        } else {
                                            ctx.env_set_to_current(move |ctx| async move {
                                                ctx.env_extend(items);
                                            }.boxed()).await;
                                        }
                                    }
                                }
                            },
                            types::Unbound => |ub| {
                                last_normal = false;
                                if mutually_exclusive {
                                    results.push(Ok(val_source.clone().with(types::Merge(val_source.with(ub.into())).into_value())));
                                } else {
                                    if had_unbound {
                                        results.push(Err(val_source.with("cannot have multiple unbound merges").into_grease_error()));
                                    } else {
                                        ctx.env_set_to_current(|ctx| async move {
                                            ctx.env_insert(types::BindRest.into_value(), Ok(val_source.with(ub.into())), None);
                                        }.boxed()).await;
                                        had_unbound = true;
                                    }
                                }
                            }
                            => |v| {
                                let name = ctx.type_name(v.grease_type_immediate().unwrap()).await?;
                                results.push(Err(val_source.with(format!("cannot merge value with type {}", name)).into_grease_error()));
                            }
                        }).await?;
                    }
                    ast::BlockItem::Bind(b,e) => {
                        last_normal = false;
                        let e = self.evaluate(ctx, e, &captures);
                        let caps = captures.clone();
                        let immediate_bind = b.expr_type() == ast::ExpressionType::Set;
                        let (b, new_env) = ctx.env_set_gather(move |ctx| async move {
                            let v = self.evaluate(ctx, b, &caps);
                            v.map_async(|v| ctx.value_by_content(v, true)).await.transpose_ok()
                        }.boxed()).await;
                        let b = match b {
                            Ok(b) => b,
                            Err(b) => {
                                results.push(Err(b));
                                continue;
                            }
                        };

                        // Create proxy entries that will perform the bind.
                        let bind_deps = depends![*b,*e];
                        let to_bind = match traits::delay_bind(ctx, b).await {
                            Ok(v) => v.bind_owned(e).shared(),
                            Err(e) => {
                                results.push(Err(e));
                                continue;
                            }
                        };
                        let mut env_extend = Vec::new();
                        let mut captures_updated = false;
                        for (k,v) in new_env {
                            let (v, cap) = v.into_tuple();
                            let deps = depends![^bind_deps.clone(), k];
                            let to_bind = to_bind.clone();
                            let v = match v {
                                EvalResultAbi::RErr(e) => EvalResultAbi::RErr(e),
                                EvalResultAbi::ROk(v) =>  EvalResultAbi::ROk(if immediate_bind {
                                    to_bind.await?;
                                    v.map_async(|v| async { v.dyn_value().await.map(|v| v.expect("immediate bind value was not dynamic")) })
                                        .await
                                        .transpose_ok()?
                                } else {
                                    v.map(|v| Value::dyn_new(async move {
                                        to_bind.await?;
                                        Ok(v.into_any_value())
                                    }, deps))
                                })
                            };

                            env_extend.push((k,(v.clone().into(),cap).into()));
                            if let Some(cap) = cap.into_option() {
                                captures.insert(ast::keyset::Key(cap), Capture::Evaluated(v.into_result()?));
                                captures_updated = true;
                            }
                        }
                        if captures_updated {
                            self.eval_captures(ctx, &mut captures).await?;
                        }
                        if mutually_exclusive {
                            exclusive_env.extend(env_extend);
                        } else {
                            ctx.env_set_to_current(move |ctx| async move {
                                ctx.env_extend_captures(env_extend);
                            }.boxed()).await
                        }
                    }
                }
            }
            Result::<_,grease::Error>::Ok((results,exclusive_env,last_normal))
        }.boxed()).await;

        let (results, exclusive_env, last_normal) = res?;

        Ok((
            results.collect_result()?,
            if mutually_exclusive {
                exclusive_env
            } else {
                scope_env
            },
            last_normal,
        ))
    }

    fn delayed_evaluate<E: ast::IsExpression, F, Fut>(
        self,
        ctx: &mut Runtime,
        e: Expression,
        captures: &Captures,
        f: F,
    ) -> Value
    where
        F: FnOnce(Runtime, &E, Captures) -> Fut,
        Fut: std::future::Future<Output = EResult<AnyValue>> + Send + 'static,
    {
        let captures = e
            .captures()
            .map(|caps| capture_subset(captures, caps))
            .unwrap_or_default();
        let deps = depends![e, ^capture_depends(&captures)];
        Value::dyn_new(
            ctx.task.spawn(f(
                ctx.empty(),
                e.as_ref::<E>().expect("incorrect type in delayed evaluate"),
                captures,
            )),
            deps,
        )
    }

    pub fn evaluate(self, ctx: &mut Runtime, e: Expr, captures: &Captures) -> Source<Value> {
        macro_rules! command_impl {
            ( $cmd:expr, $ast_tp:ident, $arg:ident ) => {
                self.delayed_evaluate(
                    ctx,
                    $cmd,
                    captures,
                    |mut ctx, cmd: &ast::$ast_tp, captures| {
                        let function = cmd.function.clone();
                        let args = cmd.args.clone();
                        async move {
                            let src = function.source();
                            let function = self.evaluate(&mut ctx, function, &captures);
                            let (pos, keyed, _) = self
                                .evaluate_block_items(&mut ctx, args, captures, true)
                                .await?;
                            let keyed = keyed
                                .into_iter()
                                .map(|(k, v)| Ok((src.clone().with(k), v.0.into_result()?)))
                                .collect_result()?;
                            let args = Arguments::new(pos, keyed).unchecked();
                            traits::bind(
                                &mut ctx,
                                function,
                                src.with(types::$arg { args }.into_value()),
                            )
                            .await
                            .map(|src_v| src_v.unwrap().into_any_value())
                        }
                    },
                )
            };
        }

        let (source, e) = e.take();

        crate::match_expression!(e,
            Unit => |_| source.with(types::Unit.into_value()),
            BindAny => |_| source.with(types::Unbound::new(
                            |_, _| async { Ok(types::Unit.into_value()) }.boxed(),
                            depends![ergo_runtime::namespace_id!(ergo::any)],
                            (),
                        )
                        .into()),
            String => |s| source.with(types::String::from(s.0.clone()).into_value()),
            Array => |_| source.with(self.delayed_evaluate(ctx, e, captures, |mut ctx, arr: &ast::Array, captures| {
                    let items = arr.items.clone();
                    async move {
                        let mut results = Vec::new();
                        for i in items {
                            match i {
                                ast::ArrayItem::Expr(e) => {
                                    results.push(Ok(self.evaluate(&mut ctx, e, &captures)));
                                }
                                ast::ArrayItem::Merge(e) => {
                                    let (val_source, val) = self.evaluate(&mut ctx, e, &captures).take();
                                    match_value!(val => {
                                        types::Array => |val| {
                                            match val.await {
                                                Err(e) => results.push(Err(val_source.with("while merging array").context_for_error(e))),
                                                Ok(arr) => results.extend(arr.owned().0.into_iter().map(|v| Ok(val_source.clone().with(v)))),
                                            }
                                        },
                                        types::Unbound => |ub| {
                                            results.push(Ok(val_source.clone().with(types::Merge(val_source.with(ub.into())).into_value())));
                                        }
                                        => |v| {
                                            let name = ctx.type_name(v.grease_type_immediate().unwrap()).await?;
                                            results.push(Err(val_source.with(format!("cannot merge value with type {}", name)).into_grease_error()));
                                        }
                                    }).await?;
                                }
                            }
                        }
                        Ok(
                            types::Array(
                                results.into_iter()
                                    .map(|v| v.map(Source::unwrap))
                                    .collect_result::<Vec<_>>()?
                                    .into()
                            ).into_any_value()
                        )
                    }
                })),
            Block => |_| source.with(self.delayed_evaluate(ctx, e, captures, |mut ctx, block: &ast::Block, captures| {
                    let items = block.items.clone();
                    async move {
                        let (mut vals, ret, sequence) = self.evaluate_block_items(&mut ctx, items, captures, false).await?;
                        let last = if sequence { vals.pop() } else { None };
                        for v in vals {
                            ctx.force_value_nested(v.unwrap()).await?;
                        }
                        if sequence {
                            Ok(last.unwrap().unwrap().into_any_value())
                        } else {
                            Ok(types::Map(ret.into_iter()
                                    .map(|(k,v)| Ok((k,v.0.into_result()?.unwrap())))
                                    .collect_result()?
                                ).into_any_value())
                        }
                    }
                })),
            Function => |_| {
                let captures = e
                    .captures()
                    .map(|caps| capture_subset(captures, caps))
                    .unwrap_or_default();
                let deps = depends![e, ^capture_depends(&captures)];
                source.with(types::Unbound::new(move |ctx, v| {
                    let func = e.as_ref::<ast::Function>().unwrap();
                    let bind = func.bind.clone();
                    let body = func.body.clone();
                    let captures = captures.clone();
                    async move {
                        ctx.env_scoped(move |ctx| {
                            async move {
                                let caps2 = captures.clone();
                                ctx.env_set_to_current(move |ctx| async move {
                                    let bind = self.evaluate(ctx, bind, &captures);
                                    traits::bind(ctx, bind, v).await?.unwrap().into_non_dyn().await
                                }.boxed()).await?;

                                let mut captures = caps2;
                                let mut captures_updated = false;
                                // Update captures
                                ctx.env_current(|env| {
                                    for (k,v) in env.iter() {
                                        let cap = v.1;
                                        let v = &v.0;
                                        if let Some(cap) = cap.into_option() {
                                            captures.insert(ast::keyset::Key(cap), Capture::Evaluated(v.clone().into_result().unwrap()));
                                            captures_updated = true;
                                        }
                                    }
                                });
                                if captures_updated {
                                    self.eval_captures(ctx, &mut captures).await?;
                                }

                                Ok(self.evaluate(ctx, body, &captures))
                            }.boxed()
                        }).await.0.map(Source::unwrap)
                    }.boxed()
                }, deps, ()).into())
            },
            Get => |_| {
                let mut ctx_clone = ctx.clone();
                source.with(self.delayed_evaluate(ctx, e, captures, |_, get: &ast::Get, captures| {
                    let value = get.value.clone();
                    async move {
                        let k = self.warn_string_in_env(false).evaluate(&mut ctx_clone, value, &captures).unwrap();
                        match ctx_clone.env_get(&k) {
                            None => Err(Error::MissingBinding(k).into()),
                            Some(value) => value.map(|src_v| src_v.unwrap().into_any_value())
                        }
                    }
                }))
            },
            Set => |set| {
                let captures = capture_subset(captures, &set.captures);
                let (k_source, k) = self.warn_string_in_env(false).evaluate(ctx, set.value.clone(), &captures).take();
                let (send_result, mut receive_result) = futures::channel::oneshot::channel::<Value>();
                ctx.env_insert(k.clone(), Ok(k_source.clone().with(Value::dyn_new(async move {
                    Ok(match receive_result.await {
                        Ok(v) => v.into_any_value(),
                        Err(_) => {
                            k_source.with("left unset here")
                                .imbue_error_context(types::Unset::new().into()).into_any_value()
                        }
                    })
                }, depends![ergo_runtime::namespace_id!(ergo::get)]))), set.capture_key.map(|v| v.0));
                let send_result = std::sync::Mutex::new(Some(send_result));
                source.with(types::Unbound::new(move |_, v| {
                    let send_result = send_result.lock().map(|mut g| g.take()).unwrap_or(None);
                    async move {
                        if let Some(sender) = send_result {
                            sender.send(v.unwrap());
                            Ok(types::Unit.into_value())
                        } else {
                            Err(v.source().with("cannot bind a setter more than once").into_grease_error())
                        }
                    }.boxed()
                }, depends![ergo_runtime::namespace_id!(ergo::set)], ()).into())
            },
            Index => |_| source.with(self.delayed_evaluate(ctx, e, captures, |mut ctx, ind: &ast::Index, captures| {
                let value = ind.value.clone();
                let index = ind.index.clone();
                async move {
                    let value = self.evaluate(&mut ctx, value, &captures);
                    let index = self.evaluate(&mut ctx, index, &captures);
                    traits::bind(&mut ctx, value, index.source().with(types::Index(index).into_value())).await
                        .map(|src_v| src_v.unwrap().into_any_value())
                }
            })),
            Command => |_| {
                source.with(command_impl!(e, Command, Args))
            },
            PatternCommand => |_| {
                source.with(command_impl!(e, PatternCommand, PatternArgs))
            },
            Force => |_| {
                panic!("unexpected force expression");
            },
            DocComment => |doc| {
                let captures = capture_subset(captures, &doc.captures);
                let mut val = self.evaluate(ctx, doc.value.clone(), &captures);
                let parts = doc.parts.clone();
                let self_key = doc.self_capture_key.map(|v| v.0);
                let deps = DocCommentPart::dependencies(&parts);
                let doc = types::Unbound::new(move |ctx, v| {
                    let parts = parts.clone();
                    let task = ctx.task.clone();
                    let mut ctx = ctx.empty();
                    let mut captures = captures.clone();
                    task.spawn(async move {
                        ctx.env_scoped(|ctx| async move {
                            // Insert special `self` keyword representing the value being
                            // documented.
                            captures.insert(ast::keyset::Key(self_key.unwrap()), Capture::Evaluated(v.clone()));
                            self.eval_captures(ctx, &mut captures).await?;
                            ctx.env_set_to_current(|ctx| async move {
                                ctx.env_insert(types::String::from("self").into(), Ok(v), self_key);
                            }.boxed()).await;
                            let mut doc = std::string::String::new();
                            let mut formatter = traits::Formatter::new(&mut doc);
                            for p in parts {
                                match p {
                                    DocCommentPart::String(s) => formatter.write_str(&s)?,
                                    DocCommentPart::ExpressionBlock(es) => {
                                        // Evaluate as a block, displaying the final value and
                                        // merging the scope into the doc comment scope.
                                        let (mut vals, scope, has_val) = self.evaluate_block_items(ctx, es, captures.clone(), false).await?;

                                        let last = if has_val { vals.pop() } else { None };
                                        for v in vals {
                                            ctx.force_value_nested(v.unwrap()).await?;
                                        }

                                        let mut captures_updated = false;
                                        for (_,v) in &scope {
                                            if let Some(cap) = v.1.into_option() {
                                                captures.insert(ast::keyset::Key(cap), Capture::Evaluated(v.0.clone().into_result()?));
                                                captures_updated = true;
                                            }
                                        }
                                        if captures_updated {
                                            self.eval_captures(ctx, &mut captures).await?;
                                        }

                                        ctx.env_set_to_current(|ctx| async move { ctx.env_extend(scope.into_iter().map(|(k,v)| (k,v.0))); }.boxed()).await;
                                        // Only add to string if the last value wasn't a bind
                                        // expression (to support using a block to only add
                                        // bindings).
                                        if has_val {
                                            ctx.display(last.unwrap().unwrap(), &mut formatter).await?;
                                        }
                                    }
                                }
                            }
                            drop(formatter);
                            Ok(types::String::from(doc).into())
                        }.boxed()).await.0
                    }).boxed()
                }, deps, ());
                val.set_metadata(&metadata::Doc, doc);
                val
            },
            Capture => |capture| {
                match captures.get(&capture.0).expect("internal capture error") {
                    Capture::Evaluated(e) => e.clone(),
                    Capture::Expr(e) => panic!("unevaluated capture"),
                }
            },
        )
    }

    /*
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
                            (),
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
                                    ctx.env_set_to_current(|ctx| {
                                        async move {
                                            ctx.env_insert(
                                                types::String::from("self").into(),
                                                Ok(Source::builtin(types::Unset::new().into())),
                                            );
                                        }
                                        .boxed()
                                    })
                                    .await;

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
        let mut bctx = ctx.clone();
        ctx.task.spawn(
        async move {
            let ctx = &mut bctx;
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
                            depends![ergo_runtime::namespace_id!(ergo::any)], ()).into())
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
                            .map(|ret| types::Map(ret).into())
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
                        }, deps, ()).into())
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
                        }, depends![ergo_runtime::namespace_id!(ergo::set)], ()).into())
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
                        }, deps, ()).into())
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
                                    None => Ok(types::Unset::new().into())
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
                                    None => Value::from(types::Unset::new()).into_any_value()
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
                        let (parts, val) = if let DocComment(parts, val) = self.compile_env_into(ctx, src.with(DocComment(parts, val))).await?.unwrap() {
                            (parts, val)
                        } else {
                            panic!("compile_env_into returned a different expression from a doc comment");
                        };
                        let deps = DocCommentPart::dependencies(&parts);
                        let doc = types::Unbound::new(move |ctx, v| {
                            let parts = parts.clone();
                            let mut ctx = ctx.empty();
                            async move {
                                ctx.env_scoped(|ctx| async move {
                                    // Insert special `self` keyword representing the value being
                                    // documented.
                                    ctx.env_set_to_current(|ctx| async move {
                                        ctx.env_insert(types::String::from("self").into(), Ok(v));
                                    }.boxed()).await;
                                    let mut doc = std::string::String::new();
                                    let mut formatter = traits::Formatter::new(&mut doc);
                                    for p in parts {
                                        match p {
                                            DocCommentPart::String(s) => formatter.write_str(&s)?,
                                            DocCommentPart::Expression(es) => {
                                                // Evaluate as a block, displaying the final value and
                                                // merging the scope into the doc comment scope.
                                                let (val, scope) = self.evaluate_block(ctx, es).await?;
                                                ctx.env_set_to_current(|ctx| async move { ctx.env_extend(scope); }.boxed()).await;
                                                // Only add to string if the last value wasn't a bind
                                                // expression (to support using a block to only add
                                                // bindings).
                                                if val.grease_type_immediate() != Some(&BindExpr::grease_type()) {
                                                    ctx.display(val.unwrap(), &mut formatter).await?;
                                                }
                                            }
                                        }
                                    }
                                    drop(formatter);
                                    Ok(types::String::from(doc).into())
                                }.boxed()).await.0
                            }.boxed()
                        }, deps, ());
                        let mut val = self.evaluate(ctx, *val).await?.unwrap();
                        val.set_metadata(&metadata::Doc, doc);
                        Ok(val)
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
        })
        .boxed()
    }
    */
}
