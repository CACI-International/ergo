//! Script runtime definitions.

use super::ast::{Expression, IntoSource, Source};
use grease::future::{BoxFuture, FutureExt};
use grease::{make_value, match_value, IntoValue, Plan, SplitInto, Value};
use log::trace;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::str::FromStr;

#[path = "runtime/do.rs"]
pub mod do_;
pub mod exec;
pub mod has;
pub mod load;
pub mod map;
pub mod path;
pub mod track;

pub mod script_types {
    use super::super::ast::{Expression, Source};
    use grease::{depends, GetValueType, TypedValue, Value};
    use std::collections::BTreeMap;
    use std::fmt;

    /// Script unit type.
    pub type ScriptUnit = ();

    /// Script string type.
    pub type ScriptString = String;

    /// Script array type.
    #[derive(Clone, Debug, GetValueType, PartialEq)]
    pub struct ScriptArray(pub Vec<Source<Value>>);

    impl From<ScriptArray> for TypedValue<ScriptArray> {
        fn from(v: ScriptArray) -> Self {
            let deps = depends![join v.0.iter().map(|s| s.as_ref())];
            Self::constant_deps(v, deps)
        }
    }

    /// Script map type.
    #[derive(Clone, Debug, GetValueType, PartialEq)]
    pub struct ScriptMap(pub BTreeMap<String, Source<Value>>);

    impl From<ScriptMap> for TypedValue<ScriptMap> {
        fn from(v: ScriptMap) -> Self {
            let deps =
                v.0.iter()
                    .map(|(a, v)| depends![*a, **v])
                    .flatten()
                    .collect::<Vec<_>>();
            Self::constant_deps(v, deps)
        }
    }

    /// Script function type.
    #[derive(GetValueType)]
    pub enum ScriptFunction {
        UserFunction(Source<Expression>, BTreeMap<String, Source<Value>>),
        BuiltinFunction(Box<Builtin>),
    }

    type Builtin = dyn Fn(&mut grease::Context<super::FunctionContext>) -> Result<Value, super::EvalError>
        + Send
        + Sync;

    impl std::hash::Hash for ScriptFunction {
        fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
            match self {
                Self::UserFunction(s, _) => s.hash(h),
                Self::BuiltinFunction(b) => std::ptr::hash(b, h),
            }
        }
    }

    impl fmt::Debug for ScriptFunction {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::UserFunction(e, _) => write!(f, "UserFunction: {:?}", e),
                Self::BuiltinFunction(_) => write!(f, "BuiltinFunction"),
            }
        }
    }
}

pub mod builtin_function_prelude {
    pub use super::script_types::*;
    pub use super::{EvalError, FunctionContext};
    pub use grease::{Context, Value};

    #[macro_export]
    macro_rules! def_builtin {
        ( $ctx:ident , $args:ident => $body:expr ) => {
            pub fn builtin() -> ::grease::Value {
                $crate::script::runtime::ScriptFunction::BuiltinFunction(Box::new(builtin_impl))
                    .into()
            }

            fn builtin_impl($ctx: &mut Context<FunctionContext>) -> Result<Value, EvalError> {
                let mut $args = Vec::new();
                std::mem::swap(&mut $args, &mut $ctx.inner.args);

                $body
            }
        };
    }

    pub use crate::def_builtin;
}

use script_types::*;

fn get_values<'a>(
    v: Source<Value>,
    set: &'a mut BTreeSet<Source<Value>>,
) -> BoxFuture<'a, Result<(), Source<String>>> {
    async move {
        let (source, v) = v.take();

        let res = async {
            match_value!(v => {
                ScriptMap => |val| {
                    let ScriptMap(mut m) = val.await.map_err(Err)?.owned();
                    if let Some(v) = m.remove("*") {
                        get_values(v, set).await.map_err(Ok)?;
                    } else {
                        for (_,v) in m {
                            get_values(v, set).await.map_err(Ok)?;
                        }
                    }
                    Ok(None)
                },
                ScriptArray => |val| {
                    let ScriptArray(arr) = val.await.map_err(Err)?.owned();
                    for v in arr {
                        get_values(v, set).await.map_err(Ok)?;
                    }
                    Ok(None)
                },
                => |v| {
                    Ok(Some(v))
                }
            })
        }
        .await;
        source
            .with(res)
            .transpose()
            .map_err(
                |e: Source<Result<Source<String>, String>>| match e.transpose_err() {
                    Ok(e) => e,
                    Err(e) => e,
                },
            )
            .map(|v| {
                let (source, v) = v.take();
                if let Some(v) = v {
                    set.insert(source.with(v));
                }
                ()
            })
    }
    .boxed()
}

/// Create a value which deeply evaluates all script types within the given value.
///
/// The returned value will be unit-typed.
pub fn script_deep_eval(v: Source<Value>) -> Source<Value> {
    let (source, v) = v.take();
    let s2 = source.clone();
    source.with(
        make_value!([v] {
            let mut set = Default::default();
            get_values(s2.with(v), &mut set).await.map_err(|e| e.to_string())?;
            grease::future::join_all(set).await.into_iter()
                .map(|e| e.transpose_err())
                .collect::<Result<Vec<_>,_>>()
                .map(|_| ())
                .map_err(|e| e.to_string())
        })
        .into(),
    )
}

/// The script context.
pub struct Context {
    env: Vec<BTreeMap<String, Source<Value>>>,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            // The default context should start with an empty map as the base.
            env: vec![Default::default()],
        }
    }
}

impl Context {
    pub fn env_insert(&mut self, k: String, v: Source<Value>) {
        self.env.last_mut().unwrap().insert(k, v);
    }

    pub fn env_remove<Q: ?Sized>(&mut self, k: &Q) -> Option<Source<Value>>
    where
        String: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env.last_mut().unwrap().remove(k)
    }

    pub fn env_get<Q: ?Sized>(&mut self, k: &Q) -> Option<&Source<Value>>
    where
        String: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env.iter().rev().find_map(|m| m.get(k))
    }

    pub fn env_flatten(&self) -> BTreeMap<String, Source<Value>> {
        self.env.iter().fold(Default::default(), |mut e, m| {
            e.append(&mut m.clone());
            e
        })
    }
}

impl From<BTreeMap<String, Source<Value>>> for Context {
    fn from(v: BTreeMap<String, Source<Value>>) -> Self {
        Context { env: vec![v] }
    }
}

/// Function call context.
pub struct FunctionContext {
    ctx: Context,
    args: Vec<Source<Value>>,
}

impl std::ops::Deref for FunctionContext {
    type Target = Context;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl std::ops::DerefMut for FunctionContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl grease::SplitInto<Context> for FunctionContext {
    type Extra = Vec<Source<Value>>;

    fn split(self) -> (Context, Self::Extra) {
        (self.ctx, self.args)
    }

    fn join(ctx: Context, args: Self::Extra) -> Self {
        FunctionContext { ctx, args }
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
    /// An error occured while evaluating a value.
    ValueError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            NonIntegerIndex => write!(f, "positive integer index expected"),
            MissingIndex(s) => write!(f, "index missing: {}", s),
            InvalidIndex => write!(f, "type is not an array or map; cannot index"),
            NonCallableExpression(d) => {
                write!(f, "cannot pass arguments to non-callable value {:?}", d)
            }
            MissingBinding(s) => write!(f, "'{}' is not available in the current environment", s),
            ValueError(s) => write!(f, "{}", s),
        }
    }
}

impl Plan<FunctionContext> for &'_ ScriptFunction {
    type Output = Result<Value, EvalError>;

    fn plan(self, ctx: &mut grease::Context<FunctionContext>) -> Self::Output {
        match self {
            ScriptFunction::UserFunction(e, env) => {
                // A FunctionContext is only used once, so swap out the arguments.
                let mut args = Vec::new();
                std::mem::swap(&mut ctx.inner.args, &mut args);

                let args = args
                    .into_source()
                    .map(|args| ScriptArray(args).into_value());

                let mut nctx: Context = env.clone().into();
                nctx.env_insert("@".to_owned(), args);
                let v =
                    ctx.split_map(|ctx: &mut grease::Context<()>| e.clone().plan_join(ctx, nctx));
                v.map(Source::unwrap).map_err(EvalError::Nested)
            }
            ScriptFunction::BuiltinFunction(f) => f(ctx),
        }
    }
}

macro_rules! value_now {
    ( $e:expr ) => {
        $e.get().map_err(move |e| Error::ValueError(e))?
    };
}

#[derive(Debug)]
pub enum EvalError {
    Err(Error),
    Nested(Source<Error>),
}

impl From<Source<EvalError>> for Source<Error> {
    fn from(v: Source<EvalError>) -> Self {
        let (s, v) = v.take();
        v.normalize(s)
    }
}

impl From<Source<EvalError>> for EvalError {
    fn from(v: Source<EvalError>) -> Self {
        Self::from(Source::<Error>::from(v))
    }
}

impl EvalError {
    /// Normalize to a Source<Error>.
    pub fn normalize(self, s: Source<()>) -> Source<Error> {
        use EvalError::*;
        match self {
            Err(e) => s.with(e),
            Nested(e) => e,
        }
    }
}

impl From<Error> for EvalError {
    fn from(e: Error) -> Self {
        Self::Err(e)
    }
}

impl From<&'_ str> for EvalError {
    fn from(s: &str) -> Self {
        Self::from(s.to_owned())
    }
}

impl From<String> for EvalError {
    fn from(s: String) -> Self {
        Self::Err(Error::ValueError(s))
    }
}

impl From<Source<Error>> for EvalError {
    fn from(e: Source<Error>) -> Self {
        Self::Nested(e)
    }
}

impl From<Source<&str>> for EvalError {
    fn from(s: Source<&str>) -> Self {
        Self::from(s.map(|s| s.to_owned()))
    }
}

impl From<Source<String>> for EvalError {
    fn from(s: Source<String>) -> Self {
        s.map(Error::ValueError).into()
    }
}

/// Apply the value to the given arguments.
///
/// If `env_lookup` is true and `v` is a `ScriptString`, it will be looked up in the environment.
pub fn apply_value<I: IntoIterator<Item = Source<Value>>>(
    ctx: &mut grease::Context<Context>,
    v: Source<Value>,
    args: I,
    env_lookup: bool,
) -> Result<Value, EvalError> {
    _apply_value(ctx, v, args.into_iter().peekable(), env_lookup)
}

fn _apply_value<I: Iterator<Item = Source<Value>>>(
    ctx: &mut grease::Context<Context>,
    v: Source<Value>,
    mut args: std::iter::Peekable<I>,
    env_lookup: bool,
) -> Result<Value, EvalError> {
    let v_source = v.source();
    v.map(move |v| {
        let v = if env_lookup {
            match v.typed::<ScriptString>() {
                Ok(val) => {
                    let s = value_now!(val).owned();
                    trace!("looking up '{}' in environment", s);
                    // Lookup string in environment, and apply result to remaining arguments
                    match ctx.inner.env_get(&s) {
                        Some(value) => {
                            trace!("found match in environment for '{}': {}", s, value.id());
                            value.clone().unwrap()
                        }
                        None => {
                            return Err(Error::MissingBinding(s).into());
                        }
                    }
                }
                Err(v) => v,
            }
        } else {
            v
        };

        if args.peek().is_none() {
            return Ok(v);
        }

        match_value!(v => {
            ScriptArray => |val| {
                let index = args.next().unwrap().map(|v|
                    v.typed::<ScriptString>()
                        .map_err(|_| "index must be a string".into())
                        .and_then(|v| v.get())
                    )
                    .transpose()?;
                let val = index.map(|index| match usize::from_str(index.as_ref()) {
                    Err(_) => Err(EvalError::from(Error::NonIntegerIndex)),
                    Ok(ind) => value_now!(val).0.get(ind).cloned().map(Source::unwrap)
                        .ok_or(Error::MissingIndex(index.to_owned()).into())
                }).transpose()?;

                let (source,val) = val.take();

                _apply_value(ctx, Source::from((source,v_source)).with(val), args, false)
            },
            ScriptMap => |val| {
                let index = args.next().unwrap().map(|v|
                    v.typed::<ScriptString>()
                        .map_err(|_| "index must be a string".into())
                        .and_then(|v| v.get())
                    )
                    .transpose()?;

                let val = index.map(|index|
                        value_now!(val).0.get(index.as_ref()).cloned().map(Source::unwrap)
                            .ok_or(EvalError::from(Error::MissingIndex(index.as_ref().into())))
                    )
                    .transpose()?;

                let (source,val) = val.take();

                _apply_value(ctx, Source::from((source,v_source)).with(val), args, false)
            },
            ScriptFunction => |val| {
                value_now!(val).plan_join(ctx, args.collect())
            },
            => |v| {
                Err(Error::NonCallableExpression(v).into())
            }
        })
    })
    .transpose_err()
    .map_err(Into::into)
}

impl Plan<Context> for Expression {
    type Output = Result<Value, EvalError>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        use Expression::*;
        match self {
            Empty => Ok(().into_value()),
            Expression::String(s) => Ok(s.into_value()),
            Array(es) => Ok(ScriptArray(
                es.into_iter()
                    .map(|e| e.plan(ctx))
                    .collect::<Result<Vec<_>, _>>()?,
            )
            .into_value()),
            SetVariable(var, e) => {
                let data = e.plan(ctx)?;
                ctx.inner.env_insert(var, data);
                Ok(().into_value())
            }
            UnsetVariable(var) => {
                ctx.inner.env_remove(&var);
                Ok(().into_value())
            }
            Command(cmd, args) => {
                let f = cmd.plan(ctx)?;
                let args = args
                    .into_iter()
                    .map(|a| a.plan(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                apply_value(ctx, f, args, true)
            }
            Block(es) => es.plan(ctx).map(Source::unwrap).map_err(Into::into),
            Function(e) => {
                Ok(ScriptFunction::UserFunction(*e, ctx.inner.env_flatten()).into_value())
            }
            If(cond, t, f) => {
                let cond = cond.plan(ctx)?.unwrap();

                let to_bool: Option<grease::IntoTyped<bool>> = ctx.traits.get(&cond);
                let cond = match to_bool {
                    Some(t) => {
                        let v = value_now!(t.into_typed(cond));
                        *v
                    }
                    None => true,
                };
                if cond { t.plan(ctx) } else { f.plan(ctx) }
                    .map(Source::unwrap)
                    .map_err(Into::into)
            }
        }
    }
}

impl Plan<Context> for Source<Expression> {
    type Output = Result<Source<Value>, Source<Error>>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        let (source, expr) = self.take();
        match expr.plan(ctx) {
            Ok(val) => Ok(source.with(val)),
            Err(e) => Err(e.normalize(source)),
        }
    }
}

impl Plan<Context> for Vec<Source<Expression>> {
    type Output = Result<Source<Value>, Source<Error>>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        // Push a new scope
        ctx.inner.env.push(Default::default());
        let mut val = Source::builtin(().into_value());
        for e in self {
            val = e.plan(ctx)?;
        }
        // Pop the pushed scope
        let ret = ctx.inner.env.pop().unwrap();
        // Return result based on final value.
        let (source, val) = val.take();
        Ok(val
            .typed::<ScriptUnit>()
            .map(move |_| Source::builtin(ScriptMap(ret).into_value()))
            .unwrap_or_else(|v| source.with(v)))
    }
}
