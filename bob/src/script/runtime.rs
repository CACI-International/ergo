//! Script runtime definitions.

use super::ast::{Expression, IntoSource, Source};
use grease::{make_value, match_value, IntoValue, Plan, Value};
use grease::future::{BoxFuture, FutureExt};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::str::FromStr;

#[path = "runtime/do.rs"]
pub mod do_;
pub mod exec;
pub mod has;
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
        UserFunction(Source<Expression>),
        BuiltinFunction(Box<Builtin>),
    }

    type Builtin = dyn Fn(&mut grease::Context<super::FunctionContext>) -> Result<Value, super::EvalError>
        + Send
        + Sync;

    impl std::hash::Hash for ScriptFunction {
        fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
            match self {
                Self::UserFunction(s) => s.hash(h),
                Self::BuiltinFunction(b) => std::ptr::hash(b, h),
            }
        }
    }

    impl fmt::Debug for ScriptFunction {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::UserFunction(e) => write!(f, "UserFunction: {:?}", e),
                Self::BuiltinFunction(_) => write!(f, "BuiltinFunction"),
            }
        }
    }
}

use script_types::*;

fn get_values<'a> (v: Source<Value>, set: &'a mut BTreeSet<Source<Value>>) -> BoxFuture<'a, Result<(), Source<String>>> {
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
        }.await;
        source.with(res)
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
    }.boxed()
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
    /// No exec bindings is available in the current environment.
    ExecMissing,
    /// The unit type is passed more than one argument.
    UnitTooManyArguments,
    /// An error occured while evaluating a value.
    ValueError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NonIntegerIndex => write!(f, "positive integer index expected"),
            Self::MissingIndex(s) => write!(f, "index missing: {}", s),
            Self::InvalidIndex => write!(f, "type is not an array or map; cannot index"),
            Self::NonCallableExpression(d) => {
                write!(f, "cannot pass arguments to non-callable value {:?}", d)
            }
            Self::ExecMissing => write!(f, "'exec' is not available in the current environment"),
            Self::UnitTooManyArguments => write!(f, "only one argument allowed"),
            Self::ValueError(s) => write!(f, "{}", s),
        }
    }
}

impl Plan<FunctionContext> for &'_ ScriptFunction {
    type Output = Result<Value, EvalError>;

    fn plan(self, ctx: &mut grease::Context<FunctionContext>) -> Self::Output {
        match self {
            ScriptFunction::UserFunction(e) => {
                // A FunctionContext is only used once, so swap out the arguments.
                let mut args = Vec::new();
                std::mem::swap(&mut ctx.inner.args, &mut args);

                let args = args
                    .into_source()
                    .map(|args| ScriptArray(args).into_value());

                ctx.inner.env.push(Default::default());
                ctx.inner.env_insert("@".to_owned(), args);
                let v = e.clone().plan_split(ctx);
                ctx.inner.env.pop();
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
            Index(e, i) => match_value!(e.plan(ctx)?.unwrap() => {
                ScriptArray => |val| match usize::from_str(&i) {
                    Err(_) => Err(Error::NonIntegerIndex.into()),
                    Ok(ind) => value_now!(val).0.get(ind).cloned().map(Source::unwrap).ok_or(Error::MissingIndex(i.clone()).into())
                },
                ScriptMap => |val| value_now!(val).0.get(&i).cloned().map(Source::unwrap).ok_or(Error::MissingIndex(i).into()),
                => |_| Err(Error::InvalidIndex.into())
            }),
            Command(cmd, mut args) => {
                let cmdsource = cmd.source();
                match_value!(cmd.plan(ctx)?.unwrap() => {
                    ScriptString => |val| {
                        let s = value_now!(val).owned();
                        // Lookup string in environment, and apply result to remaining arguments
                        let (cmd, args) = match ctx.inner.env_get(&s) {
                            Some(value) => (value, args),
                            None => {
                                // Fall back to 'exec' command.
                                let exec = match ctx.inner.env_get("exec") {
                                    Some(v) => v,
                                    None => return Err(Error::ExecMissing.into()),
                                };
                                args.insert(0, cmdsource.with(Expression::String(s)));
                                (exec, args)
                            }
                        };

                        let has_args = !args.is_empty();

                        match_value!(cmd.clone().unwrap() => {
                            ScriptFunction => |val| {
                                if has_args {
                                    let f = value_now!(val);
                                    let args =
                                        args.into_iter()
                                            .map(|a| a.plan(ctx))
                                            .collect::<Result<Vec<_>, _>>()?;
                                    f.plan_join(ctx, args)
                                } else {
                                    Ok(val.into())
                                }
                            },
                            => |v| {
                                if has_args {
                                    Err(Error::NonCallableExpression(v).into())
                                } else {
                                    Ok(v)
                                }
                            }
                        })
                    },
                    std::path::PathBuf => |val| {
                        // Fall back to 'exec' command.
                        let exec = match ctx.inner.env_get("exec") {
                            Some(f) => match f.clone().unwrap().typed::<ScriptFunction>() {
                                Ok(f) => f,
                                Err(_) => return Err(Error::ExecMissing.into())
                            }
                            _ => return Err(Error::ExecMissing.into()),
                        };
                        let mut args = args
                            .into_iter()
                            .map(|a| a.plan(ctx))
                            .collect::<Result<Vec<_>, _>>()?;
                        args.insert(0, cmdsource.with(val.into()));

                        let exec = value_now!(exec);

                        exec.plan_join(ctx, args)
                    },
                    ScriptUnit => |_| {
                        // Return a single argument if provided, else return unit
                        let mut args = args.into_iter();
                        if let Some(v) = args.next() {
                            if args.next().is_some() {
                                Err(Error::UnitTooManyArguments.into())
                            } else {
                                v.plan(ctx).map(Source::unwrap).map_err(Into::into)
                            }
                        } else {
                            Ok(().into_value())
                        }
                    },
                    => |v| Ok(v)
                })
            }
            Block(es) => es.plan(ctx).map(Source::unwrap).map_err(Into::into),
            Function(e) => Ok(ScriptFunction::UserFunction(*e).into_value()),
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
