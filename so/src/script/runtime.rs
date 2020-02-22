//! Script runtime definitions.

use super::ast::{Expression, IntoSource, Source};
use grease::future::{BoxFuture, FutureExt};
use grease::{make_value, match_value, IntoValue, Plan, Value};
use log::trace;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::iter::FromIterator;
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
    use super::{Error, Eval};
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

    pub type Env = BTreeMap<String, Eval<Source<Value>>>;

    /// Script function type.
    #[derive(GetValueType)]
    pub enum ScriptFunction {
        UserFunction(Source<Expression>, Env),
        BuiltinFunction(Box<Builtin>),
    }

    type Builtin = dyn Fn(&mut grease::Context<super::FunctionContext>) -> Result<Eval<Value>, Error>
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
    pub use super::{Error, Eval, FunctionContext, SourceContext};
    pub use grease::{Context, Value};

    #[macro_export]
    macro_rules! def_builtin {
        ( $ctx:ident , $args:ident => $body:expr ) => {
            pub fn builtin() -> ::grease::Value {
                $crate::script::runtime::ScriptFunction::BuiltinFunction(Box::new(builtin_impl))
                    .into()
            }

            fn builtin_impl($ctx: &mut Context<FunctionContext>) -> Result<Eval<Value>, Error> {
                let mut $args = Vec::new();
                std::mem::swap(&mut $args, &mut $ctx.inner.args);

                $body
            }
        };
    }

    #[macro_export]
    macro_rules! eval_error {
        ($ctx:expr , $val:expr) => {
            $crate::eval_error!($ctx, $val, {
                return Ok(Eval::Error);
            })
        };
        ($ctx:expr , $val:expr , $then:expr) => {
            match $val {
                Err(e) => {
                    $ctx.error(e);
                    $then
                }
                Ok(v) => v,
            }
        };
    }

    pub use crate::def_builtin;
    pub use crate::eval_error;
}

use builtin_function_prelude::eval_error;
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
    env: Vec<Env>,
    errors: Vec<SourceContext<Error>>,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            // The default context should start with an empty map as the base.
            env: vec![Default::default()],
            errors: vec![],
        }
    }
}

impl Context {
    pub fn env_insert<T: Into<Eval<Source<Value>>>>(&mut self, k: String, v: T) {
        self.env.last_mut().unwrap().insert(k, v.into());
    }

    pub fn env_remove<Q: ?Sized>(&mut self, k: &Q) -> Option<Eval<Source<Value>>>
    where
        String: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env.last_mut().unwrap().remove(k)
    }

    pub fn env_get<Q: ?Sized>(&mut self, k: &Q) -> Option<&Eval<Source<Value>>>
    where
        String: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env.iter().rev().find_map(|m| m.get(k))
    }

    pub fn env_flatten(&self) -> BTreeMap<String, Eval<Source<Value>>> {
        self.env.iter().fold(Default::default(), |mut e, m| {
            e.append(&mut m.clone());
            e
        })
    }

    pub fn error<E: Into<SourceContext<Error>>>(&mut self, err: E) {
        self.errors.push(err.into());
    }

    pub fn get_errors(&mut self) -> Vec<SourceContext<Error>> {
        let mut v = Vec::new();
        std::mem::swap(&mut self.errors, &mut v);
        v
    }
}

impl From<Env> for Context {
    fn from(v: Env) -> Self {
        Context {
            env: vec![v],
            errors: vec![],
        }
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

impl From<&'_ str> for Error {
    fn from(s: &str) -> Self {
        Self::from(s.to_owned())
    }
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::ValueError(s)
    }
}

impl From<Source<&'_ str>> for Source<Error> {
    fn from(s: Source<&str>) -> Self {
        Self::from(s.map(ToOwned::to_owned))
    }
}

impl From<Source<String>> for Source<Error> {
    fn from(s: Source<String>) -> Self {
        s.map(Error::ValueError)
    }
}

#[derive(Debug)]
pub struct SourceContext<T> {
    value: Source<T>,
    context: Vec<Source<String>>,
}

impl<T> SourceContext<T> {
    pub fn add_context(&mut self, ctx: Source<String>) {
        self.context.push(ctx)
    }
}

impl<U> From<Source<U>> for SourceContext<Error>
where
    Error: From<U>,
{
    fn from(u: Source<U>) -> SourceContext<Error> {
        SourceContext {
            value: u.map(Error::from),
            context: vec![],
        }
    }
}

impl<T: fmt::Display> fmt::Display for SourceContext<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.value)?;
        for c in &self.context {
            writeln!(f, "{}", c)?;
        }
        Ok(())
    }
}

macro_rules! value_now {
    ( $e:expr ) => {
        $e.get().map_err(move |e| Error::ValueError(e))?
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum Eval<T> {
    Value(T),
    Error,
}

impl<T> Eval<T> {
    pub fn map<F, U>(self, f: F) -> Eval<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Eval::Value(v) => Eval::Value(f(v)),
            Eval::Error => Eval::Error,
        }
    }
}

impl<T> From<T> for Eval<T> {
    fn from(v: T) -> Self {
        Eval::Value(v)
    }
}

impl<T, V> FromIterator<Eval<T>> for Eval<V>
where
    V: FromIterator<T>,
{
    fn from_iter<I: IntoIterator<Item = Eval<T>>>(iter: I) -> Self {
        let mut bad = false;
        let r = V::from_iter(iter.into_iter().filter_map(|v| {
            if bad {
                return None;
            }
            match v {
                Eval::Error => {
                    bad = true;
                    None
                }
                Eval::Value(v) => Some(v),
            }
        }));
        if bad {
            Eval::Error
        } else {
            Eval::Value(r)
        }
    }
}

impl<T> std::ops::BitAndAssign for Eval<T> {
    fn bitand_assign(&mut self, other: Eval<T>) {
        if let Eval::Value(a) = self {
            if let Eval::Value(b) = other {
                *a = b;
                return;
            }
        }
        *self = Eval::Error;
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
) -> Result<Eval<Value>, Error> {
    _apply_value(ctx, v, args.into_iter().peekable(), env_lookup)
}

fn _apply_value<I: Iterator<Item = Source<Value>>>(
    ctx: &mut grease::Context<Context>,
    v: Source<Value>,
    mut args: std::iter::Peekable<I>,
    env_lookup: bool,
) -> Result<Eval<Value>, Error> {
    let v_source = v.source();

    let result = v.map(|v| {
        let v = if env_lookup {
            match v.typed::<ScriptString>() {
                Ok(val) => {
                    let s = value_now!(val).owned();
                    trace!("looking up '{}' in environment", s);
                    // Lookup string in environment, and apply result to remaining arguments
                    match ctx.inner.env_get(&s) {
                        Some(Eval::Value(value)) => {
                            trace!("found match in environment for '{}': {}", s, value.id());
                            value.clone().unwrap()
                        }
                        Some(Eval::Error) => return Ok(Eval::Error),
                        None => {
                            return Err(Error::MissingBinding(s));
                        }
                    }
                }
                Err(v) => v,
            }
        } else {
            v
        };

        if args.peek().is_none() {
            return Ok(v.into());
        }

        match_value!(v => {
            ScriptArray => |val| {
                let index = eval_error!(ctx, args.next().unwrap().map(|v|
                    v.typed::<ScriptString>()
                        .map_err(|_| "index must be a string".into())
                        .and_then(|v| v.get())
                    )
                    .transpose());
                let val = eval_error!(ctx, index.map(|index| match usize::from_str(index.as_ref()) {
                    Err(_) => Err(Error::NonIntegerIndex),
                    Ok(ind) => value_now!(val).0.get(ind).cloned().map(Source::unwrap)
                        .ok_or(Error::MissingIndex(index.to_owned()))
                }).transpose());

                let (source,val) = val.take();

                _apply_value(ctx, Source::from((v_source,source)).with(val), args, false)
            },
            ScriptMap => |val| {
                let index = eval_error!(ctx, args.next().unwrap().map(|v|
                    v.typed::<ScriptString>()
                        .map_err(|_| "index must be a string".into())
                        .and_then(|v| v.get())
                    )
                    .transpose());

                let val = eval_error!(ctx, index.map(|index|
                        value_now!(val).0.get(index.as_ref()).cloned().map(Source::unwrap)
                            .ok_or(Error::MissingIndex(index.as_ref().into()))
                    )
                    .transpose());

                let (source,val) = val.take();

                _apply_value(ctx, Source::from((v_source,source)).with(val), args, false)
            },
            ScriptFunction => |val| {
                value_now!(val).plan_join(ctx, args.collect())
            },
            => |v| {
                Err(Error::NonCallableExpression(v).into())
            }
        })
    })
    .transpose_err();
    Ok(eval_error!(ctx, result))
}

impl Plan<FunctionContext> for &'_ ScriptFunction {
    type Output = Result<Eval<Value>, Error>;

    fn plan(self, ctx: &mut grease::Context<FunctionContext>) -> Self::Output {
        match self {
            ScriptFunction::UserFunction(e, env) => {
                // A FunctionContext is only used once, so swap out the arguments.
                let mut args = Vec::new();
                std::mem::swap(&mut ctx.inner.args, &mut args);

                let args = args
                    .into_source()
                    .map(|args| ScriptArray(args).into_value());

                let mut env = vec![env.clone()];
                std::mem::swap(&mut env, &mut ctx.inner.env);
                ctx.env_insert("@".to_owned(), args);
                let v = e.clone().plan_split(ctx);
                std::mem::swap(&mut env, &mut ctx.inner.env);

                Ok(v.map(Source::unwrap))
            }
            ScriptFunction::BuiltinFunction(f) => f(ctx),
        }
    }
}

impl Plan<Context> for Expression {
    type Output = Result<Eval<Value>, Error>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        use Expression::*;
        match self {
            Empty => Ok(().into_value().into()),
            Expression::String(s) => Ok(s.into_value().into()),
            Array(es) => {
                let vals = es
                    .into_iter()
                    .map(|e| e.plan(ctx))
                    .collect::<Eval<Vec<Source<Value>>>>();
                Ok(vals.map(|vals| ScriptArray(vals).into_value()))
            }
            SetVariable(var, e) => {
                let data = e.plan(ctx);
                let ret = Ok(match data {
                    Eval::Error => Eval::Error,
                    _ => ().into_value().into(),
                });
                ctx.inner.env_insert(var, data);
                ret
            }
            UnsetVariable(var) => {
                ctx.inner.env_remove(&var);
                Ok(().into_value().into())
            }
            Command(cmd, args) => {
                let f = cmd.plan(ctx);
                let args = args
                    .into_iter()
                    .map(|a| a.plan(ctx))
                    .collect::<Eval<Vec<_>>>();
                match (f, args) {
                    (Eval::Value(f), Eval::Value(args)) => apply_value(ctx, f, args, true),
                    _ => Ok(Eval::Error),
                }
            }
            Block(_) => panic!("Block expression must be evaluated at a higher level"),
            Function(e) => Ok(Eval::Value(
                ScriptFunction::UserFunction(*e, ctx.inner.env_flatten()).into_value(),
            )),
            If(cond, t, f) => {
                let cond = match cond.plan(ctx) {
                    Eval::Value(v) => v.unwrap(),
                    Eval::Error => return Ok(Eval::Error),
                };

                let to_bool: Option<grease::IntoTyped<bool>> = ctx.traits.get(&cond);
                let cond = match to_bool {
                    Some(t) => {
                        let v = value_now!(t.into_typed(cond));
                        *v
                    }
                    None => true,
                };
                Ok(if cond { t.plan(ctx) } else { f.plan(ctx) }.map(Source::unwrap))
            }
        }
    }
}

impl Plan<Context> for Source<Expression> {
    type Output = Eval<Source<Value>>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        let (source, expr) = self.take();
        if let Expression::Block(es) = expr {
            source.with(es).plan(ctx)
        } else {
            match expr.plan(ctx) {
                Ok(val) => val.map(|v| source.with(v)),
                Err(e) => {
                    ctx.error(source.with(e));
                    Eval::Error
                }
            }
        }
    }
}

impl Plan<Context> for Source<Vec<Source<Expression>>> {
    type Output = Eval<Source<Value>>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        // Push a new scope
        ctx.inner.env.push(Default::default());
        let (self_source, this) = self.take();
        let mut val = Eval::Value(self_source.clone().with(().into_value()));
        for e in this {
            val &= e.plan(ctx);
        }

        // Pop the pushed scope
        let ret = ctx.inner.env.pop().unwrap();
        let ret = ret
            .into_iter()
            .map(|(k, v)| v.map(move |v| (k, v)))
            .collect::<Eval<BTreeMap<_, _>>>();

        // Return result based on final value.
        match val {
            Eval::Error => Eval::Error,
            Eval::Value(val) => {
                let (source, val) = val.take();
                match val.typed::<ScriptUnit>() {
                    Ok(_) => ret.map(|ret| self_source.with(ScriptMap(ret).into_value())),
                    Err(v) => Eval::Value(source.with(v)),
                }
            }
        }
    }
}
