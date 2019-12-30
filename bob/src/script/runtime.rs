//! Script runtime definitions.

use super::ast::{Expression, Source};
use grease::future::FusedFuture;
use grease::{GetValueType, Plan, Value};
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::str::FromStr;
use std::task::Poll;

#[path = "runtime/do.rs"]
pub mod do_;
pub mod exec;
pub mod map;
pub mod path;
pub mod track;

/// A script runtime value.
#[derive(Clone, Debug, GetValueType)]
pub enum Data {
    /// No value.
    Unit,
    /// A string.
    String(String),
    /// A value from a command.
    Value(Value),
    /// An array of values.
    Array(Vec<Data>),
    /// A map of values, not preserving order.
    Map(HashMap<String, Data>),
    /// A function.
    Function(std::sync::Arc<DataFunction>),
}

impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Value(a), Self::Value(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Map(a), Self::Map(b)) => a == b,
            (Self::Function(a), Self::Function(b)) => std::sync::Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for Data {}

impl Future for Data {
    type Output = Result<(), String>;

    fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context) -> Poll<Self::Output> {
        match *self {
            Self::Value(_) => Future::poll(
                unsafe {
                    self.map_unchecked_mut(|s| {
                        if let Self::Value(ref mut v) = s {
                            v
                        } else {
                            panic!("value changed");
                        }
                    })
                },
                cx,
            )
            .map(|v| v.map(|_| ())),
            Self::Array(_) => {
                let pending = unsafe {
                    self.map_unchecked_mut(|s| {
                        if let Self::Array(ref mut arr) = s {
                            arr
                        } else {
                            panic!("value changed");
                        }
                    })
                }
                .iter_mut()
                .map(|a| {
                    if a.is_terminated() {
                        Some(Ok(()))
                    } else {
                        match Future::poll(Pin::new(a), cx) {
                            Poll::Pending => None,
                            Poll::Ready(r) => Some(r.map(|_| ())),
                        }
                    }
                })
                .collect::<Option<Vec<_>>>()
                .map(|v| v.into_iter().collect::<Result<(), _>>());
                if let Some(result) = pending {
                    Poll::Ready(result)
                } else {
                    Poll::Pending
                }
            }
            Self::Map(_) => {
                let pending = unsafe {
                    self.map_unchecked_mut(|s| {
                        if let Self::Map(ref mut m) = s {
                            m
                        } else {
                            panic!("value changed");
                        }
                    })
                }
                .iter_mut()
                .map(|(_, a)| {
                    if a.is_terminated() {
                        Some(Ok(()))
                    } else {
                        match Future::poll(Pin::new(a), cx) {
                            Poll::Pending => None,
                            Poll::Ready(r) => Some(r),
                        }
                    }
                })
                .collect::<Option<Vec<_>>>()
                .map(|v| v.into_iter().collect::<Result<(), _>>());
                if let Some(result) = pending {
                    Poll::Ready(result.map(|_| ()))
                } else {
                    Poll::Pending
                }
            }
            _ => std::task::Poll::Ready(Ok(())),
        }
    }
}

impl FusedFuture for Data {
    fn is_terminated(&self) -> bool {
        match self {
            Self::Value(v) => v.is_terminated(),
            Self::Array(arr) => arr.iter().all(|v| v.is_terminated()),
            Self::Map(m) => m.iter().all(|(_, v)| v.is_terminated()),
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum FunctionError {
    Err(String),
    Nested(Source<Error>),
}

impl From<&'_ str> for FunctionError {
    fn from(s: &str) -> Self {
        FunctionError::Err(s.to_owned())
    }
}

impl From<String> for FunctionError {
    fn from(s: String) -> Self {
        FunctionError::Err(s)
    }
}

impl From<Source<Error>> for FunctionError {
    fn from(e: Source<Error>) -> Self {
        FunctionError::Nested(e)
    }
}

type Builtin =
    dyn Fn(&mut grease::Context<FunctionContext>) -> Result<Data, FunctionError> + Send + Sync;

/// A function value.
pub enum DataFunction {
    UserFunction(Source<Expression>),
    BuiltinFunction(Box<Builtin>),
}

impl fmt::Debug for DataFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UserFunction(e) => write!(f, "UserFunction: {:?}", e),
            Self::BuiltinFunction(_) => write!(f, "BuiltinFunction"),
        }
    }
}

impl From<&'_ Data> for bool {
    fn from(v: &Data) -> bool {
        if let Data::Unit = v {
            false
        } else {
            true
        }
    }
}

/// The script context.
pub struct Context {
    env: Vec<HashMap<String, Data>>,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            env: vec![HashMap::new()],
        }
    }
}

impl Context {
    pub fn env_insert(&mut self, k: String, v: Data) {
        self.env.last_mut().unwrap().insert(k, v);
    }

    pub fn env_remove<Q: ?Sized>(&mut self, k: &Q) -> Option<Data>
    where
        String: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.env.last_mut().unwrap().remove(k)
    }

    pub fn env_get<Q: ?Sized>(&mut self, k: &Q) -> Option<&Data>
    where
        String: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.env.iter().rev().find_map(|m| m.get(k))
    }
}

/// Function call context.
pub struct FunctionContext {
    ctx: Context,
    args: Vec<Data>,
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
    type Extra = Vec<Data>;

    fn split(self) -> (Context, Vec<Data>) {
        (self.ctx, self.args)
    }

    fn join(ctx: Context, args: Vec<Data>) -> Self {
        FunctionContext { ctx, args }
    }
}

/// Script runtime errors.
#[derive(Debug)]
pub enum Error {
    /// An integer index (for arrays) was expected.
    NonIntegerIndex,
    /// An index was not present.
    MissingIndex,
    /// An indexing operation was attempted on a type that is not an array or map.
    InvalidIndex,
    /// An error from a builtin function.
    FunctionError(String),
    /// An expression is in call-position (had arguments) but is not callable.
    NonCallableExpression(Data),
    /// No exec bindings is available in the current environment.
    ExecMissing,
    /// The unit type is passed more than one argument.
    UnitTooManyArguments,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NonIntegerIndex => write!(f, "positive integer index expected"),
            Self::MissingIndex => write!(f, "index missing"),
            Self::InvalidIndex => write!(f, "type is not an array or map; cannot index"),
            Self::FunctionError(s) => write!(f, "{}", s),
            Self::NonCallableExpression(d) => {
                write!(f, "cannot pass arguments to non-callable value {:?}", d)
            }
            Self::ExecMissing => write!(f, "'exec' is not available in the current environment"),
            Self::UnitTooManyArguments => write!(f, "only one argument allowed"),
        }
    }
}

impl Plan<FunctionContext> for std::sync::Arc<DataFunction> {
    type Output = Result<Data, FunctionError>;

    fn plan(self, ctx: &mut grease::Context<FunctionContext>) -> Self::Output {
        match self.as_ref() {
            DataFunction::UserFunction(e) => {
                // A FunctionContext is only used once, so swap out the arguments.
                let mut args = Vec::new();
                std::mem::swap(&mut ctx.inner.args, &mut args);

                ctx.inner.env.push(HashMap::new());
                ctx.inner.env_insert("@".to_owned(), Data::Array(args));
                let v = e.clone().plan_split(ctx);
                ctx.inner.env.pop();
                v.map_err(FunctionError::Nested)
            }
            DataFunction::BuiltinFunction(f) => f(ctx),
        }
    }
}

impl Plan<Context> for Source<Expression> {
    type Output = Result<Data, Source<Error>>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        use Expression::*;
        let (source, expr) = self.take();
        match expr {
            Empty => Ok(Data::Unit),
            Expression::String(s) => Ok(Data::String(s.clone())),
            Array(es) => Ok(Data::Array(
                es.into_iter()
                    .map(|e| e.plan(ctx))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            SetVariable(var, e) => {
                let data = e.plan(ctx)?;
                ctx.inner.env_insert(var.clone(), data);
                Ok(Data::Unit)
            }
            UnsetVariable(var) => {
                ctx.inner.env_remove(&var);
                Ok(Data::Unit)
            }
            Index(e, i) => match e.plan(ctx)? {
                Data::Array(v) => match usize::from_str(&i) {
                    Err(_) => Err(source.with(Error::NonIntegerIndex)),
                    Ok(ind) => v.get(ind).cloned().ok_or(source.with(Error::MissingIndex)),
                },
                Data::Map(v) => v.get(&i).cloned().ok_or(source.with(Error::MissingIndex)),
                _ => Err(source.with(Error::InvalidIndex)),
            },
            Command(cmd, mut args) => {
                let cmdsource = cmd.clone().with(());
                match cmd.plan(ctx)? {
                    Data::String(s) => {
                        // Lookup string in environment, and apply result to remaining arguments
                        let (cmd, args) = match ctx.inner.env_get(&s) {
                            Some(data) => (data, args),
                            None => {
                                // Fall back to 'exec' command.
                                let exec = match ctx.inner.env_get("exec") {
                                    Some(v) => v,
                                    None => return Err(source.with(Error::ExecMissing)),
                                };
                                args.insert(0, cmdsource.with(Expression::String(s)));
                                (exec, args)
                            }
                        };

                        let has_args = !args.is_empty();

                        match cmd.clone() {
                            Data::Function(f) if has_args => {
                                let args =
                                    args.into_iter()
                                        .map(|a| a.plan(ctx))
                                        .collect::<Result<Vec<_>, _>>()?;
                                match f.plan_join(ctx, args) {
                                    Err(e) => match e {
                                        FunctionError::Err(e) => {
                                            Err(source.with(Error::FunctionError(e)))
                                        }
                                        FunctionError::Nested(e) => Err(e),
                                    },
                                    Ok(r) => Ok(r),
                                }
                            }
                            v => {
                                if has_args {
                                    Err(source.with(Error::NonCallableExpression(v)))
                                } else {
                                    Ok(v)
                                }
                            }
                        }
                    }
                    Data::Value(v) => {
                        // Fall back to 'exec' command.
                        let exec = match ctx.inner.env_get("exec") {
                            Some(Data::Function(f)) => f.clone(),
                            _ => return Err(source.with(Error::ExecMissing)),
                        };
                        let mut args = args
                            .into_iter()
                            .map(|a| a.plan(ctx))
                            .collect::<Result<Vec<_>, _>>()?;
                        args.insert(0, Data::Value(v));

                        match exec.plan_join(ctx, args) {
                            Err(e) => match e {
                                FunctionError::Err(e) => Err(source.with(Error::FunctionError(e))),
                                FunctionError::Nested(e) => Err(e),
                            },
                            Ok(r) => Ok(r),
                        }
                    }
                    Data::Unit => {
                        // Return a single argument if provided, else return unit
                        let mut args = args.into_iter();
                        if let Some(v) = args.next() {
                            if args.next().is_some() {
                                Err(source.with(Error::UnitTooManyArguments))
                            } else {
                                v.plan(ctx)
                            }
                        } else {
                            Ok(Data::Unit)
                        }
                    }
                    d => Ok(d),
                }
            }
            Block(es) => es.plan(ctx),
            Function(e) => Ok(Data::Function(DataFunction::UserFunction(*e).into())),
            If(cond, t, f) => {
                let cond = cond.plan(ctx)?;
                if (&cond).into() {
                    t.plan(ctx)
                } else {
                    f.plan(ctx)
                }
            }
        }
    }
}

impl Plan<Context> for Vec<Source<Expression>> {
    type Output = Result<Data, Source<Error>>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        // Push a new scope
        ctx.inner.env.push(HashMap::new());
        let mut val = Data::Unit;
        for e in self {
            val = e.plan(ctx)?;
        }
        let ret = ctx.inner.env.pop().unwrap();
        if let Data::Unit = val {
            Ok(Data::Map(ret))
        } else {
            Ok(val)
        }
    }
}
