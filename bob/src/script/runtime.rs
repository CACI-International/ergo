//! Script runtime definitions.

use super::ast::Expression;
use grease::{GetValueType, Plan, Value};
use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::str::FromStr;
use std::task::Poll;

pub mod exec;

/// A script runtime value.
#[derive(Clone, Debug, GetValueType)]
pub enum Data {
    /// A string.
    String(String),
    /// A value from a command.
    Value(Value),
    /// An array of values.
    Array(Vec<Data>),
    /// A map of values, not preserving order.
    Map(HashMap<String, Data>),
    /// A function.
    Function(std::rc::Rc<DataFunction>),
}

impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Value(a), Self::Value(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Map(a), Self::Map(b)) => a == b,
            (Self::Function(a), Self::Function(b)) => std::rc::Rc::ptr_eq(a, b),
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
            ).map(|v| v.map(|_| ())),
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
                .map(|a| match Future::poll(Pin::new(a), cx) {
                    Poll::Pending => None,
                    Poll::Ready(r) => Some(r),
                }).collect::<Option<Vec<_>>>()
                .map(|v| v.into_iter().collect::<Result<Vec<_>,_>>());
                if let Some(result) = pending {
                    Poll::Ready(result.map(|_| ()))
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
                .map(|(_, a)| match Future::poll(Pin::new(a), cx) {
                    Poll::Pending => None,
                    Poll::Ready(r) => Some(r),
                }).collect::<Option<Vec<_>>>()
                .map(|v| v.into_iter().collect::<Result<Vec<_>,_>>());
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

type Builtin = dyn Fn(&mut grease::Context<FunctionContext>) -> Result<Data, String>;

/// A function value.
pub enum DataFunction {
    UserFunction(Expression),
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
        if let Data::String(s) = v {
            !s.is_empty()
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
        }
    }
}

impl Plan<FunctionContext> for std::rc::Rc<DataFunction> {
    type Output = Result<Data, Error>;

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
                v
            }
            DataFunction::BuiltinFunction(f) => f(ctx).map_err(Error::FunctionError),
        }
    }
}

impl Plan<Context> for Expression {
    type Output = Result<Data, Error>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        match self {
            Self::String(s) => Ok(Data::String(s.clone())),
            Self::Array(es) => Ok(Data::Array(
                es.into_iter()
                    .map(|e| e.plan(ctx))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Self::SetVariable(var, e) => {
                let data = e.plan(ctx)?;
                ctx.inner.env_insert(var.clone(), data);
                Ok(Data::String("".to_owned()))
            }
            Self::UnsetVariable(var) => {
                ctx.inner.env_remove(&var);
                Ok(Data::String("".to_owned()))
            }
            Self::Index(e, i) => match e.plan(ctx)? {
                Data::Array(v) => {
                    let ind = usize::from_str(&i).map_err(|_| Error::NonIntegerIndex)?;
                    v.get(ind).cloned().ok_or(Error::MissingIndex)
                }
                Data::Map(v) => v.get(&i).cloned().ok_or(Error::MissingIndex),
                _ => Err(Error::InvalidIndex),
            },
            Self::Command(cmd, mut args) => {
                match cmd.plan(ctx)? {
                    Data::String(s) => {
                        // Lookup string in environment, and apply result to remaining arguments
                        let (cmd, args) = match ctx.inner.env_get(&s) {
                            Some(data) => (data, args),
                            None => {
                                // Fall back to 'exec' command.
                                let exec = ctx.inner.env_get("exec").ok_or(Error::ExecMissing)?;
                                args.insert(0, Expression::String(s));
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
                                f.plan_join(ctx, args)
                            }
                            v => {
                                if has_args {
                                    Err(Error::NonCallableExpression(v))
                                } else {
                                    Ok(v)
                                }
                            }
                        }
                    }
                    d => Ok(d),
                }
            }
            Self::Block(es) => es.plan(ctx).map(Data::Map),
            Self::Function(e) => Ok(Data::Function(DataFunction::UserFunction(*e).into())),
            Self::If(cond, t, f) => {
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

impl Plan<Context> for Vec<Expression> {
    type Output = Result<HashMap<String, Data>, Error>;

    fn plan(self, ctx: &mut grease::Context<Context>) -> Self::Output {
        // Push a new scope
        ctx.inner.env.push(HashMap::new());
        let mut val = Data::String("".to_owned());
        for e in self {
            val = e.plan(ctx)?;
        }
        let mut ret = ctx.inner.env.pop().unwrap();
        ret.insert("*".to_owned(), val);
        Ok(ret)
    }
}
