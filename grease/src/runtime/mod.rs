//! Execution logic for plans.

use std::fmt;

mod command;
mod log;
mod store;
mod task_manager;
mod traits;

use self::log::EmptyLogTarget;
pub use self::log::{logger_ref, Log, LogEntry, LogLevel, LogTarget, LoggerRef};
pub use command::Commands;
pub use store::{Item, ItemContent, ItemName, Store};
pub use task_manager::TaskManager;
pub use traits::{TraitGenerator, Traits};

trait CallMut {
    fn call_mut<O, F>(&mut self, f: F) -> O
    where
        Self: Sized,
        F: FnOnce(Self) -> (Self, O),
    {
        let selfptr = self as *mut Self;
        unsafe {
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(selfptr.read()))) {
                Ok((nself, o)) => {
                    selfptr.write(nself);
                    o
                }
                Err(_) => {
                    // inconsistent memory ownership, no choice but to abort
                    std::process::abort()
                }
            }
        }
    }
}

impl<T: Sized> CallMut for T {}

/// Define forwards and backwards conversions to a type, with sideband data.
pub trait SplitInto<To> {
    type Extra;

    /// Split this value into the target value and extra data.
    fn split(self) -> (To, Self::Extra);

    /// Create a value from the target value and extra data.
    fn join(a: To, b: Self::Extra) -> Self;

    /// Split this context and run the given function on it.
    fn split_map<F, Ret>(&mut self, f: F) -> Ret
    where
        F: FnOnce(&mut To) -> Ret,
        Self: Sized,
    {
        self.call_mut(move |this| {
            let (mut to, extra) = this.split();
            let ret = f(&mut to);
            (Self::join(to, extra), ret)
        })
    }
}

impl<T> SplitInto<()> for T {
    type Extra = T;

    fn split(self) -> ((), T) {
        ((), self)
    }

    fn join(_: (), v: T) -> Self {
        v
    }
}

impl<T, O> SplitInto<Context<O>> for Context<T>
where
    T: SplitInto<O>,
{
    type Extra = T::Extra;

    fn split(self) -> (Context<O>, Self::Extra) {
        let (o, e) = self.inner.split();
        (
            Context {
                task: self.task,
                cmd: self.cmd,
                log: self.log,
                store: self.store,
                traits: self.traits,
                inner: o,
            },
            e,
        )
    }

    fn join(c: Context<O>, e: Self::Extra) -> Self {
        Context {
            task: c.task,
            cmd: c.cmd,
            log: c.log,
            store: c.store,
            traits: c.traits,
            inner: T::join(c.inner, e),
        }
    }
}

/// A type which can be used for plan creation.
///
/// In general, Output should contain one or more Values for use in other plans.
/// Exactly one of (plan,plan_ref) must be specified.
pub trait Plan<Ctx = ()> {
    /// The output type of planning.
    type Output;

    /// Create a plan given the context.
    fn plan(self, ctx: &mut Context<Ctx>) -> Self::Output;

    /// Call plan by splitting a nested context.
    fn plan_split<T>(self, ctx: &mut T) -> Self::Output
    where
        Self: Sized,
        T: SplitInto<Context<Ctx>>,
    {
        ctx.call_mut(|ctx| {
            let (mut ctx2, e) = ctx.split();
            let ret = self.plan(&mut ctx2);
            (T::join(ctx2, e), ret)
        })
    }

    /// Call plan by creating the context from another context.
    fn plan_join<C>(self, ctx: &mut C, e: <Context<Ctx> as SplitInto<C>>::Extra) -> Self::Output
    where
        Self: Sized,
        Context<Ctx>: SplitInto<C>,
    {
        ctx.call_mut(move |ctx| {
            let mut ctx2 = Context::<Ctx>::join(ctx, e);
            let ret = self.plan(&mut ctx2);
            let (ctx, _) = ctx2.split();
            (ctx, ret)
        })
    }
}

impl<'a, T, Ctx> Plan<Ctx> for &'a std::rc::Rc<T>
where
    &'a T: Plan<Ctx>,
{
    type Output = <&'a T as Plan<Ctx>>::Output;

    fn plan(self, ctx: &mut Context<Ctx>) -> Self::Output {
        self.as_ref().plan(ctx)
    }
}

/// Runtime context.
#[derive(Debug)]
pub struct Context<Inner = ()> {
    /// The task manager.
    pub task: TaskManager,
    /// The command interface.
    pub cmd: Commands,
    /// The logging interface.
    pub log: Log,
    /// The storage interface.
    pub store: Store,
    /// The traits interface.
    pub traits: Traits,
    /// An inner context type.
    pub inner: Inner,
}

/// A builder for a Context.
#[derive(Default)]
pub struct ContextBuilder {
    logger: Option<LoggerRef>,
    store_dir: Option<std::path::PathBuf>,
}

/// An error produced by the ContextBuilder.
#[derive(Debug)]
pub enum BuilderError {
    TaskManagerError(futures::io::Error),
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TaskManagerError(e) => write!(f, "task manager error: {}", e),
        }
    }
}

impl ContextBuilder {
    /// Set the logger to use.
    pub fn logger<T: LogTarget + Send + 'static>(mut self, logger: T) -> Self {
        self.logger = Some(logger_ref(logger));
        self
    }

    /// Set the logger to use by a LoggerRef.
    pub fn logger_ref(mut self, logger: LoggerRef) -> Self {
        self.logger = Some(logger);
        self
    }

    /// Set the storage directory.
    pub fn storage_directory(mut self, dir: std::path::PathBuf) -> Self {
        self.store_dir = Some(dir);
        self
    }

    /// Create a Context.
    pub fn build(self) -> Result<Context, BuilderError> {
        self.build_inner()
    }

    /// Create a Context with the default value of an inner type.
    pub fn build_inner<Inner>(self) -> Result<Context<Inner>, BuilderError>
    where
        Inner: Default,
    {
        self.build_with(Inner::default())
    }

    /// Create a Context with an inner type.
    pub fn build_with<Inner>(self, inner: Inner) -> Result<Context<Inner>, BuilderError> {
        Ok(Context {
            task: TaskManager::new().map_err(BuilderError::TaskManagerError)?,
            cmd: Commands::new(),
            log: Log::new(self.logger.unwrap_or_else(|| logger_ref(EmptyLogTarget))),
            store: Store::new(self.store_dir.unwrap_or(std::env::temp_dir())),
            traits: Traits::new(),
            inner,
        })
    }
}

impl Context {
    /// Create a ContextBuilder.
    pub fn builder() -> ContextBuilder {
        Default::default()
    }
}

impl<T> Context<T> {
    /// Plan a type with this context.
    pub fn plan<P: Plan<T>>(&mut self, plan: P) -> <P as Plan<T>>::Output {
        plan.plan(self)
    }

    /// Plan a type with this split context.
    pub fn plan_split<P, Ctx>(&mut self, plan: P) -> <P as Plan<Ctx>>::Output
    where
        P: Plan<Ctx>,
        T: SplitInto<Ctx>,
    {
        plan.plan_split(self)
    }

    /// Plan a type by joining this context with extra data to form the target context.
    pub fn plan_join<P, Ctx>(&mut self, plan: P, e: Ctx::Extra) -> <P as Plan<Ctx>>::Output
    where
        P: Plan<Ctx>,
        Ctx: SplitInto<T>,
    {
        plan.plan_join(self, e)
    }
}

impl<T> std::ops::Deref for Context<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for Context<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}
