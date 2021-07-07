//! The runtime context.

use crate::abi_stable::{std_types::RArc, StableAbi};
use crate::{
    type_system::{ErgoTrait, ErgoType, Type},
    TypedValue, Value,
};
use std::fmt;

mod dynamic_scope;
mod error_scope;
mod log;
mod shared_state;
mod store;
mod task;
mod traits;

use self::log::EmptyLogTarget;

/// Create a literal item name.
///
/// Item names must contain only ascii alphanumeric characters.
pub use ergo_runtime_macro::item_name;

pub use self::log::{
    logger_ref, Log, LogEntry, LogLevel, LogTarget, LogTask, LogTaskKey, Logger, LoggerRef,
    RecordingWork, Work,
};
pub use dynamic_scope::{DynamicScope, DynamicScopeKey, DynamicScopeRef};
pub use error_scope::ErrorScope;
pub use shared_state::SharedState;
pub use store::{Item, ItemContent, ItemName, Store};
pub use task::{
    get_task_local, scope_task_local, thread_id, ScopeTaskLocal, TaskLocal, TaskLocalRef,
    TaskManager, TaskPermit,
};
pub use traits::{TraitGenerator, TraitGeneratorByTrait, TraitGeneratorByType, Traits};

/// Runtime context which is immutable.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct GlobalContext {
    /// The logging interface.
    pub log: Log,
    /// The shared state interface.
    pub shared_state: SharedState,
    /// The storage interface.
    pub store: Store,
    /// The task management interface.
    pub task: TaskManager,
    /// The type traits interface.
    pub traits: Traits,
}

/// Runtime context.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct Context {
    global: RArc<GlobalContext>,
    /// The dynamic scoping interface.
    pub dynamic_scope: DynamicScope,
    /// The error propagation interface.
    pub error_scope: ErrorScope,
}

/// A builder for a Context.
#[derive(Default)]
pub struct ContextBuilder {
    logger: Option<LoggerRef>,
    store_dir: Option<std::path::PathBuf>,
    threads: Option<usize>,
    aggregate_errors: Option<bool>,
    error_scope: Option<ErrorScope>,
}

trait Fork {
    /// Create a fork of a value.
    fn fork(&self) -> Self;

    /// Join with a forked value.
    fn join(&self, forked: Self);
}

impl<T: Clone> Fork for T {
    fn fork(&self) -> Self {
        self.clone()
    }

    fn join(&self, _forked: Self) {}
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

impl std::error::Error for BuilderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BuilderError::TaskManagerError(e) => Some(e),
        }
    }
}

impl ContextBuilder {
    /// Set the logger to use.
    pub fn logger<T: LogTarget + Send + 'static>(mut self, logger: T) -> Self {
        let logger = logger_ref(logger);
        self.logger = Some(logger.into());
        self
    }

    /// Set the storage directory.
    pub fn storage_directory(mut self, dir: std::path::PathBuf) -> Self {
        self.store_dir = Some(dir);
        self
    }

    /// Set the number of threads to use.
    pub fn threads(mut self, threads: Option<usize>) -> Self {
        self.threads = threads;
        self
    }

    /// Set whether a single error causes immediate completion or not.
    /// Default is false.
    pub fn keep_going(mut self, value: bool) -> Self {
        self.aggregate_errors = Some(value);
        self
    }

    /// Set the top-level error handler of the context.
    pub fn error_handler<F>(mut self, on_error: F) -> Self
    where
        F: Fn(crate::Error) + Send + Sync + 'static,
    {
        self.error_scope = Some(ErrorScope::new(on_error));
        self
    }

    /// Create a Context.
    pub fn build(self) -> Result<Context, BuilderError> {
        Ok(Context {
            global: RArc::new(GlobalContext {
                log: Log::new(
                    self.logger
                        .unwrap_or_else(|| logger_ref(EmptyLogTarget).into()),
                ),
                shared_state: SharedState::new(),
                store: Store::new(self.store_dir.unwrap_or(std::env::temp_dir())),
                task: TaskManager::new(self.threads, self.aggregate_errors.unwrap_or(false))
                    .map_err(BuilderError::TaskManagerError)?,
                traits: Default::default(),
            }),
            dynamic_scope: Default::default(),
            error_scope: self.error_scope.unwrap_or_default(),
        })
    }
}

impl std::ops::Deref for Context {
    type Target = GlobalContext;

    fn deref(&self) -> &Self::Target {
        &self.global
    }
}

impl Context {
    /// Create a ContextBuilder.
    pub fn builder() -> ContextBuilder {
        Default::default()
    }

    /// Get an ergo trait for the given value's type.
    ///
    /// Always returns None if the value is not yet evaluated.
    pub fn get_trait<Trt: ErgoTrait>(&self, v: &Value) -> Option<Trt> {
        self.traits.get::<Trt>(v).map(Trt::create)
    }

    /// Get an ergo trait for the given type.
    pub fn get_trait_for_type<Trt: ErgoTrait>(&self, tp: &Type) -> Option<Trt> {
        self.traits.get_type::<Trt>(tp).map(Trt::create)
    }

    /// Evaluate a value.
    pub async fn eval(&self, value: &mut Value) -> crate::Result<()> {
        value.eval(self).await;
        if value.is_type::<crate::types::Error>() {
            let error = value
                .clone()
                .as_type::<crate::types::Error>()
                .unwrap()
                .to_owned();
            self.error_scope.error(&error);
            return Err(error);
        }
        Ok(())
    }

    /// Evaluate a value once.
    pub async fn eval_once(&self, value: &mut Value) {
        value.eval_once(self).await
    }

    /// Evaluate all values.
    pub async fn eval_all<'a, I>(&self, values: I) -> crate::Result<()>
    where
        I: IntoIterator<Item = &'a mut Value>,
    {
        self.task
            .join_all(values.into_iter().map(|v| self.eval(v)))
            .await
            .map(|_| ())
    }

    /// Evaluate a value expecting a specific type, returning an error if it does not evaluate to a
    /// value of that type or if it evaluates to an Error type.
    pub async fn eval_as<T: ErgoType>(&self, mut value: Value) -> crate::Result<TypedValue<T>> {
        self.eval(&mut value).await?;
        match value.as_type::<T>() {
            Ok(v) => Ok(v),
            Err(e) => Err(crate::traits::type_error_for::<T>(self, e)),
        }
    }

    /*
    /// Create a new context with the given dynamic binding set.
    pub fn with_dynamic_binding<K: DynamicScopeKey>(
        &self,
        key: &Source<K>,
        value: K::Value,
    ) -> Self {
        let mut ctx = self.clone();
        ctx.dynamic_scope.set(key, value);
        ctx
    }

    /// Create a new context with the given error handler.
    pub fn with_error_handler<F>(&self, on_error: F) -> Self
    where
        F: Fn(crate::Error) + Send + Sync + 'static,
    {
        let mut ctx = self.clone();
        ctx.error_scope = ErrorScope::new(on_error);
        ctx
    }
    */

    /// Fork the context, allowing changes of context values.
    pub async fn fork<'a, Alter, F, Fut, Ret>(&self, alter: Alter, f: F) -> Ret
    where
        Alter: FnOnce(&mut Self),
        F: FnOnce(&'a Self) -> Fut,
        Fut: std::future::Future<Output = Ret> + Send + 'a,
    {
        let mut new_ctx = Context {
            global: self.global.fork(),
            dynamic_scope: self.dynamic_scope.fork(),
            error_scope: self.error_scope.fork(),
        };

        alter(&mut new_ctx);

        let ret = f(unsafe { std::mem::transmute(&new_ctx) }).await;
        self.global.join(new_ctx.global);
        self.dynamic_scope.join(new_ctx.dynamic_scope);
        self.error_scope.join(new_ctx.error_scope);
        ret
    }

    /// Spawn a new task.
    ///
    /// This necessarily forks the context.
    pub async fn spawn<'a, Alter, F, Fut, Ret>(
        &self,
        priority: u32,
        alter: Alter,
        f: F,
    ) -> crate::Result<Ret>
    where
        Alter: FnOnce(&mut Self),
        F: FnOnce(&'a Self) -> Fut + Send + 'static,
        Fut: std::future::Future<Output = crate::Result<Ret>> + Send + 'a,
        Ret: Send + 'static,
    {
        let mut new_ctx = Context {
            global: self.global.fork(),
            dynamic_scope: self.dynamic_scope.fork(),
            error_scope: self.error_scope.fork(),
        };
        alter(&mut new_ctx);
        match self
            .task
            .spawn_basic(priority, async move {
                let ret = f(unsafe { std::mem::transmute(&new_ctx) }).await;
                (new_ctx, ret)
            })
            .await
        {
            Ok((new_ctx, ret)) => {
                self.global.join(new_ctx.global);
                self.dynamic_scope.join(new_ctx.dynamic_scope);
                self.error_scope.join(new_ctx.error_scope);
                ret
            }
            // If the task is aborted, there's nothing relevant for the context to be joined.
            Err(_) => Err(crate::Error::aborted()),
        }
    }
}
