//! The runtime context.

use crate::abi_stable::StableAbi;
use crate::{
    type_system::{ErgoTrait, ErgoType, Type},
    Source, TypedValue, Value,
};
use std::fmt;

mod dynamic_scope;
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
pub use dynamic_scope::DynamicScope;
pub use shared_state::SharedState;
pub use store::{Item, ItemContent, ItemName, Store};
pub use task::{
    get_task_local, scope_task_local, thread_id, ScopeTaskLocal, TaskLocal, TaskLocalRef,
    TaskManager, TaskPermit,
};
pub use traits::{TraitGenerator, TraitGeneratorByTrait, TraitGeneratorByType, Traits};

/// Runtime context.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Context {
    /// The dynamic scoping interface.
    pub dynamic_scope: DynamicScope,
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

/// A builder for a Context.
#[derive(Default)]
pub struct ContextBuilder {
    logger: Option<LoggerRef>,
    store_dir: Option<std::path::PathBuf>,
    threads: Option<usize>,
    aggregate_errors: Option<bool>,
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

    /// Create a Context.
    pub fn build(self) -> Result<Context, BuilderError> {
        Ok(Context {
            dynamic_scope: Default::default(),
            log: Log::new(
                self.logger
                    .unwrap_or_else(|| logger_ref(EmptyLogTarget).into()),
            ),
            shared_state: SharedState::new(),
            store: Store::new(self.store_dir.unwrap_or(std::env::temp_dir())),
            task: TaskManager::new(self.threads, self.aggregate_errors.unwrap_or(false))
                .map_err(BuilderError::TaskManagerError)?,
            traits: Default::default(),
        })
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
        self.traits.get::<Trt>(v).map(|imp| Trt::create(imp, self))
    }

    /// Get an ergo trait for the given type.
    pub fn get_trait_for_type<Trt: ErgoTrait>(&self, tp: &Type) -> Option<Trt> {
        self.traits
            .get_type::<Trt>(tp)
            .map(|imp| Trt::create(imp, self))
    }

    /// Evaluate a value.
    pub async fn eval(&self, value: &mut Value) -> crate::Result<()> {
        value.eval(self).await;
        crate::try_value!(value.clone());
        Ok(())
    }

    /// Evaluate a value once.
    pub async fn eval_once(&self, value: &mut Value) {
        value.eval_once(self).await
    }

    /// Evaluate all values, returning whether they all evaluated to non-Error values.
    pub async fn eval_all<'a, I>(&self, values: I) -> bool
    where
        I: IntoIterator<Item = &'a mut Value>,
    {
        self.task
            .join_all(values.into_iter().map(|v| async move {
                v.eval(self).await;
                !v.is_type::<crate::types::Error>()
            }))
            .await
    }

    /// Evaluate a value expecting a specific type, returning an error if it does not evaluate to a
    /// value of that type or if it evaluates to an Error type.
    pub async fn eval_as<T: ErgoType>(
        &self,
        value: Source<Value>,
    ) -> crate::Result<Source<TypedValue<T>>> {
        let (source, mut value) = value.take();
        self.eval(&mut value).await?;
        match value.as_type::<T>() {
            Ok(v) => Ok(source.with(v)),
            Err(e) => Err(crate::traits::type_error_for::<T>(self, source.with(e))),
        }
    }

    /// Create a new context with the given dynamic binding set.
    pub fn with_dynamic_binding(&self, key: Source<Value>, value: Source<Value>) -> Self {
        let mut ctx = self.clone();
        ctx.dynamic_scope.set(key, value);
        ctx
    }
}