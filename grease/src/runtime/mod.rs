//! Execution logic for plans.

use abi_stable::StableAbi;
use std::fmt;

pub mod io;
mod log;
mod store;
mod task_manager;
mod traits;

use self::log::EmptyLogTarget;
pub use self::log::{
    logger_ref, Log, LogEntry, LogLevel, LogTarget, LogTask, LogTaskKey, Logger, LoggerRef,
};
pub use store::{Item, ItemContent, ItemName, Store};
pub use task_manager::{
    get_task_local, scope_task_local, thread_id, ScopeTaskLocal, TaskLocal, TaskLocalRef,
    TaskManager, TaskPermit,
};
pub use traits::{Trait, TraitGenerator, TraitGeneratorByTrait, TraitGeneratorByType, Traits};

/// Runtime context.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Context {
    /// The task manager.
    pub task: TaskManager,
    /// The logging interface.
    pub log: Log,
    /// The storage interface.
    pub store: Store,
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
            task: TaskManager::new(self.threads, self.aggregate_errors.unwrap_or(false))
                .map_err(BuilderError::TaskManagerError)?,
            log: Log::new(
                self.logger
                    .unwrap_or_else(|| logger_ref(EmptyLogTarget).into()),
            ),
            store: Store::new(self.store_dir.unwrap_or(std::env::temp_dir())),
            traits: Default::default(),
        })
    }
}

impl Context {
    /// Create a ContextBuilder.
    pub fn builder() -> ContextBuilder {
        Default::default()
    }

    /// Get a grease trait for the given value's type.
    pub async fn get_trait<Trt: crate::traits::GreaseTrait, F, Fut>(
        &self,
        v: &crate::value::Value,
        otherwise: F,
    ) -> crate::Result<Trt>
    where
        F: FnOnce(&crate::types::Type) -> Fut + Send + 'static,
        Fut: std::future::Future<Output = crate::Result<Trt::Impl>> + Send,
    {
        self.traits
            .get::<Trt, F, Fut>(v, otherwise)
            .await
            .map(|imp| Trt::create(imp, self))
    }

    /// Get a grease trait for the given type.
    pub fn get_trait_for_type<Trt: crate::traits::GreaseTrait>(
        &self,
        tp: &crate::types::Type,
    ) -> Option<Trt> {
        self.traits
            .get_type::<Trt>(tp)
            .map(|imp| Trt::create(imp.into(), self))
    }
}
