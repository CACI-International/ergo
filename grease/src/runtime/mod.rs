//! Execution logic for plans.

use std::fmt;

mod command;
mod log;
mod store;
mod task_manager;

use self::log::EmptyLogTarget;
pub use self::log::{logger_ref, Log, LogEntry, LogLevel, LogTarget, LoggerRef};
pub use command::Commands;
pub use store::{Item, ItemContent, ItemName, Store};
pub use task_manager::TaskManager;

/// A type which can be used for plan creation.
///
/// In general, Output should contain one or more Values for use in other plans.
pub trait Plan {
    /// The output type of planning.
    type Output;

    /// Create a plan given the context.
    fn plan(&self, ctx: &mut Context) -> Self::Output;
}

/// Runtime context.
#[derive(Debug)]
pub struct Context {
    /// The task manager.
    pub task: TaskManager,
    /// The command interface.
    pub cmd: Commands,
    /// The logging interface.
    pub log: Log,
    /// The storage interface.
    pub store: Store,
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
    pub fn logger<T: LogTarget + Send + 'static>(mut self, logger: T) -> Self {
        self.logger = Some(logger_ref(logger));
        self
    }

    pub fn logger_ref(mut self, logger: LoggerRef) -> Self {
        self.logger = Some(logger);
        self
    }

    pub fn storage_directory(mut self, dir: std::path::PathBuf) -> Self {
        self.store_dir = Some(dir);
        self
    }

    pub fn build(self) -> Result<Context, BuilderError> {
        Ok(Context {
            task: TaskManager::new().map_err(BuilderError::TaskManagerError)?,
            cmd: Commands::new(),
            log: Log::new(self.logger.unwrap_or_else(|| logger_ref(EmptyLogTarget))),
            store: Store::new(self.store_dir.unwrap_or(std::env::temp_dir())),
        })
    }
}

impl Context {
    pub fn builder() -> ContextBuilder {
        Default::default()
    }
}
