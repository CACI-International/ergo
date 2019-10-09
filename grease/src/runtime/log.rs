//! Procedure logging.

use std::fmt;
use std::fmt::Arguments;
use std::sync::{Arc, Mutex};

use crate::plan::TaskId;

/// Log output levels.
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

/// A trait for the runtime log target.
///
/// This target is set as part of runtime instantiation. It is used for task runtime logging, not
/// executable runtime logging.
pub trait LogTarget {
    fn log<'a>(&mut self, level: LogLevel, task: TaskId, context: &[String], args: Arguments<'a>);
}

pub type LoggerRef = Arc<Mutex<dyn LogTarget>>;

/// The logging interface.
#[derive(Clone)]
pub struct Log {
    logger: LoggerRef,
    task: TaskId,
    context: Vec<String>,
}

macro_rules! log_level {
    ( $name:ident, $level:ident ) => {
        pub fn $name<'a>(&self, args: Arguments<'a>) {
            let mut l = self.logger.lock().unwrap();
            l.log(LogLevel::$level, self.task, &self.context, args);
        }
    }
}

impl Log {
    pub(crate) fn new(logger: LoggerRef, task: TaskId) -> Self {
        Log {
            logger,
            task,
            context: Vec::new(),
        }
    }

    pub(crate) fn for_task(&self, task: TaskId) -> Self {
        Log::new(self.logger.clone(), task)
    }

    /// Create a sublog interface with the given context identifier.
    pub fn sublog(&self, name: String) -> Self {
        let mut ret = self.clone();
        ret.context.push(name);
        ret
    }

    /// Write a debug log.
    log_level!(debug, Debug);
    /// Write an info log.
    log_level!(info, Info);
    /// Write a warning log.
    log_level!(warn, Warn);
    /// Write an error log.
    log_level!(error, Error);
}

impl fmt::Debug for Log {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Log")
            .field("task", &self.task)
            .field("context", &self.context)
            .finish()
    }
}

