//! Procedure logging.

use std::fmt;
use std::fmt::Arguments;
use std::sync::{Arc, Mutex};

use crate::plan::TaskId;

/// Log output levels.
#[derive(Debug, std::cmp::PartialEq, std::cmp::PartialOrd)]
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", match self {
            LogLevel::Debug => "debug",
            LogLevel::Info => "info",
            LogLevel::Warn => "warn",
            LogLevel::Error => "error"
        })
    }
}

/// A single log entry.
pub struct LogEntry {
    /// The log level.
    pub level: LogLevel,
    /// The task which produced the log.
    pub task: TaskId,
    /// Additional log context.
    pub context: Arc<[String]>,
    /// The log string.
    pub args: String,
}

impl fmt::Display for LogEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(),fmt::Error> {
        write!(f, "{}[{}]", self.level, self.task)?;
        let has_context = self.context.len() > 0;
        if has_context {
            write!(f, " ({}", self.context.get(0).unwrap())?;
        }
        for s in self.context.iter().skip(1) {
            write!(f, "/{}", s)?;
        }
        if has_context {
            write!(f, ")")?;
        }
        write!(f, ": {}", self.args)
    }
}

/// A trait for the runtime log target.
///
/// This target is set as part of runtime instantiation. It is used for task runtime logging, not
/// executable runtime logging.
pub trait LogTarget {
    /// Send a log entry.
    fn log(&mut self, entry: LogEntry);

    /// A log was dropped.
    ///
    /// This is mainly useful for streaming outputs to know that a log completed.
    fn dropped(&mut self, _task: TaskId, _context: Arc<[String]>) {}
}

/// A heap-allocated reference to a LogTarget.
pub type LoggerRef = Arc<Mutex<dyn LogTarget + Send>>;

/// Create a logger reference.
pub fn logger_ref<T: LogTarget + Send + 'static>(target: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(target))
}

/// The logging interface.
#[derive(Clone)]
pub struct Log {
    logger: LoggerRef,
    task: TaskId,
    context: Arc<[String]>,
}

macro_rules! log_level {
    ( $name:ident, $level:ident ) => {
        pub fn $name<'a>(&self, args: Arguments<'a>) {
            let mut l = self.logger.lock().unwrap();
            l.log(LogEntry {
                level: LogLevel::$level,
                task: self.task,
                context: self.context.clone(),
                args: format!("{}", args)
            });
        }
    }
}

impl Log {
    pub(crate) fn new(logger: LoggerRef, task: TaskId) -> Self {
        Log {
            logger,
            task,
            context: Arc::new([]),
        }
    }

    pub(crate) fn for_task(&self, task: TaskId) -> Self {
        Log::new(self.logger.clone(), task)
    }

    /// Create a sublog interface with the given context identifier.
    pub fn sublog(&self, name: String) -> Self {
        let mut ret = self.clone();
        let mut v = Vec::from(ret.context.as_ref());
        v.push(name);
        ret.context = Arc::from(v);
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

impl std::ops::Drop for Log {
    fn drop(&mut self) {
        let mut l = self.logger.lock().unwrap();
        l.dropped(self.task, self.context.clone());
    }
}
