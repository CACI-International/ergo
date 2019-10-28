//! Runtime logging.

use std::fmt;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

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
        write!(
            f,
            "{}",
            match self {
                LogLevel::Debug => "debug",
                LogLevel::Info => "info",
                LogLevel::Warn => "warn",
                LogLevel::Error => "error",
            }
        )
    }
}

/// A single log entry.
pub struct LogEntry {
    /// The log level.
    pub level: LogLevel,
    /// Additional log context.
    pub context: Arc<[String]>,
    /// The log string.
    pub args: String,
}

impl fmt::Display for LogEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.level)?;
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
    fn dropped(&mut self, _context: Arc<[String]>) {}

    /// Indicates a unique timer for the given id has been created.
    fn timer_pending(&mut self, _id: &[String]) {}

    /// Indicates that a unique timer for the given id has completed with the given total duration.
    fn timer_complete(&mut self, _id: &[String], _duration: std::time::Duration) {}
}

/// A heap-allocated reference to a LogTarget.
pub type LoggerRef = Arc<Mutex<dyn LogTarget + Send>>;

/// A LogTarget that does nothing.
pub struct EmptyLogTarget;

impl LogTarget for EmptyLogTarget {
    fn log(&mut self, _entry: LogEntry) {}
}

/// Create a logger reference.
pub fn logger_ref<T: LogTarget + Send + 'static>(target: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(target))
}

/// The logging interface.
#[derive(Clone)]
pub struct Log {
    logger: LoggerRef,
    context: Arc<[String]>,
}

/// Tracks a particular unit of work.
pub struct Work {
    logger: LoggerRef,
    id: Box<[String]>,
    duration: Duration,
    errored: bool,
}

/// Tracks a unit of work that is currently occurring.
///
/// When dropped, the duration of work is recorded.
#[derive(Debug)]
pub struct RecordingWork<'a> {
    work: &'a mut Work,
    at: Instant,
}

macro_rules! log_level {
    ( $name:ident, $level:ident ) => {
        /// Write a log message.
        pub fn $name<T: std::string::ToString>(&self, message: T) {
            let mut l = self.logger.lock().unwrap();
            l.log(LogEntry {
                level: LogLevel::$level,
                context: self.context.clone(),
                args: message.to_string()
            });
        }
    }
}

impl Log {
    pub(crate) fn new(logger: LoggerRef) -> Self {
        Log {
            logger,
            context: Arc::new([]),
        }
    }

    /// Create a sublog interface with the given context identifier.
    pub fn sublog(&self, name: String) -> Self {
        let mut ret = self.clone();
        let mut v = Vec::from(ret.context.as_ref());
        v.push(name);
        ret.context = Arc::from(v);
        ret
    }

    /// Create and track a unit of work.
    ///
    /// The returned object should be used to record work runtime.
    pub fn work(&self, name: String) -> Work {
        let mut v = Vec::from(self.context.as_ref());
        v.push(name);
        let w = Work {
            logger: self.logger.clone(),
            id: Box::from(v),
            duration: Duration::default(),
            errored: false,
        };
        {
            let mut l = w.logger.lock().unwrap();
            l.timer_pending(w.id.as_ref());
        }
        w
    }

    log_level!(debug, Debug);
    log_level!(info, Info);
    log_level!(warn, Warn);
    log_level!(error, Error);
}

impl fmt::Debug for Log {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Log")
            .field("context", &self.context)
            .finish()
    }
}

impl std::ops::Drop for Log {
    fn drop(&mut self) {
        let mut l = self.logger.lock().unwrap();
        l.dropped(self.context.clone());
    }
}

impl Work {
    /// Start the unit of work.
    pub fn start(&mut self) -> RecordingWork {
        RecordingWork {
            work: self,
            at: Instant::now(),
        }
    }

    /// Indicate the unit of work errored.
    pub fn err(&mut self) {
        self.errored = true;
    }
}

impl fmt::Debug for Work {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Work")
            .field("id", &self.id)
            .field("duration", &self.duration)
            .finish()
    }
}

impl std::ops::Drop for Work {
    fn drop(&mut self) {
        let mut l = self.logger.lock().unwrap();
        l.timer_complete(&self.id, self.duration);
    }
}

impl std::ops::Drop for RecordingWork<'_> {
    fn drop(&mut self) {
        self.work.duration += self.at.elapsed();
    }
}
