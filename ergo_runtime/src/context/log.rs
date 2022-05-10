//! Runtime logging.

// For generated sabi trait code with LogTaskKey.
#![allow(improper_ctypes_definitions)]

use crate::abi_stable::{
    external_types::RMutex,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, RDuration, ROption, RSlice, RString, RVec},
    type_erase::Erased,
    DynTrait, StableAbi,
};
use std::fmt;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Log output levels.
#[derive(Debug, PartialEq, PartialOrd, StableAbi)]
#[repr(u8)]
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

#[derive(Debug)]
pub enum ParseLogLevelError {
    InvalidLevel(String),
}

impl fmt::Display for ParseLogLevelError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::InvalidLevel(s) => write!(f, "\"{}\" unrecognized", s),
        }
    }
}

impl std::error::Error for ParseLogLevelError {}

impl std::str::FromStr for LogLevel {
    type Err = ParseLogLevelError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "debug" => Ok(LogLevel::Debug),
            "info" => Ok(LogLevel::Info),
            "warn" => Ok(LogLevel::Warn),
            "error" => Ok(LogLevel::Error),
            _ => Err(ParseLogLevelError::InvalidLevel(s.to_lowercase())),
        }
    }
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
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct LogEntry {
    /// The log level.
    pub level: LogLevel,
    /// Additional log context.
    pub context: RVec<RString>,
    /// The log string.
    pub args: RString,
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

#[derive(StableAbi)]
#[repr(C)]
#[sabi(impl_InterfaceType(Send))]
struct LogTaskKeyInterface;

#[derive(StableAbi)]
#[repr(C)]
pub struct LogTaskKey(DynTrait<'static, RBox<()>, LogTaskKeyInterface>);

impl LogTaskKey {
    /// Create a new LogTaskKey.
    pub fn new<T: Send + 'static>(key: T) -> Self {
        LogTaskKey(DynTrait::from_any_value(key, LogTaskKeyInterface))
    }

    /// Recover a typed value from the LogTaskKey.
    ///
    /// This will panic if the type does not match the stored type.
    pub fn into<T: 'static>(self) -> Result<T, Self> {
        self.0
            .downcast_into()
            .map(RBox::into_inner)
            .map_err(|e| LogTaskKey(e.into_inner()))
    }
}

/// A trait for the runtime log target.
///
/// This target is set as part of runtime instantiation. It is used for task runtime logging, not
/// executable runtime logging.
#[sabi_trait]
pub trait LogTarget: Send {
    /// Send a log entry.
    fn log(&mut self, entry: LogEntry);

    /// Indicate a task is running.
    fn task_running(&mut self, _description: RString) -> LogTaskKey {
        LogTaskKey::new(())
    }

    /// Incidate a task is no longer running.
    ///
    /// This does not necessarily mean the task is complete.
    fn task_suspend(&mut self, _key: LogTaskKey) {}

    /// Indicates a unique timer for the given id has been created.
    fn timer_pending(&mut self, _id: RSlice<RString>) {}

    /// Indicates that a unique timer for the given id has completed with the given total duration.
    /// If the timer is cancelled, the duration will be None.
    fn timer_complete(&mut self, _id: RSlice<RString>, _duration: ROption<RDuration>) {}

    /// Pause log output.
    fn pause_logging(&mut self) {}

    /// Resume log output.
    #[sabi(last_prefix_field)]
    fn resume_logging(&mut self) {}
}

pub type Logger = RMutex<LogTarget_TO<'static, RBox<()>>>;

/// A reference to a LogTarget.
pub type LoggerRef = RArc<Logger>;

/// A LogTarget that does nothing.
#[derive(StableAbi)]
#[repr(C)]
pub struct EmptyLogTarget;

impl LogTarget for EmptyLogTarget {
    fn log(&mut self, _entry: LogEntry) {}
}

/// Create a logger reference.
pub fn logger_ref<T: LogTarget + 'static>(target: T) -> Arc<Logger> {
    Arc::new(RMutex::new(LogTarget_TO::from_value(target, TD_CanDowncast)))
}

/// The logging interface.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Log {
    logger: LoggerRef,
    context: RVec<RString>,
}

/// Tracks a particular unit of work.
#[derive(StableAbi)]
#[repr(C)]
pub struct Work {
    logger: LoggerRef,
    id: RVec<RString>,
    duration: RArc<RMutex<RDuration>>,
    recorded: bool,
    errored: bool,
}

pub struct PausedLog {
    logger: LoggerRef,
}

struct RecordingWorkInner {
    target: RArc<RMutex<RDuration>>,
    at: Instant,
}

/// Tracks a unit of work that is currently active.
///
/// When dropped, the duration of work is recorded.
#[derive(StableAbi)]
#[repr(C)]
pub struct RecordingWork(Erased);

macro_rules! log_level {
    ( $name:ident, $level:ident ) => {
        /// Write a log message.
        pub fn $name<T: std::string::ToString>(&self, message: T) {
            let mut l = self.logger.lock();
            l.log(LogEntry {
                level: LogLevel::$level,
                context: self.context.clone(),
                args: message.to_string().into(),
            });
        }
    };
}

/// A log task which will suspend the task when dropped.
#[derive(StableAbi)]
#[repr(C)]
pub struct LogTask {
    logger: LoggerRef,
    key: LogTaskKey,
}

impl Drop for LogTask {
    fn drop(&mut self) {
        let key = std::mem::replace(&mut self.key, LogTaskKey::new(()));
        self.logger.lock().task_suspend(key);
    }
}

impl Log {
    pub(crate) fn new(logger: LoggerRef) -> Self {
        Log {
            logger,
            context: RVec::default(),
        }
    }

    /// Create a sublog interface with the given context identifier.
    pub fn sublog<T: Into<String>>(&self, name: T) -> Self {
        let mut ret = self.clone();
        ret.context.push(name.into().into());
        ret
    }

    /// Create and track a unit of work.
    ///
    /// The returned object should be used to record work runtime.
    pub fn work<T: Into<String>>(&self, name: T) -> Work {
        let mut v: Vec<_> = self.context.iter().map(|v| v.clone()).collect();
        v.push(name.into().into());
        let w = Work {
            logger: self.logger.clone(),
            id: v.into(),
            duration: RArc::new(RMutex::new(RDuration::new(0, 0))),
            recorded: false,
            errored: false,
        };
        {
            let mut l = w.logger.lock();
            l.timer_pending(RSlice::from_slice(w.id.as_ref()));
        }
        w
    }

    /// Pause logging.
    pub fn pause(&self) -> PausedLog {
        self.logger.lock().pause_logging();
        PausedLog {
            logger: self.logger.clone(),
        }
    }

    /// Add a running task to the log.
    ///
    /// When the returned OwnedLogTask is dropped, the task is suspended from the log.
    pub fn task<T: ToString>(&self, description: T) -> LogTask {
        let key = self
            .logger
            .lock()
            .task_running(description.to_string().into());
        LogTask {
            logger: self.logger.clone(),
            key,
        }
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

impl Work {
    /// Start the unit of work.
    pub fn start(&mut self) -> RecordingWork {
        self.recorded = true;
        RecordingWork(Erased::new(RecordingWorkInner {
            target: self.duration.clone(),
            at: Instant::now(),
        }))
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
            .field("duration", &*self.duration.lock())
            .finish()
    }
}

impl std::ops::Drop for Work {
    fn drop(&mut self) {
        let mut l = self.logger.lock();
        l.timer_complete(
            RSlice::from_slice(&self.id),
            if self.recorded {
                ROption::RSome(self.duration.lock().clone())
            } else {
                ROption::RNone
            },
        )
    }
}

impl std::ops::Drop for RecordingWorkInner {
    fn drop(&mut self) {
        let mut guard = self.target.lock();
        let mut duration: Duration = guard.clone().into();
        duration += self.at.elapsed();
        *guard = duration.into();
    }
}

impl std::ops::Drop for PausedLog {
    fn drop(&mut self) {
        self.logger.lock().resume_logging()
    }
}
