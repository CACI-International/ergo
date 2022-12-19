//! ABI-stable interfaces for log crate loggers.

use abi_stable::{
    sabi_trait,
    sabi_trait::*,
    sabi_types::RRef,
    std_types::{RCowStr, ROption, RStr},
    StableAbi,
};

/// An abi-stable value implementing log::Log.
#[derive(StableAbi)]
#[repr(C)]
pub struct Log(LogInterface_TO<'static, RRef<'static, ()>>);

static mut ABI_STABLE_LOGGER: Option<Log> = None;

/// Get the abi-stable logger.
pub fn logger() -> Option<Log> {
    match unsafe { &ABI_STABLE_LOGGER } {
        None => None,
        Some(l) => Some(Log(l.0.sabi_reborrow())),
    }
}

pub use log::{max_level, set_max_level, LevelFilter};

/// Set the abi-stable logger, also setting the `log` crate logger.
pub fn set_boxed_logger<L: log::Log + 'static>(logger: Box<L>) -> Result<(), log::SetLoggerError> {
    let ptr = Box::leak(logger);
    log::set_logger(ptr)?;
    // Safety: relies on the guarantees of the above `log::set_logger` call failing to ensure only one write occurs.
    unsafe { ABI_STABLE_LOGGER = Some(Log::new(ptr)) };
    Ok(())
}

#[derive(Clone, StableAbi)]
#[repr(C)]
struct Metadata<'a> {
    level: Level,
    target: RStr<'a>,
}

#[derive(StableAbi)]
#[repr(C)]
struct Record<'a> {
    metadata: Metadata<'a>,
    message: RCowStr<'a>,
    module_path: MaybeStaticStr<'a>,
    file: MaybeStaticStr<'a>,
    line: ROption<u32>,
}

#[derive(StableAbi)]
#[repr(C)]
enum MaybeStaticStr<'a> {
    Static(RStr<'static>),
    Borrowed(RStr<'a>),
    None,
}

#[derive(Clone, Copy, StableAbi)]
#[repr(usize)]
enum Level {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}

impl From<log::Level> for Level {
    fn from(l: log::Level) -> Self {
        match l {
            log::Level::Error => Self::Error,
            log::Level::Warn => Self::Warn,
            log::Level::Info => Self::Info,
            log::Level::Debug => Self::Debug,
            log::Level::Trace => Self::Trace,
        }
    }
}

impl From<Level> for log::Level {
    fn from(l: Level) -> Self {
        match l {
            Level::Error => Self::Error,
            Level::Warn => Self::Warn,
            Level::Info => Self::Info,
            Level::Debug => Self::Debug,
            Level::Trace => Self::Trace,
        }
    }
}

impl<'a> From<log::Metadata<'a>> for Metadata<'a> {
    fn from(m: log::Metadata<'a>) -> Self {
        Metadata {
            level: m.level().into(),
            target: m.target().into(),
        }
    }
}

impl<'a> From<Metadata<'a>> for log::Metadata<'a> {
    fn from(m: Metadata<'a>) -> Self {
        log::Metadata::builder()
            .level(m.level.into())
            .target(m.target.into())
            .build()
    }
}

impl<'a> From<log::Record<'a>> for Record<'a> {
    fn from(r: log::Record<'a>) -> Self {
        Record {
            metadata: r.metadata().clone().into(),
            message: match r.args().as_str() {
                Some(s) => s.into(),
                None => r.args().to_string().into(),
            },
            module_path: match r.module_path_static() {
                Some(s) => MaybeStaticStr::Static(s.into()),
                None => match r.module_path() {
                    Some(s) => MaybeStaticStr::Borrowed(s.into()),
                    None => MaybeStaticStr::None,
                },
            },
            file: match r.file_static() {
                Some(s) => MaybeStaticStr::Static(s.into()),
                None => match r.file() {
                    Some(s) => MaybeStaticStr::Borrowed(s.into()),
                    None => MaybeStaticStr::None,
                },
            },
            line: r.line().into(),
        }
    }
}

impl<'a> Record<'a> {
    pub fn with_record<F: FnOnce(&log::Record)>(&self, f: F) {
        let mut builder = log::Record::builder();
        builder.metadata(self.metadata.clone().into());
        match self.module_path {
            MaybeStaticStr::Static(s) => {
                builder.module_path_static(Some(s.into()));
            }
            MaybeStaticStr::Borrowed(s) => {
                builder.module_path(Some(s.into()));
            }
            MaybeStaticStr::None => (),
        }
        match self.file {
            MaybeStaticStr::Static(s) => {
                builder.file_static(Some(s.into()));
            }
            MaybeStaticStr::Borrowed(s) => {
                builder.file(Some(s.into()));
            }
            MaybeStaticStr::None => (),
        }
        builder.line(self.line.into());
        f(&builder.args(format_args!("{}", self.message)).build())
    }
}

#[sabi_trait]
trait LogInterface: Sync + Send {
    fn enabled(&self, metadata: Metadata<'_>) -> bool;
    fn log(&self, record: Record<'_>);
    fn flush(&self);
}

impl Log {
    pub fn new<L: log::Log + 'static>(l: &'static L) -> Self {
        Log(LogInterface_TO::from_ptr(RRef::new(l), TD_Opaque))
    }
}

impl<L: log::Log> LogInterface for L {
    fn enabled(&self, metadata: Metadata<'_>) -> bool {
        log::Log::enabled(self, &metadata.into())
    }

    fn log(&self, record: Record<'_>) {
        record.with_record(|r| log::Log::log(self, r))
    }

    fn flush(&self) {
        log::Log::flush(self)
    }
}

impl log::Log for Log {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        self.0.enabled(metadata.clone().into())
    }

    fn log(&self, record: &log::Record<'_>) {
        self.0.log(record.clone().into())
    }

    fn flush(&self) {
        self.0.flush()
    }
}
