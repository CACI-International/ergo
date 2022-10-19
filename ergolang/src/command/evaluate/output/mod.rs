use crate::command::evaluate::OutputFormat;
use ergo_runtime::abi_stable::std_types::{RDuration, ROption, RSlice, RString};
use ergo_runtime::context::{LogEntry, LogLevel, LogTarget, LogTaskKey};
use ergo_runtime::Error;

mod interface;
mod plain;
mod terminal;

pub use interface::TermToTermcolor;

pub trait Output {
    fn set_log_level(&mut self, log_level: LogLevel);
    fn new_error(&mut self, err: Error);
    fn interrupt(&mut self);
    fn indicate_progress(&mut self);
    fn update(&mut self);
    fn take_errors(&mut self) -> ergo_runtime::error::Diagnostics;

    fn log(&mut self, _entry: LogEntry) {}
    fn task_running(&mut self, _key: LogTaskKey, _description: RString) {}
    fn task_suspend(&mut self, _key: LogTaskKey) {}
    fn timer_pending(&mut self, _id: RSlice<RString>) {}
    fn timer_complete(&mut self, _id: RSlice<RString>, _duration: ROption<RDuration>) {}
    fn pause_logging(&mut self) {}
    fn resume_logging(&mut self) {}
}

/// Returns the output instance, the associated log instance, and whether a terminal was detected.
pub fn output(format: OutputFormat, keep_going: bool) -> Option<(OutputInstance, bool)> {
    use interface::OutputType::*;
    interface::stdout(format).map(|v| match v {
        Term(term_output) => (terminal::Output::new(term_output, keep_going).into(), true),
        Dumb(w) => (plain::Output::new(w).into(), false),
    })
}

pub fn error(format: OutputFormat) -> Option<Box<term::StderrTerminal>> {
    interface::stderr(format)
}

pub struct OutputInstance {
    inner: super::sync::ThreadedMut<Box<dyn Output>>,
}

macro_rules! impl_from {
    ( $t:ty ) => {
        impl From<$t> for OutputInstance {
            fn from(v: $t) -> Self {
                OutputInstance {
                    inner: super::sync::ThreadedMut::new(Box::new(v)),
                }
            }
        }
    };
}

impl_from!(terminal::Output);
impl_from!(plain::Output);

impl OutputInstance {
    pub fn log(&self) -> OutputLog {
        OutputLog::new(self.inner.mutator().clone())
    }

    pub fn error_log(&self) -> OutputErrorLog {
        OutputErrorLog::new(self.inner.mutator().clone())
    }
}

impl Output for OutputInstance {
    fn set_log_level(&mut self, log_level: LogLevel) {
        self.inner.set_log_level(log_level)
    }

    fn new_error(&mut self, err: Error) {
        self.inner.new_error(err)
    }

    fn interrupt(&mut self) {
        self.inner.interrupt()
    }

    fn indicate_progress(&mut self) {
        self.inner.indicate_progress()
    }

    fn update(&mut self) {
        self.inner.flush();
        self.inner.update()
    }

    fn take_errors(&mut self) -> ergo_runtime::error::Diagnostics {
        self.inner.take_errors()
    }

    fn log(&mut self, entry: LogEntry) {
        self.inner.log(entry)
    }

    fn task_running(&mut self, key: LogTaskKey, description: RString) {
        self.inner.task_running(key, description)
    }

    fn task_suspend(&mut self, key: LogTaskKey) {
        self.inner.task_suspend(key)
    }

    fn timer_pending(&mut self, id: RSlice<RString>) {
        self.inner.timer_pending(id)
    }

    fn timer_complete(&mut self, id: RSlice<RString>, duration: ROption<RDuration>) {
        self.inner.timer_complete(id, duration)
    }

    fn pause_logging(&mut self) {
        self.inner.pause_logging()
    }

    fn resume_logging(&mut self) {
        self.inner.resume_logging()
    }
}

impl Drop for OutputInstance {
    fn drop(&mut self) {
        self.update();
    }
}

/// A LogTarget associated with an Output.
pub struct OutputLog {
    inner: super::sync::Mutator<Box<dyn Output>>,
    next_task_key: std::sync::atomic::AtomicUsize,
}

impl OutputLog {
    pub fn new(mutator: super::sync::Mutator<Box<dyn Output>>) -> Self {
        OutputLog {
            inner: mutator,
            next_task_key: Default::default(),
        }
    }
}

impl LogTarget for OutputLog {
    fn log(&self, entry: LogEntry) {
        self.inner.mutate(move |o| o.log(entry));
    }

    fn task_running(&self, description: RString) -> LogTaskKey {
        let key = self
            .next_task_key
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.inner.mutate(move |o| o.task_running(key, description));
        key
    }

    fn task_suspend(&self, key: LogTaskKey) {
        self.inner.mutate(move |o| o.task_suspend(key));
    }

    fn timer_pending(&self, id: RSlice<RString>) {
        let id = id.to_vec();
        self.inner
            .mutate(move |o| o.timer_pending(RSlice::from(id.as_slice())));
    }

    fn timer_complete(&self, id: RSlice<RString>, duration: ROption<RDuration>) {
        let id = id.to_vec();
        self.inner
            .mutate(move |o| o.timer_complete(RSlice::from(id.as_slice()), duration));
    }

    fn pause_logging(&self) {
        self.inner.mutate(move |o| o.pause_logging());
    }

    fn resume_logging(&self) {
        self.inner.mutate(move |o| o.resume_logging());
    }
}

pub struct OutputErrorLog {
    inner: super::sync::Mutator<Box<dyn Output>>,
}

impl OutputErrorLog {
    pub fn new(mutator: super::sync::Mutator<Box<dyn Output>>) -> Self {
        OutputErrorLog { inner: mutator }
    }

    pub fn new_error(&self, err: Error) {
        self.inner.mutate(move |o| o.new_error(err));
    }
}
