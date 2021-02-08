use abi_stable::std_types::{RDuration, ROption, RSlice, RString};
use grease::runtime::{LogEntry, LogLevel, LogTarget, LogTaskKey};

mod interface;
mod plain;
mod terminal;

pub trait Output: LogTarget {
    fn set_log_level(&mut self, log_level: LogLevel);
    fn new_error(&mut self, err: grease::Error);
    fn interrupt(&mut self);
}

pub fn output(format: crate::options::OutputFormat, keep_going: bool) -> Option<OutputInstance> {
    use interface::OutputType::*;
    interface::stdout(format).map(|v| match v {
        Term(term_output) => terminal::Output::new(term_output, keep_going).into(),
        Dumb(w) => plain::Output::new(w).into(),
    })
}

pub fn error(format: crate::options::OutputFormat) -> Option<Box<term::StderrTerminal>> {
    interface::stderr(format)
}

pub struct OutputInstance {
    inner: Box<dyn Output + Send>,
}

macro_rules! impl_from {
    ( $t:ty ) => {
        impl From<$t> for OutputInstance {
            fn from(v: $t) -> Self {
                OutputInstance { inner: Box::new(v) }
            }
        }
    };
}

impl_from!(terminal::Output);
impl_from!(plain::Output);

impl Output for OutputInstance {
    fn set_log_level(&mut self, log_level: LogLevel) {
        self.inner.set_log_level(log_level)
    }

    fn new_error(&mut self, err: grease::Error) {
        self.inner.new_error(err)
    }

    fn interrupt(&mut self) {
        self.inner.interrupt()
    }
}

impl LogTarget for OutputInstance {
    fn log(&mut self, entry: LogEntry) {
        self.inner.log(entry)
    }

    fn task_running(&mut self, description: RString) -> LogTaskKey {
        self.inner.task_running(description)
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
