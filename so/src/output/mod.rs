use abi_stable::std_types::{RDuration, ROption, RSlice, RString, RVec};
use grease::runtime::{LogEntry, LogLevel, LogTarget};

mod interface;
mod plain;
mod terminal;

pub trait Output: LogTarget {
    fn set_thread_ids(&mut self, ids: Vec<u64>);
    fn set_log_level(&mut self, log_level: LogLevel);
    fn new_error(&mut self);
}

pub fn output(format: crate::options::OutputFormat, keep_going: bool) -> Option<OutputInstance> {
    use interface::OutputType::*;
    interface::stdout(format).map(|v| match v {
        Term(term_output) => terminal::Output::new(term_output, keep_going).into(),
        Dumb(w) => plain::Output::new(w).into(),
    })
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
    fn set_thread_ids(&mut self, ids: Vec<u64>) {
        self.inner.set_thread_ids(ids)
    }

    fn set_log_level(&mut self, log_level: LogLevel) {
        self.inner.set_log_level(log_level)
    }

    fn new_error(&mut self) {
        self.inner.new_error()
    }
}

impl LogTarget for OutputInstance {
    fn log(&mut self, entry: LogEntry) {
        self.inner.log(entry)
    }

    fn dropped(&mut self, context: RVec<RString>) {
        self.inner.dropped(context)
    }

    fn timer_pending(&mut self, id: RSlice<RString>) {
        self.inner.timer_pending(id)
    }

    fn timer_complete(&mut self, id: RSlice<RString>, duration: ROption<RDuration>) {
        self.inner.timer_complete(id, duration)
    }
}
