use grease::{LogEntry, LogLevel, LogTarget};

mod interface;
mod plain;
mod terminal;

pub trait Output: LogTarget {
    fn set_thread_ids(&mut self, ids: Vec<std::thread::ThreadId>);
    fn set_log_level(&mut self, log_level: LogLevel);
}

pub fn output() -> OutputInstance {
    use interface::OutputType::*;
    match interface::stdout() {
        Term(term_output) => terminal::Output::new(term_output).into(),
        Dumb(w) => plain::Output::new(w).into(),
    }
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
    fn set_thread_ids(&mut self, ids: Vec<std::thread::ThreadId>) {
        self.inner.set_thread_ids(ids)
    }

    fn set_log_level(&mut self, log_level: LogLevel) {
        self.inner.set_log_level(log_level)
    }
}

impl LogTarget for OutputInstance {
    fn log(&mut self, entry: LogEntry) {
        self.inner.log(entry)
    }

    fn dropped(&mut self, context: std::sync::Arc<[String]>) {
        self.inner.dropped(context)
    }

    fn timer_pending(&mut self, id: &[String]) {
        self.inner.timer_pending(id)
    }

    fn timer_complete(&mut self, id: &[String], duration: Option<std::time::Duration>) {
        self.inner.timer_complete(id, duration)
    }
}
