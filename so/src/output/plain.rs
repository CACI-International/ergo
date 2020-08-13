//! Plain text output.

use grease::runtime::{LogEntry, LogLevel, LogTarget};
use std::io::Write;

pub struct Output {
    log_level: LogLevel,
    out: Box<dyn Write + Send>,
}

impl Output {
    pub fn new(out: Box<dyn Write + Send>) -> Self {
        Output {
            log_level: LogLevel::Info,
            out,
        }
    }
}

impl super::Output for Output {
    fn set_thread_ids(&mut self, _ids: Vec<u64>) {}

    fn set_log_level(&mut self, log_level: LogLevel) {
        self.log_level = log_level;
    }

    fn new_error(&mut self) {}
}

impl LogTarget for Output {
    fn log(&mut self, entry: LogEntry) {
        if entry.level >= self.log_level {
            writeln!(self.out, "{}", entry).expect("failed to write to output");
        }
    }
}
