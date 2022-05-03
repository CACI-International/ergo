//! Plain text output.

use ergo_runtime::context::{LogEntry, LogLevel, LogTarget};
use ergo_runtime::Error;
use std::io::Write;

pub struct Output {
    log_level: LogLevel,
    out: Box<dyn Write + Send>,
    errors: ergo_runtime::error::Diagnostics,
}

impl Output {
    pub fn new(out: Box<dyn Write + Send>) -> Self {
        Output {
            log_level: LogLevel::Info,
            out,
            errors: Default::default(),
        }
    }
}

impl super::Output for Output {
    fn set_log_level(&mut self, log_level: LogLevel) {
        self.log_level = log_level;
    }

    fn new_error(&mut self, err: Error) {
        self.errors.insert(&err);
    }

    fn interrupt(&mut self) {}

    fn indicate_progress(&mut self) {}

    fn update(&mut self) {}

    fn take_errors(&mut self) -> ergo_runtime::error::Diagnostics {
        std::mem::take(&mut self.errors)
    }
}

impl LogTarget for Output {
    fn log(&mut self, entry: LogEntry) {
        if entry.level >= self.log_level {
            writeln!(self.out, "{}", entry).expect("failed to write to output");
        }
    }
}
