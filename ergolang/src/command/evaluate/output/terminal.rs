//! Terminal outputs.

use super::interface::{render::*, TerminalOutput};
use ergo_runtime::abi_stable::std_types::{RDuration, ROption, RSlice, RString, RVec};
use ergo_runtime::context::{LogEntry, LogLevel, LogTarget, LogTaskKey};
use ergo_runtime::{
    error::{Diagnostic, Diagnostics},
    Error,
};
use log::warn;
use slab::Slab;
use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::{Arc, Mutex};

pub struct Output {
    log_level: LogLevel,
    out: TerminalOutput,
    tasks: TaskStatus,
    progress: Progress,
    errors: Errors,
    pending_logs: Vec<LogEntry>,
    paused: bool,
    need_update: bool,
}

#[derive(Clone)]
struct TaskStatus {
    tasks: Arc<Mutex<Slab<String>>>,
}

#[derive(Default)]
struct Progress {
    timings: HashMap<RVec<RString>, (std::time::Duration, usize)>,
    pending: HashMap<RVec<RString>, usize>,
}

struct Errors {
    errors: Diagnostics,
    prompt_abort: bool,
    interrupts: u8,
}

impl Output {
    pub fn new(out: TerminalOutput, keep_going: bool) -> Self {
        Output {
            log_level: LogLevel::Info,
            out,
            tasks: TaskStatus::new(),
            progress: Default::default(),
            errors: Errors::new(keep_going),
            pending_logs: Default::default(),
            paused: false,
            need_update: true,
        }
    }
}

impl super::Output for Output {
    fn set_log_level(&mut self, log_level: LogLevel) {
        self.log_level = log_level;
    }

    fn new_error(&mut self, err: Error) {
        self.errors.update(err, |_| ());
        self.need_update = true;
    }

    fn interrupt(&mut self) {
        self.errors.interrupts += 1;
        self.need_update = true;
    }

    fn update(&mut self) {
        if self.paused || !self.need_update {
            return;
        }

        let mut renderer = self
            .out
            .renderer_after(std::mem::take(&mut self.pending_logs));

        renderer += &self.tasks;
        renderer += &self.progress;
        renderer += &self.errors;

        self.need_update = false;
    }

    fn take_errors(&mut self) -> ergo_runtime::error::Diagnostics {
        std::mem::take(&mut self.errors.errors)
    }
}

impl Drop for Output {
    fn drop(&mut self) {
        use super::Output;
        self.update();
    }
}

impl LogTarget for Output {
    fn log(&mut self, entry: LogEntry) {
        if entry.level >= self.log_level {
            self.pending_logs.push(entry);
            self.need_update = true;
        }
    }

    fn task_running(&mut self, description: RString) -> LogTaskKey {
        let key = self.tasks.insert(description);
        self.need_update = true;
        LogTaskKey::new(key)
    }

    fn task_suspend(&mut self, key: LogTaskKey) {
        if let Ok(key) = key.into::<usize>() {
            self.tasks.remove(key);
            self.need_update = true;
        }
    }

    fn timer_pending(&mut self, id: RSlice<RString>) {
        self.progress.pending(id);
        self.need_update = true;
    }

    fn timer_complete(&mut self, id: RSlice<RString>, duration: ROption<RDuration>) {
        self.progress
            .complete(id, duration.map(|v| v.into()).into());
        self.need_update = true;
    }

    fn pause_logging(&mut self) {
        // Update to flush any pending logs.
        use super::Output;
        self.update();
        self.paused = true;
        self.out.enable_stdin();
        // Clear previous rendered content.
        self.out.renderer();
    }

    fn resume_logging(&mut self) {
        self.paused = false;
        self.out.disable_stdin();
        self.need_update = true;
    }
}

impl TaskStatus {
    pub fn new() -> Self {
        TaskStatus {
            tasks: Arc::new(Mutex::new(Slab::with_capacity(16))),
        }
    }

    pub fn insert(&mut self, entry: RString) -> usize {
        self.tasks.lock().unwrap().insert(entry.into())
    }

    pub fn remove(&mut self, key: usize) {
        self.tasks.lock().unwrap().remove(key);
    }
}

impl Render for TaskStatus {
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()> {
        to.fg(term::color::YELLOW)?;
        let guard = self.tasks.lock().unwrap();
        for v in guard.iter() {
            writeln!(to, "* {}", &v.1)?;
        }
        Ok(())
    }
}

impl Progress {
    pub fn pending(&mut self, id: RSlice<RString>) {
        *self.pending.entry(id.to_rvec()).or_default() += 1;
    }

    pub fn complete(&mut self, id: RSlice<RString>, duration: Option<std::time::Duration>) {
        if let Some(v) = self.pending.get_mut(id.as_slice()) {
            if *v > 0 {
                *v -= 1;
            } else {
                warn!("timer count inconsistent: {:?}", id);
            }
        } else {
            warn!("timer count inconsistent: {:?}", id);
        }
        if let Some(duration) = duration {
            let mut times = self.timings.entry(id.to_rvec()).or_default();
            times.0 += duration;
            times.1 += 1;
        }
    }
}

impl Render for Progress {
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()> {
        let mut count_remaining = 0;
        let mut duration_remaining = std::time::Duration::default();
        for (k, v) in self.pending.iter() {
            count_remaining += v;
            if let Some((duration, count)) = self.timings.get(k) {
                duration_remaining +=
                    *duration * (*v).try_into().unwrap() / (*count).try_into().unwrap();
            }
        }

        let t = chrono::naive::NaiveTime::from_hms(0, 0, 0)
            + chrono::Duration::from_std(duration_remaining).unwrap();
        write!(to, "Progress: {} remaining", count_remaining + 1)?;
        if duration_remaining != std::time::Duration::default() {
            write!(to, " ({})", t)?;
        }
        writeln!(to)
    }
}

impl Errors {
    pub fn new(prompt_abort: bool) -> Self {
        Errors {
            errors: Default::default(),
            prompt_abort,
            interrupts: 0,
        }
    }

    pub fn update<F>(&mut self, err: Error, new: F)
    where
        F: FnMut(&Diagnostic),
    {
        self.errors.insert_new(&err, new);
    }
}

impl Render for Errors {
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()> {
        let count = self.errors.len();
        if count == 0 {
            Ok(())
        } else {
            to.fg(term::color::RED)?;
            writeln!(
                to,
                "{} error{} occurred. {}",
                count,
                if count == 1 { "" } else { "s" },
                if self.prompt_abort {
                    "Press Ctrl-C to stop."
                } else {
                    ""
                }
            )
        }
    }
}
