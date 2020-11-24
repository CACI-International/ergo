//! Terminal outputs.

use super::interface::{render::*, TerminalOutput};
use abi_stable::std_types::{RDuration, ROption, RSlice, RString, RVec};
use grease::runtime::{LogEntry, LogLevel, LogTarget, LogTaskKey};
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
    paused: Option<Vec<u8>>,
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
    count: usize,
    prompt_abort: bool,
}

impl Output {
    pub fn new(out: TerminalOutput, keep_going: bool) -> Self {
        Output {
            log_level: LogLevel::Info,
            out,
            tasks: TaskStatus::new(),
            progress: Default::default(),
            errors: Errors::new(keep_going),
            paused: None,
        }
    }

    fn update(&mut self) {
        if self.paused.is_some() {
            return;
        }

        let mut renderer = self.out.renderer();

        renderer += &self.tasks;
        renderer += &self.progress;
        renderer += &self.errors;
    }

    fn update_log(&mut self, entry: LogEntry) {
        if self.paused.is_some() {
            return;
        }

        let mut renderer = self.out.renderer_after(entry);

        renderer += &self.tasks;
        renderer += &self.progress;
        renderer += &self.errors;
    }
}

impl super::Output for Output {
    fn set_log_level(&mut self, log_level: LogLevel) {
        self.log_level = log_level;
    }

    fn on_error(&mut self, added: bool) {
        self.errors.update(added);
        self.update();
    }
}

impl LogTarget for Output {
    fn log(&mut self, entry: LogEntry) {
        if entry.level >= self.log_level {
            if let Some(v) = &mut self.paused {
                writeln!(v, "{}", entry).expect("failed to write to output");
            } else {
                self.update_log(entry);
            }
        }
    }

    fn task(&mut self, description: RString) -> LogTaskKey {
        let key = self.tasks.insert(description);
        self.update();
        key
    }

    fn timer_pending(&mut self, id: RSlice<RString>) {
        self.progress.pending(id);
        self.update();
    }

    fn timer_complete(&mut self, id: RSlice<RString>, duration: ROption<RDuration>) {
        self.progress
            .complete(id, duration.map(|v| v.into()).into());
        self.update();
    }

    fn pause_logging(&mut self) {
        self.paused = Some(Default::default());
        self.update();
    }

    fn resume_logging(&mut self) {
        if let Some(bytes) = self.paused.take() {
            self.out
                .write_all(&bytes)
                .expect("failed to write to output");
            self.out.flush().expect("failed to flush output");
        }
        self.update();
    }
}

impl TaskStatus {
    pub fn new() -> Self {
        TaskStatus {
            tasks: Arc::new(Mutex::new(Slab::with_capacity(16))),
        }
    }

    pub fn insert(&mut self, entry: RString) -> LogTaskKey {
        let id = self.tasks.lock().unwrap().insert(entry.into());
        struct Key(TaskStatus, usize);

        impl Drop for Key {
            fn drop(&mut self) {
                if let Ok(mut guard) = self.0.tasks.lock() {
                    guard.remove(self.1);
                }
            }
        }

        LogTaskKey::new(Key(self.clone(), id))
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
        write!(to, "Progress: {} remaining", count_remaining)?;
        if duration_remaining != std::time::Duration::default() {
            write!(to, " ({})", t)?;
        }
        writeln!(to)
    }
}

impl Errors {
    pub fn new(prompt_abort: bool) -> Self {
        Errors {
            count: 0,
            prompt_abort,
        }
    }

    pub fn update(&mut self, added: bool) {
        if added {
            self.count += 1;
        } else {
            self.count -= 1;
        }
    }
}

impl Render for Errors {
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()> {
        if self.count == 0 {
            Ok(())
        } else {
            to.fg(term::color::RED)?;
            writeln!(
                to,
                "{} error{} occurred. {}",
                self.count,
                if self.count == 1 { "" } else { "s" },
                if self.prompt_abort {
                    "Press Ctrl-C to stop."
                } else {
                    ""
                }
            )
        }
    }
}
