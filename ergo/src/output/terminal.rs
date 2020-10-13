//! Terminal outputs.

use super::interface::{render::*, TerminalOutput};
use abi_stable::std_types::{RDuration, ROption, RSlice, RString, RVec};
use grease::runtime::{LogEntry, LogLevel, LogTarget};
use log::warn;
use std::collections::HashMap;
use std::convert::TryInto;

const THREAD_STATUS_LOG_LEVEL: LogLevel = LogLevel::Info;

pub struct Output {
    log_level: LogLevel,
    out: TerminalOutput,
    threads: Option<ThreadStatus>,
    progress: Progress,
    errors: Errors,
    paused: Option<Vec<u8>>,
}

struct ThreadStatus {
    thread_mapping: HashMap<u64, Option<LogEntry>>,
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
            threads: None,
            progress: Default::default(),
            errors: Errors::new(keep_going),
            paused: None,
        }
    }

    fn update(&mut self) {
        let mut renderer = self.out.renderer();
        if self.paused.is_some() {
            return;
        }

        if let Some(ref t) = self.threads {
            renderer += t;
        }
        renderer += &self.progress;
        renderer += &self.errors;
    }
}

impl super::Output for Output {
    fn set_thread_ids(&mut self, ids: Vec<u64>) {
        self.threads = Some(ids.into_iter().collect());
    }

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
                writeln!(v, "{}", entry)
            } else {
                writeln!(self.out, "{}", entry)
            }
            .expect("failed to write to output");
        }

        if let Some(ref mut t) = self.threads {
            if entry.level >= THREAD_STATUS_LOG_LEVEL {
                t.set(Some(entry));
            }
        }

        self.update();
    }

    fn dropped(&mut self, _context: RVec<RString>) {
        if let Some(ref mut t) = self.threads {
            t.set(None);
        }

        self.update();
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

impl ThreadStatus {
    pub fn new<I: IntoIterator<Item = u64>>(i: I) -> Self {
        ThreadStatus {
            thread_mapping: i.into_iter().map(|id| (id, None)).collect(),
        }
    }

    pub fn set(&mut self, entry: Option<LogEntry>) {
        if let Some(id) = grease::runtime::thread_id() {
            if let Some(v) = self.thread_mapping.get_mut(&id) {
                *v = entry;
            }
        }
    }
}

impl std::iter::FromIterator<u64> for ThreadStatus {
    fn from_iter<T: IntoIterator<Item = u64>>(iter: T) -> Self {
        ThreadStatus::new(iter)
    }
}

impl Render for ThreadStatus {
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()> {
        to.fg(term::color::YELLOW)?;
        // Assumes thread_mapping remains the same for order consistency
        for v in self.thread_mapping.values() {
            if let Some(entry) = v {
                writeln!(to, "* {}", entry.args)?;
            }
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
