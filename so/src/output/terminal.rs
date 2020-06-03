//! Terminal outputs.

use super::interface::{render::*, TerminalOutput};
use grease::{LogEntry, LogLevel, LogTarget};
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
}

struct ThreadStatus {
    thread_mapping: HashMap<std::thread::ThreadId, Option<LogEntry>>,
}

#[derive(Default)]
struct Progress {
    timings: HashMap<Vec<String>, (std::time::Duration, usize)>,
    pending: HashMap<Vec<String>, usize>,
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
        }
    }

    fn update(&mut self) {
        let mut renderer = self.out.renderer();
        if let Some(ref t) = self.threads {
            renderer += t;
        }
        renderer += &self.progress;
        renderer += &self.errors;
    }
}

impl super::Output for Output {
    fn set_thread_ids(&mut self, ids: Vec<std::thread::ThreadId>) {
        self.threads = Some(ids.into_iter().collect());
    }

    fn set_log_level(&mut self, log_level: LogLevel) {
        self.log_level = log_level;
    }

    fn new_error(&mut self) {
        self.errors.inc();
        self.update();
    }
}

impl LogTarget for Output {
    fn log(&mut self, entry: LogEntry) {
        if entry.level >= self.log_level {
            writeln!(self.out, "{}", entry).expect("failed to write to output");
        }

        if let Some(ref mut t) = self.threads {
            if entry.level >= THREAD_STATUS_LOG_LEVEL {
                t.set(Some(entry));
            }
        }

        self.update();
    }

    fn dropped(&mut self, _context: std::sync::Arc<[String]>) {
        if let Some(ref mut t) = self.threads {
            t.set(None);
        }

        self.update();
    }

    fn timer_pending(&mut self, id: &[String]) {
        self.progress.pending(id);

        self.update();
    }

    fn timer_complete(&mut self, id: &[String], duration: Option<std::time::Duration>) {
        self.progress.complete(id, duration);

        self.update();
    }
}

impl ThreadStatus {
    pub fn new<I: IntoIterator<Item = std::thread::ThreadId>>(i: I) -> Self {
        ThreadStatus {
            thread_mapping: i.into_iter().map(|id| (id, None)).collect(),
        }
    }

    pub fn set(&mut self, entry: Option<LogEntry>) {
        let id = std::thread::current().id();
        if let Some(v) = self.thread_mapping.get_mut(&id) {
            *v = entry;
        }
    }
}

impl std::iter::FromIterator<std::thread::ThreadId> for ThreadStatus {
    fn from_iter<T: IntoIterator<Item = std::thread::ThreadId>>(iter: T) -> Self {
        ThreadStatus::new(iter)
    }
}

impl Render for ThreadStatus {
    fn render<Target: Write + Terminal>(&self, to: &mut Target) -> std::io::Result<()> {
        to.fg(term::color::YELLOW)?;
        // Assumes thread_mapping remains the same for order consistency
        for (i, v) in self.thread_mapping.values().enumerate() {
            writeln!(
                to,
                "{}: {}",
                i + 1,
                match v {
                    None => "",
                    Some(entry) => entry.args.as_ref(),
                }
            )?;
        }
        Ok(())
    }
}

impl Progress {
    pub fn pending(&mut self, id: &[String]) {
        *self.pending.entry(Vec::from(id)).or_default() += 1;
    }

    pub fn complete(&mut self, id: &[String], duration: Option<std::time::Duration>) {
        if let Some(v) = self.pending.get_mut(id) {
            if *v > 0 {
                *v -= 1;
            } else {
                warn!("timer count inconsistent: {:?}", id);
            }
        } else {
            warn!("timer count inconsistent: {:?}", id);
        }
        if let Some(duration) = duration {
            let mut times = self.timings.entry(Vec::from(id)).or_default();
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

    pub fn inc(&mut self) {
        self.count += 1;
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
