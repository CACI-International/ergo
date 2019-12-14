use grease::{LogEntry, LogLevel, LogTarget};
use log::warn;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::Write;
use term::Terminal;

mod interface;
use interface as iface;

pub struct Output {
    log_level: LogLevel,
    thread_mapping: HashMap<std::thread::ThreadId, Option<LogEntry>>,
    out: iface::OutputInterface,
    timings: HashMap<Vec<String>, (std::time::Duration, usize)>,
    pending: HashMap<Vec<String>, usize>,
}

impl Output {
    fn send(&mut self, entry: &LogEntry) {
        // Output should already be at the stream location
        writeln!(self.out, "{}", entry).expect("failed to write to output");
    }

    fn print_thread_status(&mut self) {
        self.out.fg(term::color::YELLOW).unwrap();
        // Assumes thread_mapping remains the same for order consistency
        for (i, v) in self.thread_mapping.values().enumerate() {
            self.out.delete_line().unwrap();
            writeln!(
                self.out,
                "{}: {}",
                i + 1,
                match v {
                    None => "",
                    Some(entry) => entry.args.as_ref(),
                }
            )
            .unwrap();
        }

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
        write!(self.out, "Progress: {} remaining", count_remaining).unwrap();
        if duration_remaining != std::time::Duration::default() {
            write!(self.out, " ({})", t).unwrap();
        }
        writeln!(self.out).unwrap();
        self.out.cursor_up().unwrap();

        // Reset cursor
        for _ in 0..self.thread_mapping.len() {
            self.out.cursor_up().unwrap();
        }
        self.out.carriage_return().unwrap();
        self.out.reset().unwrap();
    }

    pub fn set_thread_ids<I: IntoIterator<Item = std::thread::ThreadId>>(&mut self, i: I)
    {
        for id in i {
            self.thread_mapping.insert(id, None);
        }
    }

    pub fn clear_status(&mut self) {
        if self.out.is_tty() {
            let final_logs: Vec<_> = self.thread_mapping.values_mut().map(|v| v.take()).collect();
            let mut lines_to_clear = 0;
            for l in final_logs {
                if let Some(v) = l {
                    self.out.delete_line().unwrap();
                    self.send(&v);
                } else {
                    lines_to_clear += 1;
                }
            }

            for _ in 0..lines_to_clear + 1 {
                self.out.delete_line().unwrap();
                writeln!(self.out).unwrap();
            }

            for _ in 0..lines_to_clear + 1 {
                self.out.cursor_up().unwrap();
            }

            self.out.carriage_return().unwrap();
            self.out.flush().unwrap();
        }
    }
}

impl Default for Output {
    fn default() -> Self {
        Output {
            log_level: LogLevel::Info,
            thread_mapping: HashMap::new(),
            out: iface::stdout(),
            timings: Default::default(),
            pending: Default::default(),
        }
    }
}

impl LogTarget for Output {
    fn log(&mut self, entry: LogEntry) {
        if entry.level < self.log_level {
            return;
        }

        if !self.out.is_tty() {
            self.send(&entry);
        } else {
            let id = std::thread::current().id();
            if let Some(v) = self.thread_mapping.get_mut(&id) {
                if let Some(previous) = v.replace(entry) {
                    self.out.delete_line().unwrap();
                    self.send(&previous);
                }
            }

            self.print_thread_status();
        }
    }

    fn dropped(&mut self, _context: std::sync::Arc<[String]>) {
        if !self.out.is_tty() {
            return;
        }

        let id = std::thread::current().id();
        if let Some(v) = self.thread_mapping.get_mut(&id) {
            if let Some(previous) = v.take() {
                self.out.delete_line().unwrap();
                self.send(&previous);
            }

            self.print_thread_status();
        }
    }

    fn timer_pending(&mut self, id: &[String]) {
        *self.pending.entry(Vec::from(id)).or_default() += 1;
        self.print_thread_status();
    }

    fn timer_complete(&mut self, id: &[String], duration: std::time::Duration) {
        if let Some(v) = self.pending.get_mut(id) {
            if *v > 0 {
                *v -= 1;
            } else {
                warn!("timer count inconsistent: {:?}", id);
            }
        } else {
            warn!("timer count inconsistent: {:?}", id);
        }
        let mut times = self.timings.entry(Vec::from(id)).or_default();
        times.0 += duration;
        times.1 += 1;
        self.print_thread_status();
    }
}
