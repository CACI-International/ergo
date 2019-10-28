use grease::{
    plan::{Plan, Value, ValueType},
    runtime::{
        future, logger_ref, Context, FutureExt, LogEntry, LogLevel, LogTarget, TryFutureExt,
    },
};
use log::warn;
use simplelog::TermLogger;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::Write;
use term::Terminal;
use uuid::Uuid;

mod output;

struct Output {
    log_level: LogLevel,
    thread_mapping: HashMap<std::thread::ThreadId, Option<LogEntry>>,
    out: output::OutputInterface,
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

    fn clear_status(&mut self) {
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
        }
    }
}

impl Default for Output {
    fn default() -> Self {
        Output {
            log_level: LogLevel::Info,
            thread_mapping: HashMap::new(),
            out: output::stdout(),
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
    }
}

fn void_type() -> ValueType {
    ValueType::new(Uuid::from_bytes([
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ]))
}

struct Sleeper {
    time: f32,
}

impl Plan<Context> for Sleeper {
    type Output = Value;
    fn plan(&self, ctx: &mut Context) -> Value {
        let l = ctx.log.sublog("sleepytime".to_owned());
        let tsk = ctx.task.clone();
        let time = self.time;
        let later =
            future::lazy(move |_| (l.work("sleep".to_owned()), l)).then(move |(mut sleep, l)| {
                tsk.delayed_fn(move || {
                    l.info(format_args!("Going to sleep"));
                    {
                        let _record = sleep.start();
                        std::thread::sleep(std::time::Duration::from_secs_f32(time));
                    }
                    l.info(format_args!("Wake up!"));
                    Ok(vec![])
                })
            });
        Value::future(void_type(), later)
    }
}

struct SleepyTasks {
    count: usize,
    min_time: f32,
    max_time: f32,
}

impl Plan<Context> for SleepyTasks {
    type Output = Value;
    fn plan(&self, ctx: &mut Context) -> Value {
        use rand::distributions::{Distribution, Uniform};
        let sleepers: Vec<_> = Uniform::from(self.min_time..self.max_time)
            .sample_iter(rand::thread_rng())
            .take(self.count)
            .map(|v| Sleeper { time: v }.plan(ctx))
            .collect();

        Value::future(
            void_type(),
            future::try_join_all(sleepers).map_ok(|_| vec![]),
        )
    }
}

fn main() {
    TermLogger::init(
        simplelog::LevelFilter::Warn,
        simplelog::Config::default(),
        simplelog::TerminalMode::Stderr,
    )
    .unwrap();

    let logger = logger_ref(Output::default());

    let mut ctx = Context::builder()
        .logger_ref(logger.clone())
        .build()
        .expect("failed to create context");

    {
        let mut l = logger.lock().unwrap();
        for id in ctx.task.thread_ids() {
            l.thread_mapping.insert(id.clone(), None);
        }
    }

    let mut result = SleepyTasks {
        count: 50,
        min_time: 1.0,
        max_time: 4.0,
    }
    .plan(&mut ctx);

    result.get().unwrap();

    let mut l = logger.lock().unwrap();
    l.clear_status();
}
