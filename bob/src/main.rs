use grease::{
    plan::{Plan, TaskId, Value},
    runtime::{logger_ref, Context, LogEntry, LogLevel, LogTarget, Runtime},
};
use std::collections::HashMap;
use std::io::Write;
use term::Terminal;
use uuid::Uuid;

mod output;

struct Output {
    log_level: LogLevel,
    thread_mapping: HashMap<std::thread::ThreadId, Option<LogEntry>>,
    out: output::OutputInterface,
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

        //TODO calculate progress
        writeln!(self.out, "Progress: ...").unwrap();
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

    fn dropped(&mut self, _task: TaskId, _context: std::sync::Arc<[String]>) {
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
}

fn sleepy(ctx: &mut Context, _inp: Vec<Value>) -> Result<Vec<Value>, String> {
    let l = ctx.log.sublog("sleepytime".to_owned());
    let later = ctx.task.delayed_fn(move || {
        l.info(format_args!("Going to sleep"));
        std::thread::sleep(std::time::Duration::from_secs(2));
        l.info(format_args!("Wake up!"));
        Ok(vec![])
    });
    Ok(vec![Value::new(vec![], later)])
}

fn join(_ctx: &mut Context, inp: Vec<Value>) -> Result<Vec<Value>, String> {
    Ok(inp)
}

fn main() {
    let logger = logger_ref(Output::default());
    let mut rt = Runtime::new(Context::new(logger.clone()).expect("failed to create context"));

    let sleep_uuid = Uuid::from_bytes([1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]);
    let join_uuid = Uuid::from_bytes([1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]);
    rt.add_procedure(sleep_uuid.clone(), sleepy);
    rt.add_procedure(join_uuid.clone(), join);

    let mut builder = Plan::builder();
    let sleepers: Vec<_> = (0..20)
        .map(|_| builder.add_task(sleep_uuid.clone()))
        .collect();
    let joiner = builder.add_task(join_uuid.clone());
    for (i, id) in sleepers.iter().enumerate() {
        builder.link(*id, 0, joiner, i);
    }
    let mut plan = builder
        .build(joiner, &mut rt)
        .expect("failed to build plan");

    {
        let mut l = logger.lock().unwrap();
        for id in rt.context.task.thread_ids() {
            l.thread_mapping.insert(id.clone(), None);
        }
    }

    plan.run();

    let mut l = logger.lock().unwrap();
    l.clear_status();
}
