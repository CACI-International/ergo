use grease::*;
use simplelog::TermLogger;
use std::io::Write;

mod output;
mod script;

/// Constant values shared throughout the program.
mod constants {
    pub const PROGRAM_NAME: &'static str = env!("CARGO_PKG_NAME");
}

use constants::PROGRAM_NAME;
use output::Output;
use script::{script_deep_eval, Script, Source, StringSource};

trait AppErr {
    type Output;

    fn app_err(self, s: &str) -> Self::Output;
}

fn err_exit(s: &str) -> ! {
    writeln!(std::io::stderr(), "{}", s).unwrap();
    std::io::stderr().flush().unwrap();
    std::process::exit(1);
}

impl<T, E: std::fmt::Display> AppErr for Result<T, E> {
    type Output = T;

    fn app_err(self, s: &str) -> Self::Output {
        match self {
            Ok(v) => v,
            Err(e) => err_exit(&format!("{}:\n{}", s, e)),
        }
    }
}

impl<T> AppErr for Option<T> {
    type Output = T;

    fn app_err(self, s: &str) -> Self::Output {
        match self {
            Some(v) => v,
            None => err_exit(s),
        }
    }
}

impl AppErr for bool {
    type Output = ();

    fn app_err(self, s: &str) -> Self::Output {
        if !self {
            err_exit(s);
        }
    }
}

fn main() {
    TermLogger::init(
        if cfg!(debug_assertions) {
            simplelog::LevelFilter::Trace
        } else {
            simplelog::LevelFilter::Warn
        },
        simplelog::Config::default(),
        simplelog::TerminalMode::Stderr,
    )
    .unwrap();

    let mut output = Output::default();
    if cfg!(debug_assertions) {
        output.set_log_level(LogLevel::Debug);
    }
    let logger = logger_ref(output);

    // Create build context
    let mut ctx = script::script_context(
        Context::builder()
            .logger_ref(logger.clone())
            .storage_directory(format!(".{}_work", PROGRAM_NAME).into()),
    )
    .expect("failed to create script context");

    // Set therad ids in the logger, as reported by the task manager.
    {
        let mut l = logger.lock().unwrap();
        l.set_thread_ids(ctx.task.thread_ids().iter().cloned());
    }

    let mut args = std::env::args().skip(1);

    let mut to_eval = String::from(PROGRAM_NAME);
    while let Some(arg) = args.next() {
        to_eval.push(' ');
        to_eval.push_str(&arg);
    }

    let source = Source::new(StringSource::new("<command line>", to_eval));
    //let source = Source::new(FileSource(script.into()));
    let loaded = Script::load(source).app_err("failed to parse script file");

    let script_output = loaded.plan(&mut ctx).app_err("script runtime error");

    let to_eval = script_deep_eval(script_output);
    let result = futures::executor::block_on(to_eval).transpose_err();

    let mut l = logger.lock().unwrap();
    l.clear_status();

    if let Err(e) = result {
        err_exit(&e);
    }
}
