use grease::*;
use simplelog::TermLogger;
use std::io::Write;

mod output;
mod script;

/// Constant values shared throughout the program.
mod constants {
    pub const PROGRAM_NAME: &'static str = env!("CARGO_PKG_NAME");
    pub const PROJECT_ROOT_BINDING: &'static str = "project-root";
    pub const LOAD_PATH_BINDING: &'static str = "load-path";

    use directories;
    pub fn app_dirs() -> Option<directories::ProjectDirs> {
        directories::ProjectDirs::from("", "", PROGRAM_NAME)
    }
}

use constants::PROGRAM_NAME;
use output::Output;
use script::{script_deep_eval, Error, Eval, Script, Source, SourceContext, StringSource};

trait AppErr {
    type Output;

    fn app_err(self, s: &str) -> Self::Output;
}

fn err_exit(s: &str) -> ! {
    writeln!(std::io::stderr(), "{}", s).unwrap();
    std::io::stderr().flush().unwrap();
    std::process::exit(1);
}

struct Errors(Vec<SourceContext<Error>>);

impl std::fmt::Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for e in &self.0 {
            writeln!(f, "{}", e)?
        }
        Ok(())
    }
}

impl AppErr for Errors {
    type Output = ();

    fn app_err(self, s: &str) -> Self::Output {
        if !self.0.is_empty() {
            err_exit(&format!("{}:\n{}", s, self))
        }
    }
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

impl<T> AppErr for Eval<T> {
    type Output = T;

    fn app_err(self, s: &str) -> Self::Output {
        match self {
            Eval::Value(v) => v,
            Eval::Error => err_exit(s),
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

    // Set thread ids in the logger, as reported by the task manager.
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
    let loaded = Script::load(source).app_err("failed to parse script file");

    let script_output = loaded.plan(&mut ctx);
    Errors(ctx.get_errors()).app_err("error(s) while executing script");
    let script_output = script_output.app_err("an evaluation error occurred");

    let to_eval = script_deep_eval(script_output);
    let result = futures::executor::block_on(to_eval).transpose_err();

    let mut l = logger.lock().unwrap();
    l.clear_status();

    if let Err(e) = result {
        err_exit(&e);
    }
}
