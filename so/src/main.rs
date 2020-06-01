use grease::*;
use simplelog::WriteLogger;
use std::io::Write;
use std::str::FromStr;

mod options;
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
use options::*;
use output::{output, Output};
use script::{force_value_nested, Error, Eval, Script, Source, SourceContext, StringSource};

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
    // Install the application logger.
    //
    // We write all logs to the temp directory, and read from the {PROGRAM_NAME}_LOG environment
    // variable to set the application log level (which defaults to warn).
    {
        let mut log_file = std::env::temp_dir();
        log_file.push(format!("{}.log", PROGRAM_NAME));
        WriteLogger::init(
            std::env::var(format!("{}_LOG", PROGRAM_NAME.to_uppercase()))
                .map(|v| simplelog::LevelFilter::from_str(&v).expect("invalid program log level"))
                .unwrap_or(simplelog::LevelFilter::Warn),
            simplelog::Config::default(),
            std::fs::File::create(log_file).expect("failed to open log file for writing"),
        )
        .unwrap();
    }

    // Parse arguments
    let opts = Opts::from_args();

    // The following block scopes exclusive use (and cleanup) of stdout/stderr.
    let result = {
        let mut output =
            output(opts.format).app_err("could not create output from requested format");
        output.set_log_level(opts.log_level);
        let logger = logger_ref(output);

        // Create build context
        let mut ctx = script::script_context(
            Context::builder()
                .logger_ref(logger.clone())
                .storage_directory(format!(".{}_work", PROGRAM_NAME).into())
                .threads(opts.jobs),
        )
        .expect("failed to create script context");

        let mut to_eval = String::from(PROGRAM_NAME);
        for arg in opts.args {
            to_eval.push(' ');
            to_eval.push_str(&arg);
        }

        let source = Source::new(StringSource::new("<command line>", to_eval));
        let loaded = Script::load(source).app_err("failed to parse script file");

        let script_output = loaded.plan(&mut ctx);
        Errors(ctx.get_errors()).app_err("error(s) while executing script");
        let script_output = script_output.app_err("an evaluation error occurred");

        // Set thread ids in the logger, as reported by the task manager.
        {
            let mut l = logger.lock().unwrap();
            l.set_thread_ids(ctx.task.thread_ids().iter().cloned().collect());
        }

        script_output
            .map(|v| futures::executor::block_on(force_value_nested(&ctx.traits, v)))
            .transpose_err()
    };

    if let Err(e) = result {
        err_exit(&e.to_string());
    }
}
