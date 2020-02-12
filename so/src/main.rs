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
use script::types::*;
use script::{script_deep_eval, FileSource, Script, Source};

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
    let args: Vec<_> = std::env::args().collect();

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

    let script = args.get(1).app_err("first argument must be a script file");
    let source = Source::new(FileSource(script.into()));
    let loaded = Script::load(source).app_err("failed to parse script file");

    let script_output = loaded.plan(&mut ctx).app_err("script runtime error");

    let params = args
        .get(2..)
        .map(|strs| strs.iter().map(|s| s.as_ref()).collect())
        .and_then(|ps: Vec<&str>| if ps.is_empty() { None } else { Some(ps) });

    let value_roots = match (
        script_output.map(|v| v.typed::<ScriptMap>()).transpose(),
        params,
    ) {
        (Ok(val), Some(mut params)) => {
            let ScriptMap(mut m) = val
                .map(|v| v.get())
                .transpose_err()
                .app_err("failed to get value")
                .owned();

            params.sort_unstable();
            params.dedup();
            // Get all data values based on parameters
            params
                .into_iter()
                .map(|p| m.remove(p).ok_or(p))
                .collect::<Result<Vec<_>, _>>()
                .app_err("target not found")
        }
        (Ok(val), None) => vec![val.map(Into::into)],
        (Err(v), params) => {
            params.is_none().app_err(
                "cannot specify additional parameters unless the script evaluates to a map",
            );
            vec![v]
        }
    };

    let to_eval = script_deep_eval(Source::builtin(ScriptArray(value_roots).into()));
    let result = futures::executor::block_on(to_eval).transpose_err();

    let mut l = logger.lock().unwrap();
    l.clear_status();

    if let Err(e) = result {
        err_exit(&e);
    }
}
