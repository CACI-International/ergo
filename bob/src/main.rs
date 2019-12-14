use grease::*;
use simplelog::TermLogger;
use std::fs::File;
use std::io::{Read, Write};

mod output;
mod script;

use output::Output;
use script::Script;

trait AppErr {
    type Output;

    fn app_err(self, s: &str) -> Self::Output;
}

fn err_exit(s: &str) -> ! {
    writeln!(std::io::stderr(), "{}", s).unwrap();
    std::process::exit(1);
}

impl<T, E: std::fmt::Display> AppErr for Result<T, E> {
    type Output = T;

    fn app_err(self, s: &str) -> Self::Output {
        match self {
            Ok(v) => v,
            Err(e) => err_exit(&format!("{}: {}", s, e)),
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
        simplelog::LevelFilter::Warn,
        simplelog::Config::default(),
        simplelog::TerminalMode::Stderr,
    )
    .unwrap();

    let logger = logger_ref(Output::default());

    // Create build context
    let mut ctx = script::script_context(
        Context::builder()
            .logger_ref(logger.clone())
            .storage_directory(".bob_work".into()),
    )
    .expect("failed to create script context");

    // Set therad ids in the logger, as reported by the task manager.
    {
        let mut l = logger.lock().unwrap();
        l.set_thread_ids(ctx.task.thread_ids().iter().cloned());
    }

    let script = args.get(1).app_err("first argument must be a script file");
    let mut f = File::open(script).app_err("failed to open script file");
    // TODO inefficient read
    let mut s = String::new();
    f.read_to_string(&mut s)
        .app_err("failed to read script file");
    let loaded = Script::load(s.chars()).app_err("failed to parse script file");

    let script_output = loaded.plan(&mut ctx).app_err("script runtime error");

    let params = args
        .get(2..)
        .map(|strs| strs.iter().map(|s| s.as_ref()).collect())
        .and_then(|ps: Vec<&str>| if ps.is_empty() { None } else { Some(ps) });

    let result = match (params, script_output) {
        (Some(params), script::Data::Map(mut m)) => {
            // Get all data values based on parameters
            let vals = params
                .into_iter()
                .map(|p| m.remove(p).ok_or(p))
                .collect::<Result<Vec<_>, _>>()
                .app_err("target not found");

            // Force outputs from parameters
            futures::executor::block_on(futures::future::join_all(vals))
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .map(|_| ())
        }
        (params, v) => {
            params.is_none().app_err(
                "cannot specify additional parameters unless the script evaluates to a map",
            );
            futures::executor::block_on(v)
        }
    };

    let mut l = logger.lock().unwrap();
    l.clear_status();

    if let Err(e) = result {
        err_exit(&e);
    }
}
