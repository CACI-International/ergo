use abi_stable::{
    rvec,
    std_types::{RDuration, ROption, RSlice, RString, RVec},
};
use ergo_runtime::{source::Source, traits, types, ScriptEnv};
use grease::runtime::*;
use simplelog::WriteLogger;
use std::io::Write;
use std::str::FromStr;

mod options;
mod output;
mod script;
mod sync;

/// Constant values shared throughout the program.
mod constants {
    pub const PROGRAM_NAME: &'static str = env!("CARGO_PKG_NAME");
    pub const LOAD_PATH_BINDING: &'static str = "load-path";
    pub const WORKING_DIRECTORY_BINDING: &'static str = "work-dir";
    pub const SCRIPT_EXTENSION: &'static str = PROGRAM_NAME;
    pub const SCRIPT_WORKSPACE_NAME: &'static str = concat!("workspace.", env!("CARGO_PKG_NAME"));
    pub const SCRIPT_PRELUDE_NAME: &'static str = "prelude";

    use directories;
    pub fn app_dirs() -> Option<directories::ProjectDirs> {
        directories::ProjectDirs::from("", "", PROGRAM_NAME)
    }
}

use constants::PROGRAM_NAME;
use options::*;
use output::{output, Output};
use script::{Script, StringSource};

trait AppErr {
    type Output;

    fn app_err_result(self, s: &str) -> Result<Self::Output, String>;

    fn app_err(self, s: &str) -> Self::Output
    where
        Self: Sized,
    {
        match self.app_err_result(s) {
            Err(e) => err_exit(&e),
            Ok(v) => v,
        }
    }
}

fn err_exit(s: &str) -> ! {
    writeln!(std::io::stderr(), "{}", s).unwrap();
    std::io::stderr().flush().unwrap();
    std::process::exit(1);
}

impl<T, E: std::fmt::Display> AppErr for Result<T, E> {
    type Output = T;

    fn app_err_result(self, s: &str) -> Result<Self::Output, String> {
        self.map_err(move |e| format!("{}:\n{}", s, e))
    }
}

impl<T> AppErr for Option<T> {
    type Output = T;

    fn app_err_result(self, s: &str) -> Result<Self::Output, String> {
        self.ok_or(s.to_owned())
    }
}

impl AppErr for bool {
    type Output = ();

    fn app_err_result(self, s: &str) -> Result<Self::Output, String> {
        if !self {
            Err(s.to_owned())
        } else {
            Ok(())
        }
    }
}

fn run(opts: Opts) -> Result<String, grease::value::Error> {
    let mut output =
        output(opts.format, !opts.stop).app_err("could not create output from requested format");
    output.set_log_level(opts.log_level);
    let (logger, orig_logger) = logger_ref(output);

    #[derive(Clone)]
    struct WeakLogTarget(std::sync::Weak<grease::runtime::Logger>);

    impl WeakLogTarget {
        fn with<F: FnOnce(&mut (dyn LogTarget + Send))>(&self, f: F) {
            if let Some(logger) = self.0.upgrade() {
                f(&mut *logger.lock())
            }
        }
    }

    impl LogTarget for WeakLogTarget {
        fn log(&mut self, entry: LogEntry) {
            self.with(move |l| l.log(entry))
        }

        fn dropped(&mut self, context: RVec<RString>) {
            self.with(move |l| l.dropped(context))
        }

        fn timer_pending(&mut self, id: RSlice<RString>) {
            self.with(move |l| l.timer_pending(id))
        }

        fn timer_complete(&mut self, id: RSlice<RString>, duration: ROption<RDuration>) {
            self.with(move |l| l.timer_complete(id, duration))
        }
    }

    let working_dir = std::env::current_dir().expect("could not get current directory");

    // Create build context
    let mut ctx = {
        // Use a weak pointer to the logger in the context, so that when the logger goes out of
        // scope the output is cleaned up reliably. futures::executor::ThreadPool (which ends up
        // holding onto the logger given in on_error) doesn't reliably shutdown as it drops the
        // ThreadPool asynchronously, and likewise for general logging we shouldn't rely on
        // values cleaning up after themselves.
        let weak = std::sync::Arc::downgrade(&orig_logger);
        let logger_weak = logger_ref(WeakLogTarget(std::sync::Arc::downgrade(&logger))).0;
        script::script_context(
            Context::builder()
                .logger_ref(logger_weak.into())
                .storage_directory(working_dir.join(opts.storage))
                .threads(opts.jobs)
                .keep_going(!opts.stop)
                .on_error(move || {
                    if let Some(logger) = weak.upgrade() {
                        logger.lock().new_error();
                    }
                }),
        )
        .expect("failed to create script context")
    };

    // Set thread ids in the logger, as reported by the task manager.
    {
        let mut l = orig_logger.lock();
        l.set_thread_ids(ctx.task.thread_ids().iter().cloned().collect());
    }

    // Set interrupt signal handler to abort tasks.
    //
    // Keep _task in scope until the end of the function. After the function exits,
    // the ctrlc handler will not hold onto the context task manager, which is important for
    // cleanup.
    let (_task, task_ref) = sync::Scoped::new_pair(ctx.task.clone());
    {
        ctrlc::set_handler(move || task_ref.with(|t| t.abort()))
            .app_err_result("failed to set signal handler")?;
    }

    let mut to_eval = String::from(PROGRAM_NAME);
    for arg in opts.args {
        to_eval.push(' ');
        to_eval.push_str(&arg);
    }

    let source = Source::new(StringSource::new("<command line>", to_eval));
    let mut loaded = Script::load(source).app_err_result("failed to parse script file")?;

    let work_dir: grease::value::Value = grease::path::PathBuf::from(working_dir).into();

    // Add initial load path and working directory.
    let mut env: ScriptEnv = Default::default();
    env.insert(
        constants::LOAD_PATH_BINDING.into(),
        Ok(Source::builtin(
            types::Array(rvec![work_dir.clone()]).into(),
        ))
        .into(),
    );
    env.insert(
        constants::WORKING_DIRECTORY_BINDING.into(),
        Ok(Source::builtin(work_dir)).into(),
    );
    loaded.top_level_env(env);
    let script_output = loaded
        .plan(&mut ctx)
        .app_err_result("errors(s) while executing script");
    let script_output = script_output.app_err_result("an evaluation error occurred")?;

    let traits = ctx.traits.clone();

    // Drop context, which removes any unneeded values that may be cached.
    //
    // This is *important* as some values (like those returned by exec) behave differently if their
    // peers still exist or not.
    drop(ctx);

    script_output
        .map(|v| {
            futures::executor::block_on(traits::force_value_nested(&traits, v.clone()))?;
            Ok(traits::display(&traits, &v).to_string())
        })
        .unwrap()
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

    // Run and check for error
    match run(opts) {
        Ok(s) => writeln!(std::io::stdout(), "{}", s).expect("writing output failed"),
        Err(e) => err_exit(&e.to_string()),
    }
}
