use grease::*;
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
    pub const PROJECT_ROOT_BINDING: &'static str = "project-root";
    pub const LOAD_PATH_BINDING: &'static str = "load-path";
    pub const MOD_PATH_BINDING: &'static str = "mod-path";

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

    fn app_err_result(self, s: &str) -> Result<Self::Output, String> {
        if !self.0.is_empty() {
            Err(format!("{}:\n{}", s, self))
        } else {
            Ok(())
        }
    }
}

impl<T, E: std::fmt::Display> AppErr for Result<T, E> {
    type Output = T;

    fn app_err_result(self, s: &str) -> Result<Self::Output, String> {
        self.map_err(move |e| format!("{}:\n{}", s, e))
    }
}

impl<T> AppErr for Eval<T> {
    type Output = T;

    fn app_err_result(self, s: &str) -> Result<Self::Output, String> {
        match self {
            Eval::Value(v) => Ok(v),
            Eval::Error => Err(s.to_owned()),
        }
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

fn run(opts: Opts) -> Result<String, grease::Error> {
    let mut output =
        output(opts.format, !opts.stop).app_err("could not create output from requested format");
    output.set_log_level(opts.log_level);
    let logger = logger_ref(output);

    #[derive(Clone)]
    struct WeakLogTarget(std::sync::Weak<std::sync::Mutex<dyn LogTarget + Send>>);

    impl WeakLogTarget {
        fn with<F: FnOnce(&mut (dyn LogTarget + Send))>(&self, f: F) {
            if let Some(logger) = self.0.upgrade() {
                if let Ok(mut guard) = logger.lock() {
                    f(&mut *guard)
                }
            }
        }
    }

    impl LogTarget for WeakLogTarget {
        fn log(&mut self, entry: LogEntry) {
            self.with(move |l| l.log(entry))
        }

        fn dropped(&mut self, context: std::sync::Arc<[String]>) {
            self.with(move |l| l.dropped(context))
        }

        fn timer_pending(&mut self, id: &[String]) {
            self.with(move |l| l.timer_pending(id))
        }

        fn timer_complete(&mut self, id: &[String], duration: Option<std::time::Duration>) {
            self.with(move |l| l.timer_complete(id, duration))
        }
    }

    // Create build context
    let mut ctx = {
        // Use a weak pointer to the logger in the context, so that when the logger goes out of
        // scope the output is cleaned up reliably. futures::executor::ThreadPool (which ends up
        // holding onto the logger given in on_error) doesn't reliably shutdown as it drops the
        // ThreadPool asynchronously, and likewise for general logging we shouldn't rely on
        // values cleaning up after themselves.
        let weak = std::sync::Arc::downgrade(&logger);
        let logger_weak = logger_ref(WeakLogTarget(weak.clone()));
        script::script_context(
            Context::builder()
                .logger_ref(logger_weak.clone())
                .storage_directory(
                    opts.storage
                        .canonicalize()
                        .app_err_result("failed to canonicalize storage path")?,
                )
                .threads(opts.jobs)
                .keep_going(!opts.stop)
                .on_error(move || {
                    if let Some(logger) = weak.upgrade() {
                        if let Ok(mut l) = logger.lock() {
                            l.new_error();
                        }
                    }
                }),
        )
        .expect("failed to create script context")
    };

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
    let loaded = Script::load(source).app_err_result("failed to parse script file")?;

    let script_output = loaded.plan(&mut ctx);
    Errors(ctx.get_errors()).app_err_result("error(s) while executing script")?;
    let script_output = script_output.app_err_result("an evaluation error occurred")?;

    // Set thread ids in the logger, as reported by the task manager.
    {
        let mut l = logger.lock().unwrap();
        l.set_thread_ids(ctx.task.thread_ids().iter().cloned().collect());
    }

    let traits = ctx.traits.clone();

    // Drop context, which removes any unneeded values that may be cached.
    //
    // This is *important* as some values (like those returned by exec) behave differently if their
    // peers still exist or not.
    drop(ctx);

    script_output
        .map(|v| {
            futures::executor::block_on(force_value_nested(&traits, v.clone()))?;
            Ok(grease::display(&traits, &v).to_string())
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
