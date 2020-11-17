use abi_stable::std_types::{RDuration, ROption, RSlice, RString, RVec};
use ergo_runtime::{source::Source, ContextExt};
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
    pub const SCRIPT_DIR_BINDING: &'static str = "script-dir";
    pub const SCRIPT_EXTENSION: &'static str = PROGRAM_NAME;
    pub const SCRIPT_WORKSPACE_NAME: &'static str = concat!("workspace.", env!("CARGO_PKG_NAME"));
    pub const SCRIPT_DIR_NAME: &'static str = concat!("dir.", env!("CARGO_PKG_NAME"));
    pub const SCRIPT_PRELUDE_NAME: &'static str = "prelude";
    pub const SCRIPT_WORKSPACE_FALLBACK_NAME: &'static str = "command";
    pub const PLUGIN_ENTRY: &'static str = concat!("_", env!("CARGO_PKG_NAME"), "_plugin");

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

fn run(opts: Opts) -> Result<String, String> {
    let mut output =
        output(opts.format, !opts.stop).app_err("could not create output from requested format");
    output.set_log_level(opts.log_level);
    let logger = std::sync::Arc::new(std::sync::Mutex::new(output));

    #[derive(Clone)]
    struct WeakLogTarget<T>(std::sync::Weak<std::sync::Mutex<T>>);

    impl<T: LogTarget + Send> WeakLogTarget<T> {
        fn with<F: FnOnce(&mut (dyn LogTarget + Send))>(&self, f: F) {
            if let Some(logger) = self.0.upgrade() {
                if let Ok(mut logger) = logger.lock() {
                    f(&mut *logger)
                }
            }
        }
    }

    impl<T: LogTarget + Send> LogTarget for WeakLogTarget<T> {
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

        fn pause_logging(&mut self) {
            self.with(move |l| l.pause_logging())
        }

        fn resume_logging(&mut self) {
            self.with(move |l| l.resume_logging())
        }
    }

    let working_dir = std::env::current_dir().expect("could not get current directory");

    // Search for furthest workspace ancestor, and set as storage directory root
    let storage_dir_root = if let Some(p) = working_dir
        .ancestors()
        .filter(|p| p.join(constants::SCRIPT_WORKSPACE_NAME).exists())
        .last()
    {
        p.to_owned()
    } else {
        working_dir.clone()
    };

    // Create build context
    let mut ctx = {
        // Use a weak pointer to the logger in the context, so that when the logger goes out of
        // scope the output is cleaned up reliably. The runtime (which ends up holding onto the
        // logger given in on_error) doesn't reliably shutdown as it drops the ThreadPool
        // asynchronously, and likewise for general logging we shouldn't rely on values cleaning up
        // after themselves.
        let weak = std::sync::Arc::downgrade(&logger);
        script::script_context(
            Context::builder()
                .logger(WeakLogTarget(weak.clone()))
                .storage_directory(storage_dir_root.join(opts.storage))
                .threads(opts.jobs)
                .keep_going(!opts.stop)
                .on_error(move |added| {
                    if let Some(logger) = weak.upgrade() {
                        if let Ok(mut logger) = logger.lock() {
                            logger.on_error(added);
                        }
                    }
                }),
        )
        .expect("failed to create script context")
    };

    ctx.lint = opts.lint;

    // Set thread ids in the logger, as reported by the task manager.
    {
        let mut l = logger.lock().unwrap();
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
    let loaded = Script::load(source).app_err_result("failed to parse script file")?;

    let task = ctx.task.clone();
    let script_output = task
        .block_on(loaded.evaluate(&ctx))
        .app_err_result("errors(s) while executing script")?;

    // Clear context, which removes any unneeded values that may be held.
    //
    // This is *important* as some values (like those returned by exec) behave differently if their
    // peers still exist or not.
    ctx.clear_env();

    // We *must* keep the context around because it holds onto plugins, which may have functions referenced in values.
    // Likewise, our return from this function is "flattened" to strings to ensure any references to plugins are used when the plugins are still loaded.

    if opts.lint {
        return Ok(Default::default());
    }

    script_output
        .map(|v| {
            ctx.task.block_on(async {
                ctx.force_value_nested(v.clone()).await?;
                ctx.display(v).await
            })
        })
        .unwrap()
        .map_err(|e: grease::Error| e.to_string())
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
        Err(e) => err_exit(&e),
    }
}
