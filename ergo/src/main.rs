use abi_stable::std_types::{RDuration, ROption, RSlice, RString};
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
        fn with<R, F: FnOnce(&mut (dyn LogTarget + Send)) -> R>(&self, f: F) -> Option<R> {
            if let Some(logger) = self.0.upgrade() {
                if let Ok(mut logger) = logger.lock() {
                    return Some(f(&mut *logger));
                }
            }
            None
        }
    }

    impl<T: LogTarget + Send> LogTarget for WeakLogTarget<T> {
        fn log(&mut self, entry: LogEntry) {
            self.with(move |l| l.log(entry));
        }

        fn task_running(&mut self, description: RString) -> LogTaskKey {
            self.with(move |l| l.task_running(description))
                .unwrap_or(LogTaskKey::new(()))
        }

        fn task_suspend(&mut self, key: LogTaskKey) {
            self.with(move |l| l.task_suspend(key));
        }

        fn timer_pending(&mut self, id: RSlice<RString>) {
            self.with(move |l| l.timer_pending(id));
        }

        fn timer_complete(&mut self, id: RSlice<RString>, duration: ROption<RDuration>) {
            self.with(move |l| l.timer_complete(id, duration));
        }

        fn pause_logging(&mut self) {
            self.with(move |l| l.pause_logging());
        }

        fn resume_logging(&mut self) {
            self.with(move |l| l.resume_logging());
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

    let storage_directory = storage_dir_root.join(opts.storage);
    if opts.clean && storage_directory.exists() {
        std::fs::remove_dir_all(&storage_directory)
            .app_err_result("failed to clean storage directory")?;
    }

    // Use a weak pointer to the logger in the context, so that when the logger goes out of
    // scope the output is cleaned up reliably. The runtime doesn't reliably shutdown as it
    // drops the ThreadPool asynchronously, and likewise for general logging we shouldn't rely
    // on values cleaning up after themselves.
    let weak_logger = std::sync::Arc::downgrade(&logger);

    // Create runtime context
    let mut ctx = script::script_context(
        Context::builder()
            .logger(WeakLogTarget(weak_logger.clone()))
            .storage_directory(storage_directory)
            .threads(opts.jobs)
            .keep_going(!opts.stop),
    )
    .expect("failed to create script context");

    ctx.lint = opts.lint;

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

    // Setup error observer to display in logging.
    let on_error = move |e: grease::Error| {
        if let Some(logger) = weak_logger.upgrade() {
            if let Ok(mut logger) = logger.lock() {
                logger.new_error(e);
            }
        }
    };

    let task = ctx.task.clone();
    let script_output = task
        .block_on(grease::value::Errored::observe(
            on_error.clone(),
            loaded.evaluate(&ctx),
        ))
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

    let ret = script_output
        .map(|v| {
            ctx.task
                .block_on(grease::value::Errored::observe(on_error, async {
                    ctx.force_value_nested(v.clone()).await?;
                    ctx.display(v).await
                }))
        })
        .unwrap()
        .map_err(|e: grease::Error| e.to_string());

    // Before the context is destroyed (unloading plugins), clear the thread-local storage in case
    // there are values which were allocated in the plugins.
    plugin_tls::Context::reset();

    // Drop the logger prior to the context dropping, so that any stored state (like errors) can
    // free with the plugins still loaded.
    drop(logger);

    ret
}

fn main() {
    // Install the application logger.
    //
    // We write all logs to the configured (by env variable) file, falling back to the data local
    // dir or temp directory, and read from the {PROGRAM_NAME}_LOG environment variable to set the
    // application log level (which defaults to warn).
    {
        let log_file = {
            if let Some(f) = std::env::var_os(format!("{}_LOG_FILE", PROGRAM_NAME.to_uppercase())) {
                f.into()
            } else {
                let mut f = if let Some(proj_dirs) = constants::app_dirs() {
                    proj_dirs.data_local_dir().to_owned()
                } else {
                    std::env::temp_dir()
                };
                f.push(format!("{}.log", PROGRAM_NAME));
                f
            }
        };
        std::fs::create_dir_all(log_file.parent().expect("log file is a root directory"))
            .expect("failed to create log output directory");
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
