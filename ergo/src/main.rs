use ergo_runtime::abi_stable::std_types::{RDuration, ROption, RSlice, RString};
use ergo_runtime::{
    context::{LogEntry, LogTarget, LogTaskKey},
    source::StringSource,
    try_value, Error, Source,
};
use ergo_script::constants::{PROGRAM_NAME, WORKSPACE_NAME};
use ergo_script::Runtime;
use simplelog::WriteLogger;
use std::io::Write;
use std::str::FromStr;

mod options;
mod output;
mod sync;

/// Constant values shared throughout the program.
mod constants {
    use directories;
    pub fn app_dirs() -> Option<directories::ProjectDirs> {
        directories::ProjectDirs::from("", "", super::PROGRAM_NAME)
    }
}

use options::*;
use output::{error as error_output, output, Output};

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

fn string_quote<S: AsRef<str>>(s: S) -> String {
    s.as_ref()
        .split('\'')
        .map(|s| {
            if s.is_empty() {
                Default::default()
            } else {
                format!("'{}'", s)
            }
        })
        .collect::<Vec<_>>()
        .join("\"'\"")
}

fn run(opts: Opts) -> Result<String, String> {
    let mut output = output(opts.format, !opts.stop)
        .app_err_result("could not create output from requested format")?;
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
        .filter(|p| p.join(WORKSPACE_NAME).exists())
        .last()
    {
        p.to_owned()
    } else {
        working_dir.clone()
    };

    let storage_directory = storage_dir_root.join(opts.storage);

    // Clean storage directory if requested.
    if opts.clean && storage_directory.exists() {
        std::fs::remove_dir_all(&storage_directory)
            .app_err_result("failed to clean storage directory")?;
    }

    // Get the load path from exe location and user directories.
    let load_path = {
        let mut load_paths = Vec::new();

        // Add neighboring share directories when running in a [prefix]/bin directory.
        let mut neighbor_dir = std::env::current_exe().ok().and_then(|path| {
            path.parent().and_then(|parent| {
                if parent.file_name() == Some("bin".as_ref()) {
                    let path = parent
                        .parent()
                        .expect("must have parent directory")
                        .join("share")
                        .join(PROGRAM_NAME)
                        .join("lib");
                    if path.exists() {
                        Some(path)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
        });

        // If the neighbor directory is somewhere in the home directory, it should be added prior to the local
        // data app dir.
        if let (Some(dir), Some(user_dirs)) = (&neighbor_dir, &directories::UserDirs::new()) {
            if dir.starts_with(user_dirs.home_dir()) {
                load_paths.push(neighbor_dir.take().unwrap());
            }
        }

        // Add local data app dir.
        if let Some(proj_dirs) = constants::app_dirs() {
            let path = proj_dirs.data_local_dir().join("lib");
            if path.exists() {
                load_paths.push(path);
            }
        }

        // If the neighbor directory wasn't added, it should be added now, after the local data app dir.
        if let Some(dir) = neighbor_dir {
            load_paths.push(dir);
        }

        load_paths
    };

    // Use a weak pointer to the logger in the context, so that when the logger goes out of
    // scope the output is cleaned up reliably. The runtime doesn't reliably shutdown as it
    // drops the ThreadPool asynchronously, and likewise for general logging we shouldn't rely
    // on values cleaning up after themselves.
    let weak_logger = std::sync::Arc::downgrade(&logger);

    let error_logger = weak_logger.clone();

    // Create script runtime.
    let runtime = Runtime::new(
        ergo_runtime::Context::builder()
            .logger(WeakLogTarget(weak_logger))
            .storage_directory(storage_directory)
            .threads(opts.jobs)
            .keep_going(!opts.stop)
            .error_handler(move |e: Error| {
                if let Some(logger) = error_logger.upgrade() {
                    if let Ok(mut logger) = logger.lock() {
                        logger.new_error(e);
                    }
                }
            })
            .detect_deadlock(opts.detect_deadlock),
        load_path,
    )
    .expect("failed to create script context");

    if opts.lint {
        runtime.enable_lint();
    }

    // Set interrupt signal handler to abort tasks.
    //
    // Keep signal_handler_task in scope until the handler is no longer needed.
    let (signal_handler_task, task_ref) = sync::Scoped::new_pair(runtime.ctx.task.clone());
    {
        ctrlc::set_handler(move || task_ref.with(|t| t.abort()))
            .app_err_result("failed to set signal handler")?;
    }

    // Build script string to evaluate
    let mut to_eval = if opts.expression {
        if opts.args.is_empty() {
            "()".into()
        } else {
            let mut eval_args = String::new();
            for arg in opts.args {
                if !eval_args.is_empty() {
                    eval_args.push(' ');
                }
                eval_args.push_str(&arg);
            }
            eval_args
        }
    } else {
        let mut args = opts.args.into_iter();
        match args.next() {
            None => {
                // When there are no arguments, always call workspace:command function.
                // This returns _just_ the function, so that documentation will work as expected, but it
                // will then be called without arguments (with Runtime::apply_unbound).
                "workspace:command".into()
            }
            Some(a) => {
                let mut s = String::new();
                // Split the first argument on `:` to support indexing.
                let mut indices = a.split(":");
                let to_load = indices.next().unwrap();
                // Inspect the first effective argument to determine whether to invoke a normal load
                // or the workspace:command function.
                s += if runtime
                    .resolve_script_path(None, to_load.as_ref())
                    .is_none()
                {
                    "workspace:command"
                } else {
                    PROGRAM_NAME
                };
                s += " ";
                s += &string_quote(to_load);

                // Apply indices
                let mut first = true;
                for index in indices {
                    if first {
                        s += " |>";
                        first = false;
                    }
                    s += ":";
                    s += &string_quote(index);
                }

                // Pass all remaining arguments as strings.
                for a in args {
                    s.push(' ');
                    s += &string_quote(a);
                }
                s
            }
        }
    };

    if opts.doc {
        to_eval = format!("doc ({})", to_eval);
    } else if let Some(path) = opts.doc_path {
        to_eval = format!(
            "doc:write {} ({})",
            string_quote(path.display().to_string()),
            to_eval
        );
    }

    let source = Source::new(StringSource::new("<command line>", to_eval));
    let loaded = runtime.evaluate(source);

    let value_to_execute = loaded.and_then(|script_output| {
        let v = runtime.block_on(Runtime::apply_unbound(script_output));
        Ok(try_value!(v))
    });

    // Clear load cache, so that lifetimes are optimistically dropped. It's not very likely that
    // stuff will be loaded while executing the final value, but if so it'll just take the hit of
    // reloading the scripts.
    runtime.clear_load_cache();

    let ret = value_to_execute.and_then(|value| {
        runtime.block_on(async {
            use ergo_runtime::traits::{display, eval_nested, Formatter};
            eval_nested(value.clone()).await?;
            let mut s = String::new();
            {
                let mut formatter = Formatter::new(&mut s);
                display(value, &mut formatter).await?;
            }
            Ok(s)
        })
    });

    // Before the context is destroyed (unloading plugins), clear the thread-local storage in case
    // there are values which were allocated in the plugins.
    ergo_runtime::plugin::Context::reset();

    drop(signal_handler_task);
    drop(runtime);

    // Drop the logger prior to the context dropping, so that any stored state (like errors) can
    // free with the plugins still loaded.
    // Only drop when nothing else is using it (so we have reliable terminal cleanup).
    let mut logger = Some(logger);
    loop {
        match std::sync::Arc::try_unwrap(logger.take().unwrap()) {
            Ok(l) => {
                drop(l);
                break;
            }
            Err(l) => {
                logger = Some(l);
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
        }
    }

    // Write error output to stderr.
    // TODO get error(s) from error scope rather than return value?
    match ret {
        Ok(v) => Ok(v),
        Err(e) => {
            use ergo_runtime::error::{self, BoxErgoError, ErrorContext};

            // For each error root, display a certain number of backtrace contexts.
            fn display_errors<'a>(
                e: &'a BoxErgoError,
                out: &mut Box<term::StderrTerminal>,
                contexts: &mut Vec<&'a error::Context>,
                limit: &Option<usize>,
            ) {
                let had_context = if let Some(ctx) = e.downcast_ref::<ErrorContext>() {
                    contexts.push(ctx.context());
                    true
                } else {
                    false
                };
                let inner = e.source();
                if inner.is_empty() {
                    // Root error
                    out.fg(term::color::RED)
                        .expect("failed to set output color");
                    write!(out, "error:").expect("failed to write to stderr");
                    out.reset().expect("failed to reset output");
                    writeln!(out, " {}", e).expect("failed to write to stderr");
                    let write_context = |ctx: &&error::Context| {
                        out.fg(term::color::BRIGHT_WHITE)
                            .expect("failed to set output color");
                        write!(out, "note:").expect("failed to write to stderr");
                        out.reset().expect("failed to reset output");
                        writeln!(out, " {}", ctx).expect("failed to write to stderr");
                    };
                    match limit.as_ref() {
                        None => contexts.iter().rev().for_each(write_context),
                        Some(&limit) => {
                            contexts.iter().rev().take(limit).for_each(write_context);
                            if limit < contexts.len() {
                                writeln!(out, "...omitting {} frame(s)", contexts.len() - limit)
                                    .expect("failed to write to stderr");
                            }
                        }
                    }
                } else {
                    for e in inner {
                        display_errors(e, out, contexts, limit);
                    }
                }
                if had_context {
                    contexts.pop();
                }
            }

            let mut err = error_output(opts.format)
                .app_err("could not create error output from requested format");

            let mut contexts = Vec::new();
            display_errors(&e.error(), &mut err, &mut contexts, &opts.error_limit);
            Err("one or more errors occurred".into())
        }
    }
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
    let app = Opts::clap().settings(&[structopt::clap::AppSettings::TrailingVarArg]);
    let mut opts = Opts::from_clap(&app.get_matches());

    // Additional opts logic.
    if opts.doc {
        opts.page ^= true;
    }

    let paging_enabled = opts.page;

    let result = run(opts);

    if paging_enabled {
        pager::Pager::with_default_pager(if cfg!(target_os = "macos") {
            // macos less is old and buggy, not working correctly with `-F`
            "less"
        } else {
            "less -F"
        })
        .setup();
    }

    match result {
        Ok(s) => writeln!(std::io::stdout(), "{}", s).expect("writing output failed"),
        Err(e) => err_exit(&e),
    }
}
