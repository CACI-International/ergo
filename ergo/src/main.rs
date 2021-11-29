use ergo_runtime::abi_stable::std_types::{RDuration, ROption, RSlice, RString};
use ergo_runtime::{
    context::{LogEntry, LogTarget, LogTaskKey},
    try_value, Error,
};
use ergo_script::constants::{PROGRAM_NAME, WORKSPACE_NAME};
use ergo_script::Runtime;
use simplelog::WriteLogger;
use std::io::Write;
use std::str::FromStr;

mod options;
mod output;
mod sync;
mod terminal_state;

/// Constant values shared throughout the program.
mod constants {
    use directories;
    pub fn app_dirs() -> Option<directories::ProjectDirs> {
        directories::ProjectDirs::from("", "", super::PROGRAM_NAME)
    }
}

use options::*;
use output::{error as error_output, output, Output, OutputInstance, TermToTermcolor};

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
    unsafe { terminal_state::restore() };
    eprintln!("{}", s);
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
    let mut ret = String::new();
    ret.push('"');
    for c in s.as_ref().chars() {
        match c {
            '\n' => ret.push_str("\\n"),
            '\t' => ret.push_str("\\t"),
            '\\' => ret.push_str("\\\\"),
            '"' => ret.push_str("\\\""),
            c => ret.push(c),
        }
    }
    ret.push('"');
    ret
}

fn run(opts: Opts, mut output: OutputInstance) -> Result<String, String> {
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
        let neighbor_data_dir = std::env::current_exe().ok().and_then(|path| {
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

        let has_data_dir = neighbor_data_dir.is_some();

        // Add the data directory prior to any user lib dir.
        if let Some(dir) = neighbor_data_dir {
            load_paths.push(dir);
        }

        // Add local data app dir if necessary, and user lib dir.
        if let Some(proj_dirs) = constants::app_dirs() {
            if !has_data_dir {
                let path = proj_dirs.data_local_dir().join("lib");
                if path.exists() {
                    load_paths.push(path);
                }
            }
            let path = proj_dirs.preference_dir().join("lib");
            if path.exists() {
                load_paths.push(path);
            }
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

    if let Some(level) = opts.lint {
        runtime.lint_level(level.unwrap_or(ergo_script::LintLevel::On));
    }

    runtime.backtrace(opts.backtrace);

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
                // will then be called (with Runtime::apply_unbound).
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

    let loaded = runtime.evaluate_string("<command line>", &to_eval);

    let (complete_send, complete) = std::sync::mpsc::channel();

    let exec_thread = std::thread::spawn(move || {
        let value_to_execute = loaded.and_then(|script_output| {
            let v = runtime.block_on(Runtime::apply_unbound(script_output));
            Ok(try_value!(v))
        });

        // Clear load cache, so that lifetimes are optimistically dropped. It's not very likely that
        // stuff will be loaded while executing the final value, but if so it'll just take the hit of
        // reloading the scripts.
        runtime.clear_load_cache();

        let result = value_to_execute.and_then(|value| {
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

        let sources =
            runtime.block_on(async { ergo_runtime::Context::global().diagnostic_sources() });
        drop(runtime);

        drop(complete_send.send(()));
        (result, sources)
    });

    loop {
        if let Ok(mut g) = logger.lock() {
            g.update();
        }
        match complete.recv_timeout(std::time::Duration::from_millis(50)) {
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => continue,
            _ => break,
        }
    }

    let (result, sources) = exec_thread.join().unwrap();

    // Drop the logger prior to the context dropping, so that any stored state (like errors) can
    // free with the plugins still loaded.
    // Only drop when nothing else is using it (so we have reliable terminal cleanup).
    let mut logger = Some(logger);
    let errors = loop {
        match std::sync::Arc::try_unwrap(logger.take().unwrap()) {
            Ok(mut l) => {
                break l.get_mut().unwrap().take_errors();
            }
            Err(l) => {
                logger = Some(l);
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
        }
    };

    match (result, errors.len()) {
        (Ok(v), 0) => Ok(v),
        (o, _) => {
            // Write error output to stderr.
            use ergo_runtime::error::emit_diagnostics;

            let mut errors = errors;
            if let Err(e) = o {
                errors.insert(&e);
            }
            if errors.len() == 0 {
                Err("interrupted".into())
            } else {
                let err = error_output(opts.format)
                    .app_err("could not create error output from requested format");

                emit_diagnostics(&errors, sources.as_ref(), &mut TermToTermcolor(err))
                    .map_err(|e| e.to_string())
                    .and_then(|()| Err("one or more errors occurred".into()))
            }
        }
    }
}

fn main() {
    // Install the application logger.
    //
    // We write all logs to the configured (by env variable) file, falling back to the data local
    // dir or temp directory, and read from the {PROGRAM_NAME}_LOG environment variable to set the
    // application log level (which defaults to warn).
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
    {
        std::fs::create_dir_all(log_file.parent().expect("log file is a root directory"))
            .expect("failed to create log output directory");
        WriteLogger::init(
            std::env::var(format!("{}_LOG", PROGRAM_NAME.to_uppercase()))
                .map(|v| simplelog::LevelFilter::from_str(&v).expect("invalid program log level"))
                .unwrap_or(simplelog::LevelFilter::Warn),
            simplelog::Config::default(),
            std::fs::File::create(log_file.clone()).expect("failed to open log file for writing"),
        )
        .unwrap();
    }

    // Store the terminal state for future use.
    unsafe { terminal_state::store() };

    // Install a panic handler that will clean up the terminal and write the panic to the log.
    {
        std::panic::set_hook(Box::new(move |info| {
            log::error!("{}", info);
            unsafe { terminal_state::restore() };
            eprintln!(
                "A fatal error occurred in ergo. Please send the program log file ({}) and any other details to the development team.",
                log_file.display()
            );
            std::io::stderr().flush().unwrap();
        }));
    }

    // Parse arguments
    let app = Opts::clap().settings(&[structopt::clap::AppSettings::TrailingVarArg]);
    let mut opts = Opts::from_clap(&app.get_matches());

    // Additional opts logic.
    if opts.doc {
        opts.page ^= true;
    }
    let doc = opts.doc;
    let paging_enabled = opts.page;

    let (output, is_terminal) =
        output(opts.format, !opts.stop).app_err("could not create output with requested format");

    let result = run(opts, output);

    // Restore terminal state prior to the pager being used.
    unsafe { terminal_state::restore() };

    if paging_enabled {
        pager::Pager::with_default_pager(if cfg!(target_os = "macos") {
            // macos less is old and buggy, not working correctly with `-F`
            "less -R"
        } else {
            "less -R -F"
        })
        .setup();
    }

    match result {
        Ok(s) => {
            if doc && is_terminal {
                termimad::print_text(&s);
            } else {
                println!("{}", s);
            }
        }
        Err(e) => {
            // Don't use err_exit as that restores terminal state, which may mess with a pager.
            eprintln!("{}", e);
            std::io::stderr().flush().unwrap();
            std::process::exit(1);
        }
    }
}
