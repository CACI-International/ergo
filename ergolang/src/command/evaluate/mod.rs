//! The evaluation command.

use crate::AppErr;
use ergo_runtime::{
    abi_stable::std_types::{RDuration, ROption, RSlice, RString},
    context::{LogEntry, LogLevel, LogTarget, LogTaskKey},
    try_value, Error,
};
use ergo_script::constants::{PROGRAM_NAME, WORKSPACE_NAME};
use ergo_script::Runtime;

mod output;
mod render_markdown;
mod sync;

/// Constant values shared throughout the program.
mod constants {
    use directories;
    pub fn app_dirs() -> Option<directories::ProjectDirs> {
        directories::ProjectDirs::from("", "", super::PROGRAM_NAME)
    }
}

use output::{error as error_output, output, Output, OutputInstance, TermToTermcolor};

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

#[derive(Debug, PartialEq, Eq, Clone, Copy, clap::ArgEnum)]
pub enum OutputFormat {
    Basic,
    Pretty,
    Auto,
}

#[derive(Debug, clap::Args)]
/// Effio ergo sum.
///
/// Ergo is a runtime and language built for lazy task execution.
#[clap(setting = clap::AppSettings::TrailingVarArg)]
pub struct Evaluate {
    #[clap(long = "log", default_value = "warn")]
    /// The runtime log level.
    ///
    /// May be "debug", "info", "warn", or "error".
    pub log_level: LogLevel,

    #[clap(long, default_value = "auto", arg_enum)]
    /// The output format.
    pub format: OutputFormat,

    #[clap(short, long)]
    /// The maximum number of jobs to run concurrently.
    ///
    /// If unspecified, the number of cpus is used.
    pub jobs: Option<usize>,

    #[clap(long, default_value = concat!(".ergo_work"))]
    /// The storage directory for the runtime.
    ///
    /// If a relative path, it will be made relative to the furthest ancestor directory that is a
    /// workspace. If none are found, the current directory is used.
    pub storage: std::path::PathBuf,

    #[clap(short, long)]
    /// Clear the storage directory prior to executing.
    pub clean: bool,

    #[clap(short, long, require_equals = true)]
    /// Check for common syntax mistakes while executing the script.
    ///
    /// If the value is omitted, defaults to "on".
    /// May be "off", "on", or "aggressive".
    pub lint: Option<Option<ergo_script::LintLevel>>,

    #[clap(skip)]
    /// Enable evaluation backtraces on errors.
    pub backtrace: bool,

    #[clap(short, long)]
    /// Display documentation for the final value rather than executing it.
    ///
    /// This option toggles the `--page` state.
    pub doc: bool,

    #[clap(long = "doc-write")]
    /// Generate documentation for the final value rather than executing it, and write it to the
    /// filesystem.
    ///
    /// This option behaves similarly to `--doc`, but rather than displaying the documentation, it
    /// is written to the given path.
    pub doc_path: Option<std::path::PathBuf>,

    #[clap(short, long)]
    /// Page the evaluation output.
    pub page: bool,

    #[clap(short = 'S', long)]
    /// Whether to stop immediately when an error occurs.
    pub stop: bool,

    #[clap(short, long)]
    /// Evaluate the arguments as an expression.
    ///
    /// That is, evaluate `[args]` instead of `ergo [args]`.
    pub expression: bool,

    /// Arguments for loading the value(s) to run.
    ///
    /// All additional arguments are run as if `ergo <args>...` were executed in a script with
    /// literal String arguments (to translate from shell/OS invocations more faithfully), unless
    /// `-e` is specified.
    ///
    /// If `-e` is _not_ used, the first argument is treated specially: any `:` characters are
    /// preserved as index operations on the loaded value.
    ///
    /// If no arguments are provided, or if the first argument cannot be resolved to a module in
    /// the load path, instead of loading a script with `ergo <args>...`, `workspace:command
    /// <args>...` is evaluated (following the same special treatment of the first argument).
    pub args: Vec<String>,
}

impl super::Command for Evaluate {
    fn run(mut self) -> Result<(), String> {
        // Additional options logic.
        if self.doc {
            self.page ^= true;
        }
        let doc = self.doc;
        let paging_enabled = self.page;

        let (output, is_terminal) = output(self.format, !self.stop)
            .app_err("could not create output with requested format")?;

        let result = self.eval(output);

        // Restore terminal state prior to the pager being used.
        unsafe { crate::terminal_state::restore() };

        // Check color support prior to possibly activating the pager.
        let color_support = render_markdown::ColorSupport::new();

        if paging_enabled {
            pager::Pager::with_default_pager(if cfg!(target_os = "macos") {
                // macos less is old and buggy, not working correctly with `-F`
                "less -R"
            } else {
                "less -R -F"
            })
            .setup();
        }

        let s = result?;
        if doc && is_terminal {
            render_markdown::render_markdown(color_support, &s);
        } else {
            println!("{}", s);
        }
        Ok(())
    }
}

impl Evaluate {
    fn eval(self, mut output: OutputInstance) -> Result<String, String> {
        output.set_log_level(self.log_level);
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

        let storage_directory = storage_dir_root.join(self.storage);

        // Clean storage directory if requested.
        if self.clean && storage_directory.exists() {
            std::fs::remove_dir_all(&storage_directory)
                .app_err("failed to clean storage directory")?;
        }

        // Get the load path from exe location and user directories.
        let load_path = {
            let mut load_paths = Vec::new();

            // Add neighboring lib directories when running in a [prefix]/bin directory.
            let neighbor_data_dir = std::env::current_exe().ok().and_then(|path| {
                path.parent().and_then(|parent| {
                    if parent.file_name() == Some("bin".as_ref()) {
                        let path = parent
                            .parent()
                            .expect("must have parent directory")
                            .join("lib")
                            .join(PROGRAM_NAME);
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

            // Add the data directory prior to any user lib dir.
            if let Some(dir) = neighbor_data_dir {
                load_paths.push(dir);
            }

            // Add user lib dir.
            if let Some(proj_dirs) = constants::app_dirs() {
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
                .threads(self.jobs)
                .keep_going(!self.stop)
                .error_handler(move |e: Error| {
                    if let Some(logger) = error_logger.upgrade() {
                        if let Ok(mut logger) = logger.lock() {
                            logger.new_error(e);
                        }
                    }
                }),
            load_path,
        )
        .expect("failed to create script context");

        if let Some(level) = self.lint {
            runtime.lint_level(level.unwrap_or(ergo_script::LintLevel::On));
        }

        runtime.backtrace(self.backtrace);

        // Set interrupt signal handler to abort tasks.
        //
        // Keep signal_handler_task in scope until the handler is no longer needed.
        let (signal_handler_task, task_ref) = sync::Scoped::new_pair(runtime.ctx.task.clone());
        {
            ctrlc::set_handler(move || task_ref.with(|t| t.abort()))
                .app_err("failed to set signal handler")?;
        }

        // Build script string to evaluate
        let mut to_eval = if self.expression {
            if self.args.is_empty() {
                "()".into()
            } else {
                let mut eval_args = String::new();
                for arg in self.args {
                    if !eval_args.is_empty() {
                        eval_args.push(' ');
                    }
                    eval_args.push_str(&arg);
                }
                eval_args
            }
        } else {
            let mut args = self.args.into_iter();
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
                    let use_workspace_command = runtime
                        .resolve_script_path(None, to_load.as_ref())
                        .is_none();
                    // Inspect the first effective argument to determine whether to invoke a normal load
                    // or the workspace:command function.
                    s += if use_workspace_command {
                        "workspace:command"
                    } else {
                        "load"
                    };
                    s.push(' ');
                    s += &string_quote(to_load);

                    let mut needs_call = !use_workspace_command;

                    // Apply indices
                    let mut first = true;
                    for index in indices {
                        if first {
                            s += " |>";
                            needs_call = false;
                            first = false;
                        }
                        s += ":";
                        s += &string_quote(index);
                    }

                    // Pass all remaining arguments as strings.
                    for a in args {
                        if needs_call {
                            s += " |>";
                            needs_call = false;
                        }
                        s.push(' ');
                        s += &string_quote(a);
                    }
                    s
                }
            }
        };

        if self.doc {
            to_eval = format!("doc ({})", to_eval);
        } else if let Some(path) = self.doc_path {
            to_eval = format!(
                "doc:write {} ({})",
                string_quote(path.display().to_string()),
                to_eval
            );
        }

        let loaded = runtime.evaluate_string("<command line>", &to_eval);
        let progress = runtime.ctx.global.progress.clone();

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

            let sources = runtime.ctx.global.diagnostic_sources();
            let progress = runtime.ctx.global.progress.clone();
            runtime.ctx.global.hooks().shutdown();
            drop(runtime);

            drop(complete_send.send(()));
            (result, sources, progress)
        });

        loop {
            if let Ok(mut g) = logger.lock() {
                if progress.made_progress() {
                    g.indicate_progress();
                }
                g.update();
            }
            match complete.recv_timeout(std::time::Duration::from_millis(50)) {
                Err(std::sync::mpsc::RecvTimeoutError::Timeout) => continue,
                _ => break,
            }
        }

        let (result, sources, progress) = exec_thread.join().unwrap();

        // Drop the logger prior to the context dropping, so that any stored state (like errors) can
        // free with the plugins still loaded.
        // Only drop when nothing else is using it (so we have reliable terminal cleanup).
        let mut logger = Some(logger);
        let errors = loop {
            match std::sync::Arc::try_unwrap(logger.take().unwrap()) {
                Ok(mut l) => {
                    let deadlock_errors = progress.deadlock_errors();
                    break if deadlock_errors.len() > 0 {
                        deadlock_errors
                    } else {
                        l.get_mut().unwrap().take_errors()
                    };
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
                    let err = error_output(self.format)
                        .app_err("could not create error output from requested format")?;

                    emit_diagnostics(&errors, sources.as_ref(), &mut TermToTermcolor(err))
                        .map_err(|e| e.to_string())
                        .and_then(|()| Err("one or more errors occurred".into()))
                }
            }
        }
    }
}
