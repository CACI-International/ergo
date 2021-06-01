//! Application runtime options.

use ergo_runtime::context::LogLevel;
pub use structopt::StructOpt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OutputFormat {
    Basic,
    Pretty,
    Auto,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "auto" => Ok(OutputFormat::Auto),
            "basic" => Ok(OutputFormat::Basic),
            "pretty" => Ok(OutputFormat::Pretty),
            _ => Err(format!("invalid output format '{}'", s)),
        }
    }
}

fn parse_error_limit(arg: &str) -> Result<Option<usize>, std::num::ParseIntError> {
    if arg == "none" {
        Ok(None)
    } else {
        use std::str::FromStr;
        usize::from_str(arg).map(Some)
    }
}

#[derive(Debug, StructOpt)]
/// Effio ergo sum.
///
/// Ergo is a runtime and language built for lazy task execution.
pub struct Opts {
    #[structopt(long = "log", default_value = "warn")]
    /// The log level used for tasks.
    ///
    /// May be "debug", "info", "warn", or "error".
    pub log_level: LogLevel,

    #[structopt(long, default_value = "auto")]
    /// The output format.
    ///
    /// May be "auto", "basic", or "pretty".
    pub format: OutputFormat,

    #[structopt(short = "E", long, default_value = "3", parse(try_from_str = parse_error_limit))]
    /// The number of frames of context to display with each error.
    ///
    /// Specify "none" to display all frames.
    pub error_limit: std::option::Option<usize>,

    #[structopt(short, long)]
    /// The maximum number of jobs to run concurrently.
    ///
    /// If unspecified, the number of cpus is used.
    pub jobs: Option<usize>,

    #[structopt(long, default_value = concat!(".", env!("CARGO_PKG_NAME"), "_work"))]
    /// The storage directory for the runtime.
    ///
    /// If a relative path, it will be made relative to the furthest ancestor directory that is a
    /// workspace. If none are found, the current directory is used.
    pub storage: std::path::PathBuf,

    #[structopt(short, long)]
    /// Prints help information.
    pub help: bool,

    #[structopt(short, long)]
    /// Clear the storage directory prior to executing.
    pub clean: bool,

    #[structopt(short, long)]
    /// Check for common syntax mistakes while executing the script.
    pub lint: bool,

    #[structopt(short, long)]
    /// Display documentation for the final value rather than executing it.
    ///
    /// This option toggles the `--page` state.
    pub doc: bool,

    #[structopt(long = "doc-write")]
    /// Generate documentation for the final value rather than executing it, and write it to the
    /// filesystem.
    ///
    /// This option behaves similarly to `--doc`, but rather than displaying the documentation, it
    /// is written to the given path.
    pub doc_path: Option<std::path::PathBuf>,

    #[structopt(short, long)]
    /// Page the evaluation output.
    pub page: bool,

    #[structopt(short = "S", long)]
    /// Whether to stop immediately when an error occurs.
    pub stop: bool,

    #[structopt(short, long)]
    /// Evaluate the arguments as an expression.
    ///
    /// That is, evaluate `[args]` instead of `ergo [args]`.
    pub expression: bool,

    /// Arguments for loading the value(s) to run.
    ///
    /// All additional arguments are run as if "ergo <args>..." were executed in a script, unless
    /// `-e` is specified.
    ///
    /// If `-e` is _not_ used, the first `:` is translated into `|>:` for convenience. For example,
    /// `ergo std:fs:read` becomes `ergo std |>:fs:read` (the same as `(ergo std):fs:read`). Note
    /// that this does _not_ occur if the first `:` is already preceded by `|>`.
    pub args: Vec<String>,
}
