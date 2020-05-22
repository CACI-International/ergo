//! Application runtime options.

use grease::LogLevel;
pub use structopt::StructOpt;

#[derive(Debug, StructOpt)]
/// So you want to run some tasks?
pub struct Opts {
    #[structopt(long = "log", default_value = "warn")]
    /// The log level used for tasks. May be "debug", "info", "warn", or "error".
    pub log_level: LogLevel,

    #[structopt(short, long)]
    /// The maximum number of jobs to run concurrently. If unspecified, the number of cpus is used.
    pub jobs: Option<usize>,

    /// Arguments for loading the value(s) to run.
    /// All additional arguments are run as if "so <args>..." were executed in a script.
    pub args: Vec<String>,
}
