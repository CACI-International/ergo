//! Application runtime options.

use grease::LogLevel;
pub use structopt::StructOpt;

#[derive(Debug, StructOpt)]
/// So you want to run some tasks?
pub struct Opts {
    #[structopt(long = "log")]
    /// The log level used for tasks. Defaults to "warn".
    pub log_level: Option<LogLevel>,

    #[structopt(long)]
    /// Whether to display a tree of values rather than running tasks.
    pub tree: bool,

    /// Arguments for loading the value(s) to run.
    /// All additional arguments are run as if "so <args>..." were executed in a script.
    pub args: Vec<String>,
}
