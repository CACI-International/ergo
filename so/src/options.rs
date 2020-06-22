//! Application runtime options.

use grease::LogLevel;
pub use structopt::StructOpt;

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, StructOpt)]
/// So you want to run some tasks?
pub struct Opts {
    #[structopt(long = "log", default_value = "warn")]
    /// The log level used for tasks. May be "debug", "info", "warn", or "error".
    pub log_level: LogLevel,

    #[structopt(long, default_value = "auto")]
    /// The output format. May be "auto", "basic", or "pretty".
    pub format: OutputFormat,

    #[structopt(short, long)]
    /// The maximum number of jobs to run concurrently. If unspecified, the number of cpus is used.
    pub jobs: Option<usize>,

    #[structopt(long, default_value = concat!(".", env!("CARGO_PKG_NAME"), "_work"))]
    /// The storage directory for the runtime.
    pub storage: std::path::PathBuf,

    #[structopt(short = "S", long)]
    /// Whether to stop immediately when an error occurs.
    pub stop: bool,

    /// Arguments for loading the value(s) to run.
    /// All additional arguments are run as if "so <args>..." were executed in a script.
    pub args: Vec<String>,
}
