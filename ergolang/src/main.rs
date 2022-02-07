use ergo_script::constants::PROGRAM_NAME;
use std::io::Write;
use std::str::FromStr;

mod app_err;
mod command;
pub mod terminal_state;

pub use app_err::AppErr;

#[derive(Debug, clap::Parser)]
/// Effio ergo sum.
///
/// Ergo is a runtime and language built for lazy task execution.
#[clap(version = env!("CARGO_PKG_VERSION"))]
enum Command {
    /// Load and evaluate a value.
    Evaluate(command::Evaluate),
    /// Run the language server.
    Lsp(command::Lsp),
}

impl command::Command for Command {
    fn run(self) -> Result<(), String> {
        match self {
            Command::Evaluate(a) => a.run(),
            Command::Lsp(a) => a.run(),
        }
    }
}

#[derive(Debug, clap::Parser)]
#[clap(version = env!("CARGO_PKG_VERSION"))]
struct ImplicitCommand<T: clap::Args> {
    #[clap(flatten)]
    args: T,
}

impl<T: command::Command + clap::Args> command::Command for ImplicitCommand<T> {
    fn run(self) -> Result<(), String> {
        self.args.run()
    }
}

enum Error {
    Clap(clap::Error),
    Other(String),
}

impl Error {
    pub fn exit(self) -> ! {
        match self {
            Error::Clap(e) => e.exit(),
            Error::Other(s) => {
                eprintln!("{}", s);
                std::io::stderr().flush().unwrap();
                std::process::exit(1);
            }
        }
    }
}

fn cli<T: command::Command + clap::Parser>() -> Result<(), Error> {
    let me = T::try_parse().map_err(Error::Clap)?;
    me.run().map_err(Error::Other)
}

#[cfg(not(feature = "backtrace"))]
fn log_panic(info: &std::panic::PanicInfo<'_>) {
    log::error!("{}", info);
}

#[cfg(feature = "backtrace")]
fn log_panic(info: &std::panic::PanicInfo<'_>) {
    log::error!("{}\n{:?}", info, backtrace::Backtrace::new());
}

fn main() {
    // Install the application logger.
    //
    // We write all logs to the configured (by env variable) file, falling back to the data local
    // dir or temp directory, and read from the {PROGRAM_NAME}_LOG environment variable to set the
    // application log level (which defaults to warn).
    let mut delete_log = true;
    let log_file = {
        if let Some(f) = std::env::var_os(format!("{}_LOG_FILE", PROGRAM_NAME.to_uppercase())) {
            delete_log = false;
            f.into()
        } else {
            let mut f = std::env::temp_dir();
            f.push(format!("{}-{}.log", PROGRAM_NAME, std::process::id()));
            f
        }
    };
    {
        std::fs::create_dir_all(log_file.parent().expect("log file is a root directory"))
            .expect("failed to create log output directory");
        simplelog::WriteLogger::init(
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
        let log_file = log_file.clone();
        std::panic::set_hook(Box::new(move |info| {
            log_panic(info);
            unsafe { terminal_state::restore() };
            eprintln!(
                "A fatal error occurred in ergo. Please send the program log file ({}) and any other details to the development team.",
                log_file.display()
            );
            std::io::stderr().flush().unwrap();
        }));
    }

    // Run the appropriate command.
    let result = loop {
        if let Some(proc) = std::env::args_os().next() {
            if let Some(name) = std::path::PathBuf::from(proc).file_name() {
                if name == "ergo" {
                    break cli::<ImplicitCommand<command::Evaluate>>();
                } else if name == "ergo-lsp" {
                    break cli::<ImplicitCommand<command::Lsp>>();
                }
            }
        }
        break cli::<Command>();
    };

    // Restore terminal state.
    unsafe { terminal_state::restore() };

    // Clean up the log (this won't occur on panic or if the log was set by an env variable).
    if delete_log {
        drop(std::fs::remove_file(log_file));
    }

    // Display any error and exit.
    if let Err(e) = result {
        e.exit();
    }
}
