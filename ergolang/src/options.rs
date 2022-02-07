pub use structopt::StructOpt;

#[derive(Debug, StructOpt)]
/// Effio ergo sum.
///
/// Ergo is a runtime and language built for lazy task execution.
pub enum Command {
    /// Load and evaluate a value.
    Eval,
    /// Run the language server.
    Lsp,
}
