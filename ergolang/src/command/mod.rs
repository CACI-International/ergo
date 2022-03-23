mod evaluate;
mod format;
mod lsp;
mod migrate;

pub use evaluate::Evaluate;
pub use format::Format;
pub use lsp::Lsp;
pub use migrate::Migrate;

pub trait Command {
    fn run(self) -> Result<(), String>;
}
