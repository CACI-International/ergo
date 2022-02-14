mod evaluate;
mod format;
mod lsp;

pub use evaluate::Evaluate;
pub use format::Format;
pub use lsp::Lsp;

pub trait Command {
    fn run(self) -> Result<(), String>;
}
