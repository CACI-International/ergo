mod evaluate;
mod lsp;

pub use evaluate::Evaluate;
pub use lsp::Lsp;

pub trait Command {
    fn run(self) -> Result<(), String>;
}
