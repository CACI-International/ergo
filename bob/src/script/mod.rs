//! Script loading and execution.

use grease::{Context, Plan};
use std::collections::HashMap;

mod ast;
mod runtime;

/// A loaded script.
pub struct Script {
    ast: ast::Script,
}

/// Create a script context from a context builder.
pub fn script_context(
    cb: grease::ContextBuilder,
) -> Result<Context<runtime::Context>, grease::BuilderError> {
    let mut ctx = runtime::Context::default();

    // Add 'exec' to the initial environment
    ctx.current_env().insert("exec".to_owned(), runtime::exec::exec_builtin());
    cb.build_with(ctx)
}

impl Script {
    /// Load a script from a character stream.
    pub fn load<I: Iterator<Item = char>>(i: I) -> Result<Self, ast::Error> {
        ast::load(i).map(|ast| Script { ast })
    }
}

impl Plan<runtime::Context> for Script {
    type Output = Result<HashMap<String, runtime::Data>, runtime::Error>;

    fn plan(self, ctx: &mut Context<runtime::Context>) -> Self::Output {
        self.ast.plan(ctx)
    }
}
