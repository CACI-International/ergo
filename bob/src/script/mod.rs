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
    ctx.env_insert("exec".to_owned(), runtime::exec::exec_builtin());
    cb.build_with(ctx)
}

impl Script {
    /// Load a script from a character stream.
    pub fn load<I: IntoIterator<Item = char>>(i: I) -> Result<Self, ast::Error> {
        ast::load(i.into_iter()).map(|ast| Script { ast })
    }
}

impl Plan<runtime::Context> for Script {
    type Output = Result<HashMap<String, runtime::Data>, runtime::Error>;

    fn plan(self, ctx: &mut Context<runtime::Context>) -> Self::Output {
        self.ast.plan(ctx)
    }
}

#[cfg(test)]
mod test {
    use super::runtime::Data;
    use super::*;
    use std::iter::FromIterator;

    #[test]
    fn array() -> Result<(), String> {
        script_eval_to(
            "[a,b]",
            Data::Array(vec![Data::String("a".into()), Data::String("b".into())]),
        )
    }

    #[test]
    fn block() -> Result<(), String> {
        script_eval_to(
            "{alpha=one,beta=two}",
            Data::Map(HashMap::from_iter(vec![
                ("alpha".to_owned(), Data::String("one".into())),
                ("beta".to_owned(), Data::String("two".into())),
                ("*".to_owned(), Data::String("".into())),
            ])),
        )
    }

    #[test]
    fn index() -> Result<(), String> {
        script_eval_to("[a,b]:0", Data::String("a".into()))?;
        script_eval_to("{alpha=one,beta=two}:beta", Data::String("two".into()))
    }

    #[test]
    fn bindings() -> Result<(), String> {
        script_eval_to("var = something; var", Data::String("something".into()))?;
        script_eval_to(
            "var = {inner=[blah,{v=\"test()\"; v}]}; var:inner:1:*",
            Data::String("test()".into()),
        )
    }

    #[test]
    fn exec() -> Result<(), String> {
        let m = script_eval("(echo hello):stdout")?;
        if let Data::Value(_) = m.get("*").unwrap() {
            Ok(())
        } else {
            panic!("expected value");
        }
    }

    #[test]
    fn exec_failure() -> Result<(), String> {
        let mut fail = script_eval("false:stdout")?;
        let mut d = fail.remove("*").unwrap();
        assert!(futures::executor::block_on(&mut d).is_err());
        Ok(())
    }

    fn script_eval_to(s: &str, expected: Data) -> Result<(), String> {
        script_eval_with(s, "*", expected)
    }

    fn script_eval_with(s: &str, index: &str, expected: Data) -> Result<(), String> {
        let mut map = script_eval(s)?;
        let mut val = map.remove(index).ok_or(format!("index not found: {}", index))?;
        futures::executor::block_on(&mut val)?;
        assert!(val == expected);
        Ok(())
    }

    fn script_eval(s: &str) -> Result<HashMap<String, Data>, String> {
        let mut ctx = script_context(Context::builder()).map_err(|e| e.to_string())?;
        ctx.plan(Script::load(s.chars()).map_err(|e| e.to_string())?)
            .map_err(|e| e.to_string())
    }
}
