//! Script loading and execution.

use grease::{Context, Plan};

mod ast;
mod runtime;

pub use ast::{FileSource, Source, StringSource};
pub use runtime::Data;

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
    ctx.env_insert("exec".into(), runtime::exec::exec_builtin());
    ctx.env_insert("track".into(), runtime::track::track_builtin());
    ctx.env_insert("map".into(), runtime::map::map_builtin());
    ctx.env_insert("do".into(), runtime::do_::do_builtin());
    ctx.env_insert("path".into(), runtime::path::path_builtin());
    cb.build_with(ctx)
}

impl Script {
    /// Load a script from a character stream.
    pub fn load(src: Source<()>) -> Result<Self, ast::Error> {
        ast::load(src).map(|ast| Script { ast })
    }
}

impl Plan<runtime::Context> for Script {
    type Output = Result<runtime::Data, Source<runtime::Error>>;

    fn plan(self, ctx: &mut Context<runtime::Context>) -> Self::Output {
        self.ast.plan(ctx)
    }
}

#[cfg(test)]
mod test {
    use super::runtime::Data;
    use super::*;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    #[test]
    fn array() -> Result<(), String> {
        script_eval_to(
            "$ [a,b]",
            Data::Array(vec![Data::String("a".into()), Data::String("b".into())]),
        )
    }

    #[test]
    fn block() -> Result<(), String> {
        script_eval_to(
            "$ {alpha=one,beta=two}",
            Data::Map(HashMap::from_iter(vec![
                ("alpha".to_owned(), Data::String("one".into())),
                ("beta".to_owned(), Data::String("two".into())),
            ])),
        )
    }

    #[test]
    fn comment() -> Result<(), String> {
        script_eval_to(
            "# comment comment comment\n$ something",
            Data::String("something".into()),
        )
    }

    #[test]
    fn index() -> Result<(), String> {
        script_eval_to("$ [a,b]:0", Data::String("a".into()))?;
        script_eval_to("$ {alpha=one,beta=two}:beta", Data::String("two".into()))
    }

    #[test]
    fn bindings() -> Result<(), String> {
        script_eval_to("var = $ something; var", Data::String("something".into()))?;
        script_eval_to(
            "var = $ {inner=[blah,{v=\"test()\"}]}; var:inner:1:v",
            Data::String("test()".into()),
        )
    }

    #[test]
    fn exec() -> Result<(), String> {
        let m = script_eval("(echo hello):stdout")?;
        if let Data::Value(_) = m {
            Ok(())
        } else {
            panic!("expected value");
        }
    }

    #[test]
    fn exec_failure() -> Result<(), String> {
        let mut fail = script_eval("false:stdout")?;
        assert!(futures::executor::block_on(&mut fail).is_err());
        Ok(())
    }

    #[test]
    fn function() -> Result<(), String> {
        script_eval_to("f = fn $ a\nf something", Data::String("a".into()))?;
        script_eval_to("second = fn @:1\nsecond a b", Data::String("b".into()))?;
        script_eval_to(
            "f = fn {\n  a = $ ($ $@:0)\n  b = @:2\n}\nf 1 2 3",
            Data::Map(HashMap::from_iter(vec![
                ("a".into(), Data::String("1".into())),
                ("b".into(), Data::String("3".into())),
            ])),
        )
    }

    fn script_eval_to(s: &str, expected: Data) -> Result<(), String> {
        let mut val = script_eval(s)?;
        futures::executor::block_on(&mut val)?;
        assert!(dbg!(val) == expected);
        Ok(())
    }

    fn script_eval(s: &str) -> Result<Data, String> {
        let mut ctx = script_context(Context::builder()).map_err(|e| e.to_string())?;
        ctx.plan(Script::load(Source::new(StringSource(s.to_owned()))).map_err(|e| e.to_string())?)
            .map_err(|e| e.to_string())
    }
}
