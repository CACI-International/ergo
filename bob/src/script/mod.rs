//! Script loading and execution.

use grease::{Context, Plan};

mod ast;
mod runtime;

pub use ast::{FileSource, Source, StringSource};
pub use runtime::script_deep_eval;
pub use runtime::script_types as types;

/// A loaded script.
pub struct Script {
    ast: ast::Script,
}

/// Create a script context from a context builder.
pub fn script_context(
    cb: grease::ContextBuilder,
) -> Result<Context<runtime::Context>, grease::BuilderError> {
    let mut ctx = runtime::Context::default();

    // Add initial environment functions
    let env = vec![
        ("do", runtime::do_::builtin()),
        ("exec", runtime::exec::builtin()),
        ("has", runtime::has::builtin()),
        ("load", runtime::load::builtin()),
        ("map", runtime::map::builtin()),
        ("path", runtime::path::builtin()),
        ("track", runtime::track::builtin()),
    ];
    for (k, v) in env.into_iter() {
        ctx.env_insert(k.into(), Source::builtin(v));
    }

    cb.build_with(ctx).map(|mut ctx| {
        // Add initial traits
        ctx.traits.add(::exec::trait_generator);
        ctx
    })
}

impl Script {
    /// Load a script from a character stream.
    pub fn load(src: Source<()>) -> Result<Self, ast::Error> {
        ast::load(src).map(|ast| Script { ast })
    }
}

impl Plan<runtime::Context> for Script {
    type Output = <ast::Script as Plan<runtime::Context>>::Output;

    fn plan(self, ctx: &mut Context<runtime::Context>) -> Self::Output {
        self.ast.plan(ctx)
    }
}

#[cfg(test)]
mod test {
    use super::types::*;
    use super::*;
    use grease::Value;

    #[derive(Clone, Debug)]
    enum ScriptResult {
        SRUnit,
        SRString(&'static str),
        SRArray(&'static [ScriptResult]),
        SRMap(&'static [(&'static str, ScriptResult)]),
        SRAny,
    }

    use ScriptResult::*;

    #[test]
    fn unit() -> Result<(), String> {
        script_eval_to("[$]", SRArray(&[SRUnit]))
    }

    #[test]
    fn array() -> Result<(), String> {
        script_eval_to("$ [a,b]", SRArray(&[SRString("a"), SRString("b")]))
    }

    #[test]
    fn block() -> Result<(), String> {
        script_eval_to(
            "$ {alpha=one,beta=two}",
            SRMap(&[("alpha", SRString("one")), ("beta", SRString("two"))]),
        )
    }

    #[test]
    fn comment() -> Result<(), String> {
        script_eval_to(
            "# comment comment comment\n$ something",
            SRString("something"),
        )
    }

    #[test]
    fn index() -> Result<(), String> {
        script_eval_to("$ [a,b]:0", SRString("a"))?;
        script_eval_to("$ {alpha=one,beta=two}:beta", SRString("two"))
    }

    #[test]
    fn bindings() -> Result<(), String> {
        script_eval_to("var = $ something; var", SRString("something"))?;
        script_eval_to(
            "var = $ {inner=[blah,{v=\"test()\"}]}; var:inner:1:v",
            SRString("test()"),
        )
    }

    #[test]
    fn exec() -> Result<(), String> {
        script_eval_to("(echo hello):stdout", SRAny)
    }

    #[test]
    fn exec_failure() -> Result<(), String> {
        let mut fail = script_eval("false:stdout")?;
        assert!(futures::executor::block_on(&mut fail).is_err());
        Ok(())
    }

    #[test]
    fn function() -> Result<(), String> {
        script_eval_to("f = fn $ a\nf something", SRString("a"))?;
        script_eval_to("second = fn @:1\nsecond a b", SRString("b"))?;
        script_eval_to(
            "f = fn {\n  a = $ ($ $@:0)\n  b = @:2\n}\nf 1 2 3",
            SRMap(&[("a", SRString("1")), ("b", SRString("3"))]),
        )
    }

    fn script_eval_to(s: &str, expected: ScriptResult) -> Result<(), String> {
        val_match(script_eval(s)?, expected)
    }

    trait ExpectOk {
        type Output;

        fn expect_ok(self, msg: &str) -> Result<Self::Output, String>;
    }

    impl<T, E> ExpectOk for Result<T, E> {
        type Output = T;

        fn expect_ok(self, msg: &str) -> Result<T, String> {
            self.map_err(|_| msg.into())
        }
    }

    fn val_match(v: Value, expected: ScriptResult) -> Result<(), String> {
        match expected {
            SRUnit => v
                .typed::<ScriptUnit>()
                .map(|_| ())
                .expect_ok("expected unit"),
            SRString(s) => v
                .typed::<ScriptString>()
                .expect_ok("expected string")
                .and_then(|v| v.get())
                .and_then(|st| {
                    if st.as_ref() == s {
                        Ok(())
                    } else {
                        Err("string mismatch".into())
                    }
                }),
            SRArray(arr) => v
                .typed::<ScriptArray>()
                .expect_ok("expected array")
                .and_then(|v| v.get())
                .map(|v| v.owned())
                .and_then(|ScriptArray(varr)| {
                    if varr.len() != arr.len() {
                        Err("array length mismatch".into())
                    } else {
                        varr.into_iter()
                            .zip(arr)
                            .map(|(v, e)| val_match(v.unwrap(), e.clone()))
                            .collect()
                    }
                }),
            SRMap(entries) => v
                .typed::<ScriptMap>()
                .expect_ok("expected map")
                .and_then(|v| v.get())
                .map(|v| v.owned())
                .and_then(|ScriptMap(mut m)| {
                    if m.len() != entries.len() {
                        Err("map length mismatch".into())
                    } else {
                        entries
                            .iter()
                            .map(|(k, e)| {
                                m.remove(*k)
                                    .map(|v| val_match(v.unwrap(), e.clone()))
                                    .unwrap_or(Err("missing expected key".into()))
                            })
                            .collect()
                    }
                }),
            SRAny => Ok(()),
        }
    }

    fn script_eval(s: &str) -> Result<Value, String> {
        let mut ctx = script_context(Context::builder()).map_err(|e| e.to_string())?;
        ctx.plan(Script::load(Source::new(StringSource(s.to_owned()))).map_err(|e| e.to_string())?)
            .map_err(|e| e.to_string())
            .map(|sv| sv.unwrap())
    }
}
