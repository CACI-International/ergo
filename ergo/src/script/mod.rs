//! Script loading and execution.

use crate::constants::*;
pub use ergo_runtime::source::{FileSource, Source, StringSource};
use ergo_runtime::{ContextEnv, EvalResult, Runtime, ScriptEnv};
use futures::future::FutureExt;

mod ast;
mod base;
mod runtime;

use runtime::*;

/// A loaded script.
pub struct Script {
    ast: ast::Script,
    top_level_env: ScriptEnv,
}

/// Create a script context from a context builder.
pub fn script_context(
    cb: grease::runtime::ContextBuilder,
) -> Result<Runtime, grease::runtime::BuilderError> {
    let mut global_env = ScriptEnv::default();
    {
        let mut insert = |k: &str, v| global_env.insert(k.into(), Ok(Source::builtin(v)).into());

        /* TODO restore something akin to this functionality
            if let Some(dirs) = app_dirs() {
                insert(
                    LOAD_PATH_BINDING,
                    types::ScriptArray(vec![dirs.config_dir().to_owned().into_value()])
                        .into_value(),
                );
            }
        */

        // Add initial environment functions
        let env = vec![(PROGRAM_NAME, base::load())];
        for (k, v) in env.into_iter() {
            insert(k, v);
        }
    }

    cb.build().map(|mut ctx| {
        // Add initial traits
        ergo_runtime::traits::traits(&mut ctx.traits);
        Runtime::new(ctx, global_env)
    })
}

impl Script {
    /// Load a script from a character stream.
    pub fn load(src: Source<()>) -> Result<Self, ast::Error> {
        ast::load(src).map(|ast| Script {
            ast,
            top_level_env: Default::default(),
        })
    }

    /// Configure the top-level environment.
    pub fn top_level_env(&mut self, env: ScriptEnv) {
        self.top_level_env = env;
    }

    pub async fn evaluate(self, ctx: &mut Runtime) -> EvalResult {
        // Swap existing env; loading a script should have a clean environment besides the
        // configured top-level env.
        let ast = self.ast;
        ctx.substituting_env(vec![self.top_level_env].into(), move |ctx| {
            Rt(ast).evaluate(ctx).boxed()
        })
        .await
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ergo_runtime::types;
    use grease::value::Value;

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
        script_eval_to("[a,b]", SRArray(&[SRString("a"), SRString("b")]))
    }

    #[test]
    fn block() -> Result<(), String> {
        script_eval_to(
            "{alpha=one,beta=two}",
            SRMap(&[("alpha", SRString("one")), ("beta", SRString("two"))]),
        )
    }

    #[test]
    fn block_set_shorthand() -> Result<(), String> {
        script_eval_to(
            "a = 1\nb = 2\n{c=3,a,b}",
            SRMap(&[
                ("a", SRString("1")),
                ("b", SRString("2")),
                ("c", SRString("3")),
            ]),
        )
    }

    #[test]
    fn comment() -> Result<(), String> {
        script_eval_to(
            "# comment comment comment\na = something\n$a",
            SRString("something"),
        )
    }

    #[test]
    fn index() -> Result<(), String> {
        script_eval_to("[a,b] 0", SRString("a"))?;
        script_eval_to("[a,b] 2", SRUnit)?;
        script_eval_to("{alpha=one,beta=two} beta", SRString("two"))?;
        script_eval_to("{alpha=one,beta=two} omega", SRUnit)?;
        script_eval_to("{alpha=[a,{key=b}]} alpha 1 key", SRString("b"))?;
        Ok(())
    }

    #[test]
    fn bindings() -> Result<(), String> {
        script_eval_to("var = something; $var", SRString("something"))?;
        script_eval_to(
            "var = {inner=[blah,{v=\"test()\"}]}; var inner 1 v",
            SRString("test()"),
        )?;
        Ok(())
    }

    #[test]
    fn function() -> Result<(), String> {
        script_eval_to("f = fn ^_ -> a\nf something", SRString("a"))?;
        script_eval_to("second = fn _ b ^_ -> $b\nsecond a b", SRString("b"))?;
        script_eval_to(
            "f = fn a _ b ^_ -> {\n  a = $a\n  b = $b\n}\nf 1 2 3",
            SRMap(&[("a", SRString("1")), ("b", SRString("3"))]),
        )
    }

    #[test]
    fn match_expr() -> Result<(), String> {
        script_eval_to(
            "match [1,2,3] { {^keys} = $keys, [a,b] = $a, [a,b,c] = $a }",
            SRString("1"),
        )?;
        script_eval_to(
            "match [1,2,3] { a = $a, [a,b,c] = $b }",
            SRArray(&[SRString("1"), SRString("2"), SRString("3")]),
        )?;
        Ok(())
    }

    #[test]
    fn match_failure() -> Result<(), String> {
        script_fail("match {a=[1,2]} { {b} = $b, [a,b] = $b }")
    }

    mod merge {
        use super::*;

        #[test]
        fn array() -> Result<(), String> {
            script_eval_to(
                "arr = [b,c]\n[a,^$arr,d]",
                SRArray(&[SRString("a"), SRString("b"), SRString("c"), SRString("d")]),
            )
        }

        #[test]
        fn array_invalid() -> Result<(), String> {
            script_fail("[a,^b,c]")?;
            script_fail("[^{a=1}]")?;
            Ok(())
        }

        #[test]
        fn block() -> Result<(), String> {
            script_eval_to(
                "{ a = 1, b = 2, ^{ c = 3, d = 4 }, e = $c }",
                SRMap(&[
                    ("a", SRString("1")),
                    ("b", SRString("2")),
                    ("c", SRString("3")),
                    ("d", SRString("4")),
                    ("e", SRString("3")),
                ]),
            )
        }

        #[test]
        fn block_script_scope() -> Result<(), String> {
            script_eval_to(
                "a = 1\n^{b = 2}\nc = $b",
                SRMap(&[
                    ("a", SRString("1")),
                    ("b", SRString("2")),
                    ("c", SRString("2")),
                ]),
            )
        }

        #[test]
        fn block_invalid() -> Result<(), String> {
            script_fail("{^a}")?;
            script_fail("{^[1,2]}")?;
            Ok(())
        }

        #[test]
        fn command_array() -> Result<(), String> {
            script_eval_to(
                "to_array = fn ^args -> $args\nto_array a ^[b,c] d ^[e,f]",
                SRArray(&[
                    SRString("a"),
                    SRString("b"),
                    SRString("c"),
                    SRString("d"),
                    SRString("e"),
                    SRString("f"),
                ]),
            )
        }

        #[test]
        fn command_block() -> Result<(), String> {
            script_eval_to(
                "to_array = fn ^args ^{^_} -> $args\nto_array a ^{k=3} b",
                SRArray(&[SRString("a"), SRString("b")]),
            )
        }

        #[test]
        fn command_block_no_pattern() -> Result<(), String> {
            script_fail("to_array = fn ^args -> $args\nto_array a ^{k=3} b")
        }

        #[test]
        fn command_invalid() -> Result<(), String> {
            script_fail("f = fn ^_ -> a\nf ^hello")
        }
    }

    mod pattern_binding {
        use super::*;

        #[test]
        fn any() -> Result<(), String> {
            script_eval_to("_ = a", SRMap(&[]))?;
            script_eval_to("_ = {a=1,b=2}", SRMap(&[]))?;
            script_eval_to("_ = [4,5,6]", SRMap(&[]))?;
            Ok(())
        }

        #[test]
        fn literal() -> Result<(), String> {
            script_eval_to("=hello = hello", SRMap(&[]))?;
            script_eval_to("=[1,2,3] = [1,2,3]", SRMap(&[]))?;
            script_eval_to("={a=b} = {a=b}", SRMap(&[]))?;
            script_eval_to(
                "something = {a=b}; =$something = {a=b}; something=",
                SRMap(&[]),
            )?;
            Ok(())
        }

        #[test]
        fn literal_mismatch() -> Result<(), String> {
            script_fail("=hello = goodbye")?;
            script_fail("=[1,2,3] = [1,2,3,4]")?;
            script_fail("={a=1} = {a=b}")?;
            Ok(())
        }

        #[test]
        fn binding() -> Result<(), String> {
            script_eval_to("a = hello; $a", SRString("hello"))
        }

        #[test]
        fn array() -> Result<(), String> {
            script_eval_to(
                "[a,b,_] = [1,2,3]",
                SRMap(&[("a", SRString("1")), ("b", SRString("2"))]),
            )
        }

        #[test]
        fn array_mismatch() -> Result<(), String> {
            script_fail("[a,b] = [1,2,3]")?;
            script_fail("[a,b,c] = [1,2]")?;
            Ok(())
        }

        #[test]
        fn array_merge() -> Result<(), String> {
            script_eval_to(
                "[a,b,^rest] = [1,2,3,4,5]",
                SRMap(&[
                    ("a", SRString("1")),
                    ("b", SRString("2")),
                    (
                        "rest",
                        SRArray(&[SRString("3"), SRString("4"), SRString("5")]),
                    ),
                ]),
            )?;
            script_eval_to(
                "[a,b,^=[3,4,5]] = [1,2,3,4,5]",
                SRMap(&[("a", SRString("1")), ("b", SRString("2"))]),
            )?;
            Ok(())
        }

        #[test]
        fn array_undecidable() -> Result<(), String> {
            script_fail("[^_,^_] = [1,2,3]")?;
            script_fail("[^_,a,^_] = [1,2,3]")?;
            Ok(())
        }

        #[test]
        fn array_complex() -> Result<(), String> {
            script_eval_to(
                "[a,^rest1,b,c,=anchor,d,^rest2,e] = [1,2,3,anchor,4,5,6,7]",
                SRMap(&[
                    ("a", SRString("1")),
                    ("b", SRString("2")),
                    ("c", SRString("3")),
                    ("d", SRString("4")),
                    ("e", SRString("7")),
                    ("rest1", SRArray(&[])),
                    ("rest2", SRArray(&[SRString("5"), SRString("6")])),
                ]),
            )
        }

        #[test]
        fn map() -> Result<(), String> {
            script_eval_to(
                "{a,b=binding} = {a=1,b=2}",
                SRMap(&[("a", SRString("1")), ("binding", SRString("2"))]),
            )
        }

        #[test]
        fn map_merge() -> Result<(), String> {
            script_eval_to(
                "{a,b=binding,^keys} = {a=1,b=2,c=3,d=4}",
                SRMap(&[
                    ("a", SRString("1")),
                    ("binding", SRString("2")),
                    ("keys", SRMap(&[("c", SRString("3")), ("d", SRString("4"))])),
                ]),
            )?;
            script_eval_to(
                "{a,b=binding,^={c=3,d=4}} = {a=1,b=2,c=3,d=4}",
                SRMap(&[("a", SRString("1")), ("binding", SRString("2"))]),
            )?;
            Ok(())
        }

        #[test]
        fn map_mismatch() -> Result<(), String> {
            script_fail("{a,b,c} = {a=1,b=2}")?;
            script_fail("{a,^keys,^keys2} = {a=1}")?;
            Ok(())
        }
    }

    fn script_eval_to(s: &str, expected: ScriptResult) -> Result<(), String> {
        val_match(script_eval(s)?, expected)
    }

    fn script_result_fail(s: &str) -> Result<(), String> {
        let mut fail = script_eval(s)?;
        assert!(futures::executor::block_on(&mut fail).is_err());
        Ok(())
    }

    fn script_fail(s: &str) -> Result<(), String> {
        match script_eval(s) {
            Ok(v) => Err(format!("expected failure, got {:?}", v)),
            Err(_) => Ok(()),
        }
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
                .typed::<types::Unit>()
                .map(|_| ())
                .expect_ok("expected unit"),
            SRString(s) => v
                .typed::<types::String>()
                .expect_ok("expected string")
                .and_then(|v| v.get().map_err(|e| e.to_string()))
                .and_then(|st| {
                    if st.as_ref().as_str() == s {
                        Ok(())
                    } else {
                        Err(format!(
                            "string mismatch, expected \"{}\", got \"{}\"",
                            s,
                            st.as_ref()
                        ))
                    }
                }),
            SRArray(arr) => v
                .typed::<types::Array>()
                .expect_ok("expected array")
                .and_then(|v| v.get().map_err(|e| e.to_string()))
                .map(|v| v.owned())
                .and_then(|types::Array(varr)| {
                    if varr.len() != arr.len() {
                        Err("array length mismatch".into())
                    } else {
                        varr.into_iter()
                            .zip(arr)
                            .map(|(v, e)| val_match(v, e.clone()))
                            .collect()
                    }
                }),
            SRMap(entries) => v
                .typed::<types::Map>()
                .expect_ok("expected map")
                .and_then(|v| v.get().map_err(|e| e.to_string()))
                .map(|v| v.owned())
                .and_then(|types::Map(mut m)| {
                    if m.len() != entries.len() {
                        Err("map length mismatch".into())
                    } else {
                        entries
                            .iter()
                            .map(|(k, e)| {
                                m.remove(*k)
                                    .map(|v| val_match(v, e.clone()))
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
        ctx.plan(
            Script::load(Source::new(StringSource::new("<string>", s.to_owned())))
                .map_err(|e| e.to_string())?,
        )
        .map(|sv| sv.unwrap())
        .map_err(|e| format!("{:?}", e))
    }
}
