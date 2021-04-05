//! Ergo script loading and execution.

pub use ergo_runtime::source::{FileSource, Source, StringSource};
use ergo_runtime::{traits, types, EvalResult, Runtime, ScriptEnv};
use futures::FutureExt;
use grease::types::GreaseType;

mod ast;
mod base;
mod runtime;
pub mod testing;

pub mod constants {
    macro_rules! program_name {
        () => {
            "ergo"
        };
    }

    pub const PROGRAM_NAME: &'static str = program_name!();
    pub const EXTENSION: &'static str = program_name!();
    pub const WORKSPACE_NAME: &'static str = concat!("workspace.", program_name!());
    pub const DIR_NAME: &'static str = concat!("dir.", program_name!());
    pub const PLUGIN_ENTRY: &'static str = concat!("_", program_name!(), "_plugin");
}

use runtime::*;

pub use base::{resolve_script_path, LOAD_DOCUMENTATION};

/// Script runtime functions.
pub struct RuntimeControl {
    load_data: base::LoadData,
}

impl RuntimeControl {
    /// Clear the load cache of any loaded scripts.
    pub fn clear_load_cache(&self) {
        self.load_data.load_cache.lock().clear();
    }
}

/// A loaded script.
pub struct Script {
    ast: ast::Expr,
    captures: runtime::Captures,
    top_level_env: ScriptEnv,
    file_path: Option<std::path::PathBuf>,
}

/// Create a script context from a context builder and initial load path.
pub fn script_context(
    cb: grease::runtime::ContextBuilder,
    initial_load_path: Vec<std::path::PathBuf>,
) -> Result<(Runtime, RuntimeControl), grease::runtime::BuilderError> {
    let mut global_env = ScriptEnv::default();
    let load_data = {
        let mut insert = |k: &str, v| {
            global_env.insert(
                ergo_runtime::types::String::from(k).into(),
                (Ok(Source::builtin(v)).into(), None.into()).into(),
            )
        };

        let load_functions = base::load_functions();

        // Add initial environment functions
        insert(constants::PROGRAM_NAME, load_functions.load);
        insert("std", load_functions.std);
        insert("workspace", load_functions.workspace);
        insert("fn", base::pat_args_to_args());
        insert("pat", base::pat_args_to_pat_args());
        insert("index", base::pat_args_to_index());
        insert("doc", base::doc());
        insert("bind", base::bind());
        load_functions.load_data
    };

    let ctrl = RuntimeControl { load_data };

    cb.build().map(move |mut ctx| {
        // Add initial traits
        ergo_runtime::traits::traits(&mut ctx.traits);
        runtime::traits(&mut ctx.traits);
        (Runtime::new(ctx, global_env, initial_load_path), ctrl)
    })
}

/// Get the final value.
///
/// This will apply any unbound values on empty Args.
pub async fn final_value(ctx: &mut Runtime, mut val: Source<grease::Value>) -> EvalResult {
    while val.grease_type().await? == &types::Unbound::grease_type() {
        let src = val.source();
        val = traits::bind(
            ctx,
            val,
            src.with(grease::value::IntoValue::into_value(types::Args {
                args: Default::default(),
            })),
        )
        .await?;
    }
    Ok(val)
}

impl Script {
    /// Load a script from a character stream.
    pub fn load(src: Source<()>) -> Result<Self, grease::Error> {
        ast::load(src).map(|(ast, captures)| Script {
            ast,
            captures: captures.into_iter().map(|(k, v)| (k, v.into())).collect(),
            top_level_env: Default::default(),
            file_path: None,
        })
    }

    /// Configure the top-level environment.
    pub fn top_level_env(&mut self, env: ScriptEnv) {
        self.top_level_env = env;
    }

    /// Configure the file path of the script, if any.
    pub fn file_path(&mut self, path: std::path::PathBuf) {
        self.file_path = Some(path);
    }

    /// Evaluate the script with the given runtime and additional load paths.
    pub async fn evaluate(self, ctx: &Runtime) -> EvalResult {
        // Create an environment based on the configuration.
        let mut ctx = ctx.empty();
        ctx.mod_path = self.file_path.map(|v| v.into()).into();

        ctx.reset_load_path();

        let top_level_env = self.top_level_env;
        ctx.env_set_to_current(move |ctx| {
            async move { ctx.env_extend(top_level_env.into_iter().map(|(k, v)| (k, v.0))) }.boxed()
        })
        .await;

        let evaluator = Evaluator::default();
        let mut captures = self.captures;
        evaluator.fill_unbound(&mut ctx, &mut captures)?;
        evaluator.eval_captures(&mut ctx, &mut captures).await?;
        Ok(evaluator.evaluate(&mut ctx, self.ast, &captures))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ergo_runtime::types;
    use futures::executor::block_on;
    use futures::future::{BoxFuture, FutureExt};
    use grease::value::Value;

    #[derive(Clone, Debug)]
    enum ScriptResult {
        SRUnit,
        SRUnset,
        SRString(&'static str),
        SRArray(&'static [ScriptResult]),
        SRMap(&'static [(&'static str, ScriptResult)]),
        SRAny,
    }

    use ScriptResult::*;

    #[test]
    fn unit() -> Result<(), String> {
        script_eval_to("()", SRUnit)
    }

    #[test]
    fn array() -> Result<(), String> {
        script_eval_to("[a,b]", SRArray(&[SRString("a"), SRString("b")]))
    }

    #[test]
    fn block() -> Result<(), String> {
        script_eval_to(
            "{:alpha=one,:beta=two}",
            SRMap(&[("alpha", SRString("one")), ("beta", SRString("two"))]),
        )
    }

    #[test]
    fn set_with_equals() -> Result<(), String> {
        script_eval_to(":a = \"1=2\"", SRMap(&[("a", SRString("1=2"))]))
    }

    #[test]
    fn block_set_shorthand() -> Result<(), String> {
        script_eval_to(
            ":a = 1\n:b = 2\n{:c=3,a,b}",
            SRMap(&[
                ("a", SRString("1")),
                ("b", SRString("2")),
                ("c", SRString("3")),
            ]),
        )
    }

    #[test]
    fn block_set_ref() -> Result<(), String> {
        script_eval_to(":a = b\n{::a = 1}", SRMap(&[("b", SRString("1"))]))
    }

    #[test]
    fn block_set_dedup() -> Result<(), String> {
        script_eval_to(
            ":a = something\n:b = something\n{::a = ();::b = ()}",
            SRMap(&[("something", SRUnit)]),
        )
    }

    #[test]
    fn block_set_arbitrary() -> Result<(), String> {
        script_eval_to(
            ":a = b; ::a = c; :arr = [::a,1]; ::arr = d; :::arr = e; :map = {:k=v}; ::map = f",
            SRAny,
        )
    }

    mod doc_comment {
        use super::*;

        #[test]
        fn basic() -> Result<(), String> {
            script_eval_to(
                "## doc comment comment comment
                :a = something
                :a",
                SRString("something"),
            )
        }

        #[test]
        fn expression() -> Result<(), String> {
            script_eval_to(
                "## doc {{ comment }}
                a = ()
                doc :a",
                SRString("doc comment"),
            )
        }

        #[test]
        fn expression_scope() -> Result<(), String> {
            script_eval_to(
                "## {{v = comment}}doc {{:v}}
                a = ()
                doc :a",
                SRString("doc comment"),
            )?;
            script_eval_to(
                "v = comment
                ## doc {{:v}}
                a = ()
                doc :a",
                SRString("doc comment"),
            )?;
            script_eval_to(
                "v = hello
                ## {{v = comment}}doc {{:v}}
                ()
                :v",
                SRString("hello"),
            )?;
            Ok(())
        }

        #[test]
        fn expression_self() -> Result<(), String> {
            script_eval_to(
                "## doc {{self:v}}
                a = { v = comment }
                doc :a",
                SRString("doc comment"),
            )
        }
    }

    #[test]
    fn index() -> Result<(), String> {
        script_eval_to("[a,b]:0", SRString("a"))?;
        script_parse_fail("[a,b]:2")?;
        script_eval_to("{:alpha=one,:beta=two}:beta", SRString("two"))?;
        script_eval_to("{:alpha=one,:beta=two}:omega", SRUnset)?;
        script_eval_to("{:alpha=[a,{:key=b}]}:alpha:1:key", SRString("b"))?;
        Ok(())
    }

    #[test]
    fn array_negative_index() -> Result<(), String> {
        script_eval_to("[a,b]:-1", SRString("b"))?;
        script_parse_fail("[a,b]:-3")?;
        Ok(())
    }

    #[test]
    fn bindings() -> Result<(), String> {
        script_eval_to(":var = something; :var", SRString("something"))?;
        script_eval_to(
            ":var = {:inner=[blah,{:v=\"test()\"}]}; var:inner:1:v",
            SRString("test()"),
        )?;
        Ok(())
    }

    #[test]
    fn function() -> Result<(), String> {
        script_eval_to(":f = fn ^_ -> a\nf something", SRString("a"))?;
        script_eval_to(":f = fn :b -> :b\nf b", SRString("b"))?;
        script_eval_to(":second = fn _ :b ^_ -> :b\nsecond a b", SRString("b"))?;
        script_eval_to(
            ":f = fn :a _ :b ^_ -> {\n  :a = :a\n  :b = :b\n}\nf 1 2 3",
            SRMap(&[("a", SRString("1")), ("b", SRString("3"))]),
        )?;
        Ok(())
    }

    #[test]
    fn function_no_args() -> Result<(), String> {
        script_eval_to("f = fn: -> a; f:", SRString("a"))
    }

    #[test]
    fn function_capture() -> Result<(), String> {
        script_eval_to(":f = { :a = 100; _ -> :a }; :a = 5; f:", SRString("100"))?;
        script_parse_fail(":f = _ -> :b; f:")?;
        script_eval_to(
            ":f = fn :a -> { :b = [:a,something]; fn: -> {b,a} }; (f hi):",
            SRMap(&[
                ("b", SRArray(&[SRString("hi"), SRString("something")])),
                ("a", SRString("hi")),
            ]),
        )?;
        script_fail(":a = 5; ::a = 10; :f = _ -> ::a; f:")?;
        script_parse_fail(":a = 5; ::a = 10; :f = _ -> :b = 4; f:; :b")?;
        Ok(())
    }

    mod merge {
        use super::*;

        #[test]
        fn array() -> Result<(), String> {
            script_eval_to(
                ":arr = [b,c]\n[a,^:arr,d]",
                SRArray(&[SRString("a"), SRString("b"), SRString("c"), SRString("d")]),
            )
        }

        #[test]
        fn array_invalid() -> Result<(), String> {
            script_fail("[a,^b,c]")?;
            Ok(())
        }

        #[test]
        fn block() -> Result<(), String> {
            script_eval_to(
                "{ :a = 1, :b = 2, ^{ :c = 3, :d = 4 }, :e = 5 }",
                SRMap(&[
                    ("a", SRString("1")),
                    ("b", SRString("2")),
                    ("c", SRString("3")),
                    ("d", SRString("4")),
                    ("e", SRString("5")),
                ]),
            )
        }

        #[test]
        fn block_script_scope() -> Result<(), String> {
            script_parse_fail(":a = 1\n^{:b = 2}\n:c = :b")
        }

        #[test]
        fn block_with_array() -> Result<(), String> {
            script_eval_to("{^[1,2]}", SRString("2"))
        }

        #[test]
        fn command_array() -> Result<(), String> {
            script_eval_to(
                ":to_array = fn ^:args -> :args\nto_array a ^[b,c] d ^[e,f]",
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
        fn command_non_positional() -> Result<(), String> {
            script_eval_to(
                ":to_array = fn ^:args ^{^_} -> :args\nto_array a ^{:k=3} b",
                SRArray(&[SRString("a"), SRString("b")]),
            )?;
            script_eval_to(
                ":kw = fn ^_ ^{^:args} -> :args\nkw a ^{k=3} (d=4) b",
                SRMap(&[("k", SRString("3")), ("d", SRString("4"))]),
            )?;
            script_eval_to(
                ":kw = fn ^{ something } -> :something\nkw (something = hi)",
                SRString("hi"),
            )?;
            Ok(())
        }

        #[test]
        fn command_block_no_pattern() -> Result<(), String> {
            script_fail(":to_array = fn ^:args -> :args\nto_array a ^{:k=3} b")
        }

        #[test]
        fn command_invalid() -> Result<(), String> {
            script_fail(":f = fn ^_ -> a\nf ^hello")
        }
    }

    mod binding {
        use super::*;

        #[test]
        fn any() -> Result<(), String> {
            script_eval_to("_ = a", SRMap(&[]))?;
            script_eval_to("_ = {:a=1,:b=2}", SRMap(&[]))?;
            script_eval_to("_ = [4,5,6]", SRMap(&[]))?;
            Ok(())
        }

        #[test]
        fn binding() -> Result<(), String> {
            script_eval_to(":a = hello; :a", SRString("hello"))
        }

        #[test]
        fn literal() -> Result<(), String> {
            script_eval_to("() = ()", SRMap(&[]))?;
            script_eval_to("!hello = hello", SRMap(&[]))?;
            script_eval_to("[1,2,3] = [1,2,3]", SRMap(&[]))?;
            script_eval_to("{:a=b} = {:a=b}", SRMap(&[]))?;
            script_eval_to(":something = {:a=b}; !:something = {:a=b}; ()", SRUnit)?;
            Ok(())
        }

        #[test]
        fn literal_mismatch() -> Result<(), String> {
            script_fail("fn hello -> () |> goodbye")?;
            script_fail("fn [1,2,3] -> () |> [1,2,3,4]")?;
            script_fail("fn {:a=1} -> () |> {:a=b}")?;
            Ok(())
        }

        #[test]
        fn array() -> Result<(), String> {
            script_eval_to(
                "[:a,:b,_] = [1,2,3]",
                SRMap(&[("a", SRString("1")), ("b", SRString("2"))]),
            )
        }

        #[test]
        fn array_mismatch() -> Result<(), String> {
            script_fail("[:a,:b] = [1,2,3]; :a")?;
            script_fail("[:a,:b,:c] = [1,2]; :a")?;
            Ok(())
        }

        #[test]
        fn array_merge() -> Result<(), String> {
            script_eval_to(
                "[:a,:b,^:rest] = [1,2,3,4,5]",
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
                "[:a,:b,^[3,4,5]] = [1,2,3,4,5]",
                SRMap(&[("a", SRString("1")), ("b", SRString("2"))]),
            )?;
            Ok(())
        }

        #[test]
        fn array_undecidable() -> Result<(), String> {
            script_fail("[:x,^_,^_] = [1,2,3], :x")?;
            Ok(())
        }

        #[test]
        fn array_complex() -> Result<(), String> {
            script_eval_to(
                "[:a,^:rest1,:b,:c,anchor,^:rest2] = [1,2,3,anchor,4,5,6,7]\n[:d,^:rest2,:e] = :rest2",
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
                "{a,:b=:binding} = {:a=1,:b=2}",
                SRMap(&[("a", SRString("1")), ("binding", SRString("2"))]),
            )
        }

        #[test]
        fn map_merge() -> Result<(), String> {
            script_eval_to(
                "{a,:b=:binding,^:keys} = {:a=1,:b=2,:c=3,:d=4}",
                SRMap(&[
                    ("a", SRString("1")),
                    ("binding", SRString("2")),
                    ("keys", SRMap(&[("c", SRString("3")), ("d", SRString("4"))])),
                ]),
            )?;
            script_eval_to(
                "{a,:b=:binding,^{:c=3,:d=4}} = {:a=1,:b=2,:c=3,:d=4}",
                SRMap(&[("a", SRString("1")), ("binding", SRString("2"))]),
            )?;
            Ok(())
        }

        #[test]
        fn map_mismatch() -> Result<(), String> {
            script_fail("fn {a,b} -> () |> {a=1,b=2,c=3}")?;
            script_fail("fn {:a,^:keys,^:keys2} -> () |> {:a=1}")?;
            Ok(())
        }

        #[test]
        fn bind_command() -> Result<(), String> {
            script_eval_to(
                ":keyto = pat :key -> :v -> { !bind :key v:key }; :m = {:key = hi}; fn (keyto hi) -> () |> :m",
                SRUnit,
            )?;
            script_eval_to(
                ":keyto = pat :key -> :v -> { !bind :key v:key }; :m = {:key = hi}; keyto :b = :m; :b",
                SRString("hi"),
            )?;
            script_fail(
                ":keyto = pat :key -> :v -> { !bind :key v:key }; :m = {:key = hi}; fn (keyto bye) -> () |> :m",
            )?;
            script_fail(
                ":keyto = pat :key -> :v -> { !bind :key v:key }; :m = {:notkey = hi}; fn (keyto hi) -> () |> :m",
            )?;
            Ok(())
        }
    }

    fn script_eval_to(s: &str, expected: ScriptResult) -> Result<(), String> {
        let mut ctx = make_context()?;
        let task = ctx.task.clone();
        let res =
            block_on(async move { val_match(script_eval(&mut ctx, s).await?, expected).await });
        task.shutdown();
        res
    }

    fn script_parse_fail(s: &str) -> Result<(), String> {
        let mut ctx = make_context()?;
        let failed = block_on(script_eval(&mut ctx, s));
        assert!(failed.is_err());
        ctx.task.shutdown();
        Ok(())
    }

    fn script_fail(s: &str) -> Result<(), String> {
        let mut ctx = make_context()?;
        let should_fail = block_on(script_eval(&mut ctx, s))?;
        assert!(block_on(should_fail.into_non_dyn()).is_err());
        ctx.task.shutdown();
        Ok(())
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

    fn val_match(v: Value, expected: ScriptResult) -> BoxFuture<'static, Result<(), String>> {
        dbg!(&v);
        async move {
            match expected {
                SRUnit => {
                    v.typed::<types::Unit, _, _>(|_| async { panic!("expected unit") })
                        .await
                        .map_err(|e| e.to_string())?
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(())
                }
                SRUnset => {
                    v.typed_immediate::<types::Unset, _, _>(|_| async { panic!("expected unset") })
                        .await
                        .map_err(|e| e.to_string())?;
                    Ok(())
                }
                SRString(s) => {
                    let string = v
                        .typed::<types::String, _, _>(|_| async { panic!("expected string") })
                        .await
                        .map_err(|e| e.to_string())?;
                    let string = string.await.map_err(|e| e.to_string())?;
                    let got = string.as_ref().as_str();
                    if got == s {
                        Ok(())
                    } else {
                        Err(format!(
                            "string mismatch, expected \"{}\", got \"{}\"",
                            s, got
                        ))
                    }
                }
                SRArray(expected_arr) => {
                    let arr = v
                        .typed::<types::Array, _, _>(|_| async { panic!("expected array") })
                        .await
                        .map_err(|e| e.to_string())?;
                    let arr = arr.await.map_err(|e| e.to_string())?;
                    let types::Array(varr) = arr.owned();
                    if varr.len() != expected_arr.len() {
                        Err("array length mismatch".into())
                    } else {
                        for (v, expected_v) in varr.iter().zip(expected_arr) {
                            val_match(v.clone(), expected_v.clone()).await?;
                        }
                        Ok(())
                    }
                }
                SRMap(entries) => {
                    let map = v
                        .typed::<types::Map, _, _>(|_| async { panic!("expected map") })
                        .await
                        .map_err(|e| e.to_string())?;
                    let map = map.await.map_err(|e| e.to_string())?;
                    let types::Map(mut m) = map.owned();
                    if m.len() != entries.len() {
                        Err("map length mismatch".into())
                    } else {
                        for (k, e) in entries.iter() {
                            let k: Value = types::String::from(*k).into();
                            let v = m.remove(&k).ok_or("missing expected key")?;
                            val_match(v, e.clone()).await?;
                        }
                        Ok(())
                    }
                }
                SRAny => Ok(()),
            }
        }
        .boxed()
    }

    fn make_context() -> Result<Runtime, String> {
        let (ctx, _) = script_context(Default::default(), vec![]).map_err(|e| e.to_string())?;
        Ok(ctx)
    }

    async fn script_eval(ctx: &mut Runtime, s: &str) -> Result<Value, String> {
        Script::load(Source::new(StringSource::new("<test>", s.to_owned())))
            .map_err(|e| e.to_string())?
            .evaluate(ctx)
            .await
            .map(|sv| sv.unwrap())
            .map_err(|e| format!("{:?}", e))
    }
}
