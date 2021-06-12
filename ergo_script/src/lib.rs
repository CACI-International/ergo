//! Ergo script loading and execution..map_err(|e| e.to_string())

pub use ergo_runtime::source::{FileSource, Source, StringSource};
use ergo_runtime::{Context, Error, Value};
use std::collections::BTreeMap;

mod ast;
mod base;
mod eval;
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

use eval::*;

/// Script runtime functions.
pub struct Runtime {
    load_data: base::LoadData,
    pub ctx: Context,
}

impl Runtime {
    pub fn new(
        cb: ergo_runtime::context::ContextBuilder,
        load_path: Vec<std::path::PathBuf>,
    ) -> Result<Self, ergo_runtime::context::BuilderError> {
        let load_functions = base::LoadFunctions::new(load_path);
        let env = vec![
            (constants::PROGRAM_NAME, load_functions.load),
            ("std", load_functions.std),
            ("workspace", load_functions.workspace),
            ("fn", base::pat_args_to_args()),
            ("pat", base::pat_args_to_pat_args()),
            ("index", base::pat_args_to_index()),
            ("doc", base::doc()),
            ("bind", base::bind()),
        ]
        .into_iter()
        .map(|(k, v)| (k.into(), Source::builtin(v)))
        .collect();
        let load_data = load_functions.load_data;
        load_data.set_top_level_env(env);

        let ctx = cb.build()?;
        // Add initial traits
        ergo_runtime::ergo_traits(&ctx.traits);
        Ok(Runtime { load_data, ctx })
    }

    /// Load a script from a Source.
    pub fn load(&self, src: Source<()>) -> Result<Script, Error> {
        let mut s = {
            let mut guard = self.load_data.ast_context.lock();
            Script::load(src, &mut *guard)?
        };
        s.top_level_env(self.load_data.top_level_env.lock().clone());
        Ok(s)
    }

    /// Load and evaluate a script.
    pub async fn evaluate(&self, src: Source<()>) -> Result<Source<Value>, Error> {
        let script = self.load(src)?;
        script.evaluate(&self.ctx).await
    }

    /// Resolve a path to the full script path, based on the load path.
    ///
    /// If resolution succeeds, the returned path will be to a file (not directory).
    pub fn resolve_script_path(
        &self,
        working_dir: Option<&std::path::Path>,
        path: &std::path::Path,
    ) -> Option<std::path::PathBuf> {
        self.load_data.resolve_script_path(working_dir, path)
    }

    /// Get the final value.
    ///
    /// This will apply any Unbound values on empty Args.
    pub async fn final_value(&self, mut val: Source<Value>) -> Source<Value> {
        loop {
            drop(self.ctx.eval(&mut val).await);
            if val.is_type::<ergo_runtime::types::Unbound>() {
                let src = val.source();
                val = ergo_runtime::traits::bind(
                    &self.ctx,
                    val,
                    src.with(
                        ergo_runtime::types::Args {
                            args: Default::default(),
                        }
                        .into(),
                    ),
                )
                .await;
            } else {
                break;
            }
        }
        val
    }

    /// Clear the load cache of any loaded scripts.
    pub fn clear_load_cache(&self) {
        self.load_data.load_cache.lock().clear();
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        self.load_data.reset();
        self.ctx.task.shutdown();
    }
}

/// A loaded script.
pub struct Script {
    ast: ast::Expr,
    captures: eval::Captures,
    top_level_env: BTreeMap<String, Source<Value>>,
}

/*
/// Get the final value.
///
/// This will apply any unbound values on empty Args.
pub async fn final_value(ctx: &Context, mut val: Source<Value>) -> Value {
}
*/

impl Script {
    /// Load a script from a Source.
    pub(crate) fn load(src: Source<()>, ctx: &mut ast::Context) -> Result<Self, Error> {
        ast::load(src, ctx).map(|(ast, captures)| Script {
            ast,
            captures: captures.into_iter().collect(),
            top_level_env: Default::default(),
        })
    }

    /// Set the top-level environment.
    pub fn top_level_env(&mut self, env: BTreeMap<String, Source<Value>>) {
        self.top_level_env = env;
    }

    /// Extend the top-level environment.
    pub fn extend_top_level_env(&mut self, env: BTreeMap<String, Source<Value>>) {
        self.top_level_env.extend(env);
    }

    /// Evaluate the script with the given runtime and additional load paths.
    pub async fn evaluate(self, ctx: &Context) -> Result<Source<Value>, Error> {
        let top_level_env = self.top_level_env;

        let evaluator = Evaluator::default();
        let mut captures = self.captures;
        captures.resolve_string_gets(top_level_env)?;
        captures.evaluate_ready(evaluator, ctx).await;
        Ok(evaluator.evaluate(ctx, self.ast, &captures))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ergo_runtime::{types, Value};
    use futures::executor::block_on;
    use futures::future::{BoxFuture, FutureExt};

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
        script_fail("[a,b]:2")?;
        script_eval_to("{:alpha=one,:beta=two}:beta", SRString("two"))?;
        script_eval_to("{:alpha=one,:beta=two}:omega", SRUnset)?;
        script_eval_to("{:alpha=[a,{:key=b}]}:alpha:1:key", SRString("b"))?;
        Ok(())
    }

    #[test]
    fn array_negative_index() -> Result<(), String> {
        script_eval_to("[a,b]:-1", SRString("b"))?;
        script_fail("[a,b]:-3")?;
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

    #[test]
    fn forced_function_capture() -> Result<(), String> {
        script_eval_to("b = {a = 1}; f = !(fn :x -> b::x); f a", SRString("1"))
    }

    #[test]
    fn forced_expr_capture() -> Result<(), String> {
        script_eval_to(
            "b = {a = 1}; q = fn :p -> :p; f = !q (fn :x -> b::x); f a",
            SRString("1"),
        )
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
        fn block_with_string() -> Result<(), String> {
            script_eval_to("{^something}", SRMap(&[("something", SRUnit)]))
        }

        #[test]
        fn command_array() -> Result<(), String> {
            script_eval_to(
                ":to_array = fn ^:args -> args:positional\nto_array a ^[b,c] d ^[e,f]",
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
        fn command_keyed() -> Result<(), String> {
            script_eval_to(
                ":to_array = fn ^:args -> args:positional\nto_array a ^{:k=3} b",
                SRArray(&[SRString("a"), SRString("b")]),
            )?;
            script_eval_to(
                ":kw = fn ^:args -> args:keyed\nkw a ^{k=3} (d=4) b ^e",
                SRMap(&[("k", SRString("3")), ("d", SRString("4")), ("e", SRUnit)]),
            )?;
            script_eval_to(
                ":kw = fn ^{something} -> :something\nkw (something = hi)",
                SRString("hi"),
            )?;
            Ok(())
        }

        #[test]
        fn command_args() -> Result<(), String> {
            script_eval_to(
                "to_array = fn ^:args -> args:positional\np = fn _ ^:rest -> to_array ^:rest\np 1 2 3",
                SRArray(&[SRString("2"), SRString("3")])
            )
        }
    }

    mod binding {
        use super::*;

        #[test]
        fn any() -> Result<(), String> {
            script_eval_to("fn _ -> () |> a", SRUnit)?;
            script_eval_to("fn _ -> () |> {:a=1,:b=2}", SRUnit)?;
            script_eval_to("fn _ -> () |> [4,5,6]", SRUnit)?;
            Ok(())
        }

        #[test]
        fn basic() -> Result<(), String> {
            script_eval_to(":a = hello; :a", SRString("hello"))
        }

        #[test]
        fn literal() -> Result<(), String> {
            script_eval_to("fn () -> a |> ()", SRString("a"))?;
            script_eval_to("fn hello -> () |> hello", SRUnit)?;
            script_eval_to("fn [1,2,3] -> () |> [1,2,3]", SRUnit)?;
            script_eval_to("fn {:a=b} -> () |> {:a=b}", SRUnit)?;
            script_eval_to(
                ":something = {:a=b}; fn !:something -> () |> {:a=b}",
                SRUnit,
            )?;
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
            script_fail("fn [:a,:b] -> () |> [1,2,3]")?;
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

        #[test]
        fn nested_binds() -> Result<(), String> {
            script_eval_to(
                "p = pat :a -> :b -> bind :a :b; p [:k] = [1]; :k",
                SRString("1"),
            )?;
            script_eval_to(
                "p = pat :a -> :b -> bind :a :b; p {k} = {k=1}; :k",
                SRString("1"),
            )?;
            Ok(())
        }
    }

    fn script_eval_to(s: &str, expected: ScriptResult) -> Result<(), String> {
        let runtime = make_runtime()?;
        let res = block_on(async move {
            val_match(&runtime.ctx, script_eval(&runtime, s).await?, expected).await
        });
        res
    }

    fn script_parse_fail(s: &str) -> Result<(), String> {
        let runtime = make_runtime()?;
        let failed = block_on(script_eval(&runtime, s));
        assert!(failed.is_err());
        Ok(())
    }

    fn script_fail(s: &str) -> Result<(), String> {
        let runtime = make_runtime()?;
        let mut should_fail = block_on(script_eval(&runtime, s))?;
        assert!(block_on(async move {
            runtime.ctx.eval(&mut should_fail).await.is_err()
        }));
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

    fn val_match<'a>(
        ctx: &'a Context,
        v: Value,
        expected: ScriptResult,
    ) -> BoxFuture<'a, Result<(), String>> {
        dbg!(&v);
        let v = Source::builtin(v);
        async move {
            match expected {
                SRUnit => match ctx.eval_as::<types::Unit>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(_) => Ok(()),
                },
                SRUnset => match ctx.eval_as::<types::Unset>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(_) => Ok(()),
                },
                SRString(s) => match ctx.eval_as::<types::String>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(v) => {
                        let v = v.unwrap();
                        let got = v.as_ref().as_str();
                        if got == s {
                            Ok(())
                        } else {
                            Err(format!(
                                "string mismatch, expected \"{}\", got \"{}\"",
                                s, got
                            ))
                        }
                    }
                },
                SRArray(expected_arr) => match ctx.eval_as::<types::Array>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(arr) => {
                        let arr = arr.unwrap().to_owned().0;
                        if arr.len() != expected_arr.len() {
                            Err("array length mismatch".into())
                        } else {
                            for (v, expected_v) in arr.iter().zip(expected_arr) {
                                val_match(ctx, v.clone().unwrap(), expected_v.clone()).await?;
                            }
                            Ok(())
                        }
                    }
                },
                SRMap(entries) => match ctx.eval_as::<types::Map>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(map) => {
                        let mut map = map.unwrap().to_owned().0;
                        dbg!(&map);
                        if map.len() != entries.len() {
                            Err("map length mismatch".into())
                        } else {
                            for (k, e) in entries.iter() {
                                let k: Value = types::String::from(*k).into();
                                let v = map.remove(&k).ok_or("missing expected key")?;
                                val_match(ctx, v.unwrap(), e.clone()).await?;
                            }
                            Ok(())
                        }
                    }
                },
                SRAny => Ok(()),
            }
        }
        .boxed()
    }

    fn make_runtime() -> Result<Runtime, String> {
        Runtime::new(Default::default(), vec![]).map_err(|e| e.to_string())
    }

    async fn script_eval(runtime: &Runtime, s: &str) -> Result<Value, String> {
        runtime
            .evaluate(Source::new(StringSource::new("<test>", s.to_owned())))
            .await
            .map(|sv| sv.unwrap())
            .map_err(|e| format!("{:?}", e))
    }
}
