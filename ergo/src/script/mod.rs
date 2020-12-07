//! Script loading and execution.

use crate::constants::*;
use abi_stable::rvec;
pub use ergo_runtime::source::{FileSource, Source, StringSource};
use ergo_runtime::{types, EvalResult, Runtime, ScriptEnv};
use grease::types::GreaseType;

mod ast;
mod base;
mod runtime;

use runtime::*;

/// A loaded script.
pub struct Script {
    ast: ast::Script,
    top_level_env: ScriptEnv,
    file_path: Option<std::path::PathBuf>,
}

/// Create a script context from a context builder.
pub fn script_context(
    cb: grease::runtime::ContextBuilder,
) -> Result<Runtime, grease::runtime::BuilderError> {
    let mut global_env = ScriptEnv::default();
    {
        let mut insert = |k: &str, v| {
            global_env.insert(
                ergo_runtime::types::String::from(k).into(),
                Ok(Source::builtin(v)).into(),
            )
        };

        // Add initial environment functions
        let env = vec![(PROGRAM_NAME, base::load())];
        for (k, v) in env.into_iter() {
            insert(k, v);
        }
    }

    cb.build().map(|mut ctx| {
        // Add initial traits
        ergo_runtime::traits::traits(&mut ctx.traits);
        runtime::traits(&mut ctx.traits);
        Runtime::new(ctx, global_env)
    })
}

/// Get the final value.
///
/// This will apply any functions without arguments.
pub async fn final_value(ctx: &mut Runtime, mut val: Source<grease::Value>) -> EvalResult {
    // Apply any functions (with no arguments).
    while val.grease_type().await? == &types::Function::grease_type() {
        val = runtime::apply_value(ctx, val, Default::default(), false).await?;
    }
    Ok(val)
}

impl Script {
    /// Load a script from a character stream.
    pub fn load(src: Source<()>) -> Result<Self, ast::Error> {
        ast::load(src).map(|ast| Script {
            ast,
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

    pub async fn evaluate(self, ctx: &Runtime) -> EvalResult {
        // Create an environment appropriate based on the configuration.
        let mut ctx = ctx.empty();
        ctx.mod_path = self.file_path.map(|v| v.into()).into();

        let script_dir = grease::path::PathBuf::from(ctx.mod_dir());

        // Set up load paths
        let mut load_paths = rvec![script_dir.clone()];

        // Add neighboring share directories when running in a [prefix]/bin directory.
        let mut neighbor_dir = std::env::current_exe().ok().and_then(|path| {
            path.parent().and_then(|parent| {
                if parent.file_name() == Some("bin".as_ref()) {
                    let path = parent
                        .parent()
                        .expect("must have parent directory")
                        .join("share")
                        .join(crate::constants::PROGRAM_NAME)
                        .join("lib");
                    if path.exists() {
                        Some(path)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
        });

        // If the neighbor directory is somewhere in the home directory, it should be added prior to the local
        // data app dir.
        if let (Some(dir), Some(user_dirs)) = (&neighbor_dir, &directories::UserDirs::new()) {
            if dir.starts_with(user_dirs.home_dir()) {
                load_paths.push(neighbor_dir.take().unwrap().into());
            }
        }

        // Add local data app dir.
        if let Some(proj_dirs) = app_dirs() {
            let path = proj_dirs.data_local_dir().join("lib");
            if path.exists() {
                load_paths.push(path.into());
            }
        }

        // If the neighbor directory wasn't added, it should be added now, after the local data app dir.
        if let Some(dir) = neighbor_dir {
            load_paths.push(dir.into());
        }

        ctx.load_paths = load_paths.clone();

        let mut env = self.top_level_env;
        // Add script-dir/load-path last; they should not be overwritten.
        env.insert(
            types::String::from(SCRIPT_DIR_BINDING).into(),
            Ok(Source::builtin(script_dir.into())).into(),
        );
        env.insert(
            types::String::from(LOAD_PATH_BINDING).into(),
            Ok(Source::builtin(
                types::Array(load_paths.into_iter().map(|v| v.into()).collect()).into(),
            ))
            .into(),
        );

        ctx.env_extend(env);

        Rt(self.ast).evaluate(&mut ctx).await
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
    fn block_set_ref() -> Result<(), String> {
        script_eval_to("a = b\n{:a = 1}", SRMap(&[("b", SRString("1"))]))
    }

    #[test]
    fn block_set_dedup() -> Result<(), String> {
        script_eval_to(
            "a = something\nb = something\n{:a = ();:b = ()}",
            SRMap(&[("something", SRUnit)]),
        )
    }

    #[test]
    fn block_set_arbitrary() -> Result<(), String> {
        script_eval_to(
            "a = b; :a = c; arr = [:b,1]; :arr = d; ::arr = e; map = {k=v}; :map = f",
            SRAny,
        )
    }

    #[test]
    fn comment() -> Result<(), String> {
        script_eval_to(
            "# comment comment comment\na = something\n:a",
            SRString("something"),
        )
    }

    #[test]
    fn index() -> Result<(), String> {
        script_eval_to("[a,b]:0", SRString("a"))?;
        script_fail("[a,b]:2")?;
        script_eval_to("{alpha=one,beta=two}:beta", SRString("two"))?;
        script_fail("{alpha=one,beta=two}:omega")?;
        script_eval_to("{alpha=[a,{key=b}]}:alpha:1:key", SRString("b"))?;
        Ok(())
    }

    #[test]
    fn bindings() -> Result<(), String> {
        script_eval_to("var = something; :var", SRString("something"))?;
        script_eval_to(
            "var = {inner=[blah,{v=\"test()\"}]}; var:inner:1:v",
            SRString("test()"),
        )?;
        Ok(())
    }

    #[test]
    fn forced_bindings() -> Result<(), String> {
        script_eval_to("[a,b] = [a]", SRAny)?;
        script_fail("![a,b] = [a]")?;
        Ok(())
    }

    #[test]
    fn function() -> Result<(), String> {
        script_eval_to("f = fn ^_ -> a\nf something", SRString("a"))?;
        script_eval_to("second = fn _ b ^_ -> :b\nsecond a b", SRString("b"))?;
        script_eval_to(
            "f = fn a _ b ^_ -> {\n  a = :a\n  b = :b\n}\nf 1 2 3",
            SRMap(&[("a", SRString("1")), ("b", SRString("3"))]),
        )?;
        Ok(())
    }

    #[test]
    fn function_capture() -> Result<(), String> {
        script_eval_to("f = { a = 100; fn -> :a }; a = 5; (f)", SRString("100"))?;
        script_fail("f = fn -> :b")?;
        script_eval_to(
            "f = fn a -> { b = [:a,something]; fn -> {b,a} }; ((f hi))",
            SRMap(&[
                ("b", SRArray(&[SRString("hi"), SRString("something")])),
                ("a", SRString("hi")),
            ]),
        )?;
        script_eval_to("a = 5; :a = 10; f = fn -> ::a; (f)", SRString("10"))?;
        script_fail("a = 5; :a = 10; f = fn -> b = 4; ::b")?;
        Ok(())
    }

    #[test]
    fn match_expr() -> Result<(), String> {
        script_eval_to(
            "match [1,2,3] { {^keys} = :keys, [a,b] = :a, [a,b,c] = :a }",
            SRString("1"),
        )?;
        script_eval_to(
            "match [1,2,3] { a = :a, [a,b,c] = :b }",
            SRArray(&[SRString("1"), SRString("2"), SRString("3")]),
        )?;
        Ok(())
    }

    #[test]
    fn match_failure() -> Result<(), String> {
        script_result_fail("match {a=[1,2]} { {b} = :b, [a,b] = :b }")
    }

    #[test]
    fn forced_match() -> Result<(), String> {
        script_eval_to("match {a=[1,2]} { {b} = :b, [a,b] = :b }; ()", SRUnit)?;
        script_fail("!match {a=[1,2]} { {b} = :b, [a,b] = :b }")?;
        Ok(())
    }

    #[test]
    fn if_expr() -> Result<(), String> {
        script_eval_to("t = 1; f = 2; if () :t :f", SRString("2"))?;
        script_eval_to("t = 1; f = 2; if something :t :f", SRString("1"))?;
        script_eval_to("t = 1; f = 2; if () :t", SRUnit)?;
        Ok(())
    }

    mod merge {
        use super::*;

        #[test]
        fn array() -> Result<(), String> {
            script_eval_to(
                "arr = [b,c]\n[a,^:arr,d]",
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
                "{ a = 1, b = 2, ^{ c = 3, d = 4 }, e = :c }",
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
                "a = 1\n^{b = 2}\nc = :b",
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
                "to_array = fn ^args -> :args\nto_array a ^[b,c] d ^[e,f]",
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
                "to_array = fn ^args ^{^_} -> :args\nto_array a ^{k=3} b",
                SRArray(&[SRString("a"), SRString("b")]),
            )
        }

        #[test]
        fn command_block_no_pattern() -> Result<(), String> {
            script_fail("to_array = fn ^args -> :args\nto_array a ^{k=3} b")
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
                "something = {a=b}; =:something = {a=b}; something=",
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
            script_eval_to("a = hello; :a", SRString("hello"))
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
            script_result_fail("[a,b] = [1,2,3]; :a")?;
            script_result_fail("[a,b,c] = [1,2]; :a")?;
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
            script_result_fail("match [1,2,3] { [^_,^_] = doh }")?;
            script_result_fail("[^_,a,^_] = [1,2,3]; :a")?;
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
            script_result_fail("{a,b,c} = {a=1,b=2}; :a")?;
            script_result_fail("{a,^keys,^keys2} = {a=1}; :a")?;
            Ok(())
        }
    }

    fn script_eval_to(s: &str, expected: ScriptResult) -> Result<(), String> {
        block_on(async move { val_match(script_eval(s).await?, expected).await })
    }

    fn script_result_fail(s: &str) -> Result<(), String> {
        let mut fail = block_on(script_eval(s))?;
        assert!(block_on(&mut fail).is_err());
        Ok(())
    }

    fn script_fail(s: &str) -> Result<(), String> {
        match block_on(script_eval(s)) {
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

    fn val_match(v: Value, expected: ScriptResult) -> BoxFuture<'static, Result<(), String>> {
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
                            let v = m
                                .remove(&types::String::from(*k).into())
                                .ok_or("missing expected key")?;
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

    async fn script_eval(s: &str) -> Result<Value, String> {
        let mut ctx = script_context(Default::default()).map_err(|e| e.to_string())?;
        Script::load(Source::new(StringSource::new("<string>", s.to_owned())))
            .map_err(|e| e.to_string())?
            .evaluate(&mut ctx)
            .await
            .map(|sv| sv.unwrap())
            .map_err(|e| format!("{:?}", e))
    }
}
