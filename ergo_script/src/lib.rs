//! Ergo script loading and execution.

pub use ergo_runtime::source::Source;
use ergo_runtime::{Context, Error, Value};
use std::collections::HashMap;

pub mod ast;
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

pub use ast::LintLevel;

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
            ("fn", base::fn_()),
            ("index", base::index()),
            ("doc", base::doc()),
            ("bind", base::bind()),
            ("unset", base::unset()),
            ("force", base::force()),
        ]
        .into_iter()
        .map(|(k, v)| (k.into(), v))
        .collect();
        let load_data = load_functions.load_data;
        load_data.set_top_level_env(env);

        let ctx = cb.build()?;
        // Add initial traits
        ergo_runtime::ergo_traits(&ctx.traits);
        Ok(Runtime { load_data, ctx })
    }

    /// Set the lint level when loading scripts.
    pub fn lint_level(&self, level: LintLevel) {
        self.load_data.set_lint_level(level);
    }

    /// Set whether backtraces are enabled when loading scripts.
    pub fn backtrace(&self, backtrace: bool) {
        self.load_data.set_backtrace(backtrace);
    }

    /// Load a script from a string.
    pub fn load_string(&self, name: &str, script: &str) -> Result<Script, Error> {
        let source_id = self
            .ctx
            .block_on(async { Context::global().diagnostic_sources() })
            .add_string(name.to_owned(), script.to_owned());
        self.load(Source::new(source_id))
    }

    /// Load a script from a Source.
    pub fn load(&self, src: Source<()>) -> Result<Script, Error> {
        let sources = self
            .ctx
            .block_on(async { Context::global().diagnostic_sources() });
        let content = sources.content(src.source_id).ok_or_else(|| ergo_runtime::error! {
            error: format!("failed to read content for source '{}'", sources.name(src.source_id).unwrap_or("unknown".into()))
        })?;
        let mut s = {
            let mut guard = self.load_data.ast_context.lock();
            Script::load(src.with(content), &mut *guard, self.load_data.lint_level())?
        };
        let source_path = sources.path(src.source_id);
        s.top_level_env(
            self.load_data
                .script_top_level_env(source_path.as_ref().map(|p| p.as_path())),
        );
        if self.load_data.backtrace() {
            s.enable_backtrace();
        }
        Ok(s)
    }

    /// Load and evaluate a script from a string.
    pub fn evaluate_string(&self, name: &str, script: &str) -> Result<Value, Error> {
        let script = self.load_string(name, script)?;
        self.block_on(script.evaluate())
    }

    /// Load and evaluate a script.
    pub fn evaluate(&self, src: Source<()>) -> Result<Value, Error> {
        let script = self.load(src)?;
        self.block_on(script.evaluate())
    }

    /// Block on the given future, setting the runtime task-local Context.
    pub fn block_on<Fut: std::future::Future + Send>(&self, fut: Fut) -> Fut::Output {
        self.ctx.block_on(fut)
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

    /// Apply any Unbound values on `()` Args.
    pub async fn apply_unbound(mut val: Value) -> Value {
        let src = ergo_runtime::metadata::Source::get(&val);
        let set_source = |v| ergo_runtime::metadata::Source::imbue(src.clone().with(v));
        drop(Context::eval(&mut val).await);
        use ergo_runtime::types;
        if val.is_type::<types::Unbound>() {
            val = ergo_runtime::traits::bind(
                val,
                set_source(
                    types::Args {
                        args: types::args::Arguments::positional(vec![set_source(
                            types::Unit.into(),
                        )])
                        .unchecked(),
                    }
                    .into(),
                ),
            )
            .await;
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
#[derive(Clone, Debug)]
pub struct Script {
    ast: ast::Expr,
    captures: eval::Captures,
    top_level_env: HashMap<String, Value>,
    lint_messages: Vec<Source<String>>,
    backtrace: bool,
}

impl Script {
    /// Load a script from a Source.
    pub(crate) fn load(
        src: Source<&str>,
        ctx: &mut ast::Context,
        lint_level: LintLevel,
    ) -> Result<Self, Error> {
        ast::load(src, ctx, lint_level).map(|(ast, captures, lint_messages)| Script {
            ast,
            captures: captures.into_iter().collect(),
            top_level_env: Default::default(),
            lint_messages,
            backtrace: false,
        })
    }

    /// Set the top-level environment.
    pub fn top_level_env(&mut self, env: HashMap<String, Value>) {
        self.top_level_env = env;
    }

    /// Extend the top-level environment.
    pub fn extend_top_level_env(&mut self, env: HashMap<String, Value>) {
        self.top_level_env.extend(env);
    }

    /// Enable backtrace on errors while evaluating values from this script.
    pub fn enable_backtrace(&mut self) {
        self.backtrace = true;
    }

    /// Evaluate the script.
    ///
    /// This must be called with the Context set.
    pub async fn evaluate(self) -> Result<Value, Error> {
        let Script {
            top_level_env,
            lint_messages,
            mut captures,
            ast,
            backtrace,
        } = self;

        if !lint_messages.is_empty() {
            let lint_log = Context::global().log.sublog("lint");
            for m in lint_messages {
                use ergo_runtime::error::{
                    diagnostics_to_string, Diagnostic, DiagnosticInfo, Severity,
                };
                let (src, m) = m.take();
                let diag = Diagnostic::from(m)
                    .set_severity(Severity::Warning)
                    .add_primary_label(src.with(""));
                lint_log.warn(diagnostics_to_string(
                    &[diag],
                    Context::global().diagnostic_sources().as_ref(),
                    false,
                ));
            }
        }

        let evaluator = Evaluator { backtrace };
        captures.resolve_string_gets(&top_level_env)?;
        Ok(evaluator.evaluate(ast, captures).await)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ergo_runtime::{types, Value};
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
    fn unset() -> Result<(), String> {
        script_eval_to("$unset", SRUnset)
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
    fn block_unset() -> Result<(), String> {
        script_eval_to(
            "{b = $unset,:beta=two}",
            SRMap(&[("beta", SRString("two"))]),
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
    fn block_return_string() -> Result<(), String> {
        script_eval_to(":a = 1\n:b = 2\n{:c=3,a,\"b\"}", SRString("b"))
    }

    #[test]
    fn block_set_ref() -> Result<(), String> {
        script_eval_to(":a = b\n{:$a = 1}", SRMap(&[("b", SRString("1"))]))
    }

    #[test]
    fn block_set_dedup() -> Result<(), String> {
        script_eval_to(
            ":a = something\n:b = something\n{:$a = ();:$b = ()}",
            SRMap(&[("something", SRUnit)]),
        )
    }

    #[test]
    fn block_set_arbitrary() -> Result<(), String> {
        script_eval_to(
            ":a = b; v = { :$a = c }; v = { :arr = [v:$a,1] }; :(v:arr) = d; :map = {:k=v}; :$map = f",
            SRAny,
        )
    }

    #[test]
    fn block_indirect_get() -> Result<(), String> {
        script_parse_fail(":a = name, :$a = b, $$a")
    }

    mod doc_comment {
        use super::*;

        #[test]
        fn basic() -> Result<(), String> {
            script_eval_to(
                "## doc comment comment comment
                :a = something
                $a",
                SRString("something"),
            )
        }

        #[test]
        fn expression() -> Result<(), String> {
            script_eval_to(
                "## doc $\"comment\"
                a = ()
                doc $a",
                SRString("doc comment"),
            )
        }

        #[test]
        fn expression_scope() -> Result<(), String> {
            script_eval_to(
                "v = comment
                ## doc $v
                a = ()
                doc $a",
                SRString("doc comment"),
            )
        }

        #[test]
        fn expression_value() -> Result<(), String> {
            script_eval_to(
                "## doc $(doc:value () |>:v)
                a = { v = comment }
                doc $a",
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
        script_eval_to(":var = something; $var", SRString("something"))?;
        script_eval_to(
            ":var = {:inner=[blah,{:v=\"test()\"}]}; var:inner:1:v",
            SRString("test()"),
        )?;
        Ok(())
    }

    #[test]
    fn bindings_unset() -> Result<(), String> {
        script_eval_to(
            "do-nothing = fn :x -> :v -> (); f = fn (do-nothing :x) -> $x; f hi",
            SRUnset,
        )?;
        script_eval_to(
            "do-nothing = fn :x -> :v -> (); f = fn (do-nothing (do-nothing :x)) -> $x; f hi",
            SRUnset,
        )?;
        script_eval_to(
            "do-nothing = fn :x -> :v -> (); f = fn (do-nothing [do-nothing :x]) -> $x; f hi",
            SRUnset,
        )?;
        script_eval_to(
            "do-nothing = fn :x -> :v -> (); f = fn (do-nothing {k = do-nothing :x}) -> $x; f hi",
            SRUnset,
        )?;
        Ok(())
    }

    #[test]
    fn function() -> Result<(), String> {
        script_eval_to(":f = fn ^_ -> a\nf something", SRString("a"))?;
        script_eval_to(":f = fn :b -> $b\nf b", SRString("b"))?;
        script_eval_to(":second = fn _ :b ^_ -> $b\nsecond a b", SRString("b"))?;
        script_eval_to(
            ":f = fn :a _ :b ^_ -> {\n  :a = $a\n  :b = $b\n}\nf 1 2 3",
            SRMap(&[("a", SRString("1")), ("b", SRString("3"))]),
        )?;
        Ok(())
    }

    #[test]
    fn function_no_args() -> Result<(), String> {
        script_eval_to("f = fn ^[] -> a; f ^[]", SRString("a"))?;
        script_parse_fail("f = fn: -> a")?;
        Ok(())
    }

    #[test]
    fn function_capture() -> Result<(), String> {
        script_eval_to(":f = { :a = 100; _ -> $a }; :a = 5; f()", SRString("100"))?;
        script_parse_fail(":f = _ -> $b; f()")?;
        script_eval_to(
            ":f = fn :a -> { :b = [$a,something]; fn() -> {b,a} }; (f hi) ()",
            SRMap(&[
                ("b", SRArray(&[SRString("hi"), SRString("something")])),
                ("a", SRString("hi")),
            ]),
        )?;
        script_parse_fail(":a = 5; :$a = 10; :f = _ -> :b = 4; f(); $b")?;
        Ok(())
    }

    #[test]
    fn function_unset_keyed_argument() -> Result<(), String> {
        script_eval_to(
            "f = fn ^:args -> args:keyed; f (a = value) (blah = $unset)",
            SRMap(&[("a", SRString("value"))]),
        )
    }

    #[test]
    fn set_and_unset() -> Result<(), String> {
        script_eval_to("{a = 1, a = $unset}", SRMap(&[]))
    }

    #[test]
    fn merge_and_unset() -> Result<(), String> {
        script_eval_to("m = {a = 1}, {^$m, a = $unset}", SRMap(&[]))
    }

    #[test]
    fn quote_compound_string() -> Result<(), String> {
        script_eval_to("x = b, \"a $x\"", SRString("a b"))?;
        script_eval_to("x = b, y = c, \"a$x$y\"", SRString("abc"))?;
        Ok(())
    }

    #[test]
    fn compound_string_ids_differ() {
        script_parse_id_ne("x = b, \"a $x\"", "x = b, \"c $x\"");
    }

    #[test]
    fn quote_preserves_whitespace() -> Result<(), String> {
        script_eval_to("hi=hi, \"\n\n$hi\n\"", SRString("\n\nhi\n"))
    }

    #[test]
    fn basic_string_equality() {
        script_parse_id_eq("string", "\"string\"");
        script_parse_id_eq("string", "' string");
    }

    #[test]
    fn block_string() -> Result<(), String> {
        script_eval_to(
            "v = block, '\n  ' this is my $v string",
            SRString("this is my block string"),
        )
    }

    #[test]
    fn attributes() -> Result<(), String> {
        script_eval_to(
            "replace = fn :val -> _ -> $val; ##(replace newvalue) value",
            SRString("newvalue"),
        )
    }

    mod merge {
        use super::*;

        #[test]
        fn array() -> Result<(), String> {
            script_eval_to(
                ":arr = [b,c]\n[a,^$arr,d]",
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
            script_parse_fail(":a = 1\n^{:b = 2}\n:c = $b")
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
                ":kw = fn ^{:something} -> $something\nkw (something = hi)",
                SRString("hi"),
            )?;
            script_fail("nokw = fn ^{:something} -> (); nokw ^kw")?;
            Ok(())
        }

        #[test]
        fn command_args() -> Result<(), String> {
            script_eval_to(
                "to_array = fn ^:args -> args:positional\np = fn _ ^:rest -> to_array ^$rest\np 1 2 3",
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
            script_eval_to(":a = hello; $a", SRString("hello"))
        }

        #[test]
        fn literal() -> Result<(), String> {
            script_eval_to("fn () -> a |> ()", SRString("a"))?;
            script_eval_to("fn hello -> () |> hello", SRUnit)?;
            script_eval_to("fn [1,2,3] -> () |> [1,2,3]", SRUnit)?;
            script_eval_to("fn {:a=b} -> () |> {:a=b}", SRUnit)?;
            script_eval_to(":something = {:a=b}; fn $something -> () |> {:a=b}", SRUnit)?;
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
            script_fail("[:a,:b,:c] = [1,2]; $a")?;
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
            script_fail("[:x,^_,^_] = [1,2,3], $x")?;
            Ok(())
        }

        #[test]
        fn array_complex() -> Result<(), String> {
            script_eval_to(
                "[:a,^:rest1,:b,:c,anchor,^:rest2] = [1,2,3,anchor,4,5,6,7]\n[:d,^:rest2,:e] = $rest2",
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
                "{:a,:b=:binding} = {:a=1,:b=2}",
                SRMap(&[("a", SRString("1")), ("binding", SRString("2"))]),
            )
        }

        #[test]
        fn map_merge() -> Result<(), String> {
            script_eval_to(
                "{:a,:b=:binding,^:keys} = {:a=1,:b=2,:c=3,:d=4}",
                SRMap(&[
                    ("a", SRString("1")),
                    ("binding", SRString("2")),
                    ("keys", SRMap(&[("c", SRString("3")), ("d", SRString("4"))])),
                ]),
            )?;
            script_eval_to(
                "{:a,:b=:binding,^{:c=3,:d=4}} = {:a=1,:b=2,:c=3,:d=4}",
                SRMap(&[("a", SRString("1")), ("binding", SRString("2"))]),
            )?;
            Ok(())
        }

        #[test]
        fn map_mismatch() -> Result<(), String> {
            script_fail("fn {:a,:b} -> () |> {a=1,b=2,c=3}")?;
            script_fail("fn {:a,^:keys,^:keys2} -> () |> {:a=1}")?;
            Ok(())
        }

        #[test]
        fn index() -> Result<(), String> {
            script_eval_to("a = index :p -> $p; a:hello", SRString("hello"))?;
            Ok(())
        }

        #[test]
        fn bind_command() -> Result<(), String> {
            script_eval_to(
                ":keyto = fn :key -> {key = $key} -> (); :m = {:key = hi}; fn (keyto hi) -> () |> $m",
                SRUnit,
            )?;
            script_eval_to(
                ":keyto = fn :key -> {key = $key} -> (); :m = {:key = hi}; keyto :b = $m; $b",
                SRString("hi"),
            )?;
            script_fail(
                ":keyto = fn :key -> {key = $key} -> (); :m = {:key = hi}; fn (keyto bye) -> () |> $m",
            )?;
            script_fail(
                ":keyto = fn :key -> {key = $key} -> (); :m = {:notkey = hi}; fn (keyto hi) -> () |> $m",
            )?;
            Ok(())
        }

        #[test]
        fn nested_binds() -> Result<(), String> {
            script_eval_to(
                "p = fn :a -> :b -> bind $a $b; p [:k] = [1]; $k",
                SRString("1"),
            )?;
            script_eval_to(
                "p = fn :a -> :b -> bind $a $b; p {:k} = {k=1}; $k",
                SRString("1"),
            )?;
            Ok(())
        }

        #[test]
        fn rebinding() -> Result<(), String> {
            script_fail(
                "store = fn :a in :b -> () -> { $b = $a }; store :x in :y = (); $y = 1; $y = 2",
            )
        }
    }

    mod identity {
        use super::*;

        #[test]
        fn function_with_captures() -> Result<(), String> {
            script_eval_id_eq(
                "k = fn :b -> $b; fn :x -> k $x",
                "k = fn :b -> $b; hi = k hi; fn :x -> k $x",
            )?;
            script_eval_id_eq(
                "k = fn :b -> $b; r = fn :x -> $x; fn :x -> r $x",
                "k = fn :b -> $b; hi = k hi; r = fn :x -> $x; fn :x -> r $x",
            )?;
            Ok(())
        }

        #[test]
        fn same_capture_expressions() -> Result<(), String> {
            script_eval_to(
                "k = fn :a -> $a; p = fn :x -> k $x; q = fn :x -> k $x; [p hi, q hi]",
                SRArray(&[SRString("hi"), SRString("hi")]),
            )
        }

        #[test]
        fn basic_value() {
            script_eval_semantics("*id -");
        }

        #[test]
        fn forced_value() {
            script_eval_semantics("*id <| force +");
        }

        #[test]
        fn array() {
            script_eval_semantics("*id [-,-,-]");
            script_eval_semantics("*id [force +,-,force +,-]");
        }

        #[test]
        fn block() {
            script_eval_semantics("*id { -; -; - }");
            script_eval_semantics("*id { force +; -; - }");
            script_eval_semantics("*id { -; -; force + }");
        }

        #[test]
        fn unused_block_bind() {
            // No way to know that `b` is unused without static verification, which we don't
            // consider part of the runtime.
            script_eval_semantics("*id { a = -, b = force $a, +, - }");
        }

        #[test]
        fn used_block_bind() {
            script_eval_semantics("*id { a = +, b = force $a, $b, +, - }");
        }

        #[test]
        fn unused_block_fn() {
            script_eval_semantics("*id { f = fn :x -> force $x, +, - }");
        }

        #[test]
        fn used_block_fn() {
            script_eval_semantics("*id { f = fn :x -> force $x, f +, - }");
        }

        #[test]
        fn map_bind() {
            script_eval_semantics("*id { a = -, b = force + }");
        }

        #[test]
        fn used_map_bind() {
            script_eval_semantics("*id { a = +, b = force $a, c = - }");
        }

        #[test]
        fn unused_map_fn_bind() {
            script_eval_semantics("*id { f = fn :x -> force $x, +, b = - }");
        }

        #[test]
        fn used_map_fn_bind() {
            script_eval_semantics("*id { f = fn :x -> force $x, b = f +, +, c = - }");
        }

        #[test]
        fn fn_creation() {
            script_eval_semantics("*id <| fn :x -> { -; force + }");
            script_eval_semantics("*id <| fn :x -> { -; force $x }");
        }

        #[test]
        fn fn_call() {
            script_eval_semantics("*id <| fn :x -> { -; force + } |> -");
            script_eval_semantics("*id <| fn :x -> { -; force $x } |> +");
        }
    }

    mod force {
        use super::*;

        #[test]
        fn value_eval() -> Result<(), String> {
            script_eval_id_eq("force my-string", "my-string")
        }

        #[test]
        fn value_id() {
            // One is a String value, the other is a syntax string
            script_parse_id_ne("force my-string", "my-string");
        }

        #[test]
        fn equivalent_value() {
            script_parse_id_eq("force { x = a; $x }", "force a");
        }

        #[test]
        fn fn_inherit() {
            // Add the first `f a` to force both cases to evaluate the whole block (since otherwise
            // the first case will evaluate the whole block to bind `f` whereas the second case
            // will not).
            script_parse_id_eq(
                "f = fn :x -> force $x; f a",
                "f = fn :x -> force $x; force a",
            );
        }

        #[test]
        fn fn_no_inherit() {
            script_parse_id_ne("f = fn _ -> force a; f ()", "f = fn _ -> force a; force a");
        }

        #[test]
        fn block_dependency() {
            script_parse_id_eq("force a; ()", "force a; ()");
            script_parse_id_ne("force a; ()", "force b; ()");
        }

        #[test]
        fn map_dependency() {
            script_parse_id_eq("force a; x = x", "force a; x = x");
            script_parse_id_ne("force a; x = x", "force b; x = x");
        }

        #[test]
        fn map_unresolvable() {
            script_eval_to("{$force; x = 1}", SRMap(&[("x", SRString("1"))])).unwrap();
        }
    }

    fn script_eval_to(s: &str, expected: ScriptResult) -> Result<(), String> {
        let runtime = make_runtime()?;
        let script = script_eval(&runtime, s);
        let res = runtime.block_on(async { val_match(script?, expected).await });
        res
    }

    fn script_parse_fail(s: &str) -> Result<(), String> {
        let runtime = make_runtime()?;
        let failed = script_eval(&runtime, s);
        assert!(failed.is_err());
        Ok(())
    }

    fn script_fail(s: &str) -> Result<(), String> {
        let runtime = make_runtime()?;
        let mut should_fail = script_eval(&runtime, s)?;
        assert!(runtime.block_on(async move { Context::eval(&mut should_fail).await.is_err() }));
        Ok(())
    }

    fn script_eval_id_eq(a: &str, b: &str) -> Result<(), String> {
        let runtime = make_runtime()?;
        let mut a = script_eval(&runtime, a)?;
        let mut b = script_eval(&runtime, b)?;
        let a = runtime.block_on(async {
            Context::eval(&mut a).await.unwrap();
            a.as_identified().await
        });
        let b = runtime.block_on(async {
            Context::eval(&mut b).await.unwrap();
            b.as_identified().await
        });
        assert_eq!(a.id(), b.id());
        Ok(())
    }

    fn script_parse_id_eq(a: &str, b: &str) {
        let runtime = make_runtime().unwrap();
        let a = script_eval(&runtime, a).unwrap();
        let b = script_eval(&runtime, b).unwrap();
        let a = runtime.block_on(a.as_identified());
        let b = runtime.block_on(b.as_identified());
        assert_eq!(a.id(), b.id());
    }

    fn script_parse_id_ne(a: &str, b: &str) {
        let runtime = make_runtime().unwrap();
        let a = script_eval(&runtime, a).unwrap();
        let b = script_eval(&runtime, b).unwrap();
        let a = runtime.block_on(a.as_identified());
        let b = runtime.block_on(b.as_identified());
        assert_ne!(a.id(), b.id());
    }

    #[ergo_runtime::types::ergo_fn]
    async fn t_id(v: _) -> Value {
        v.id().await;
        ergo_runtime::types::Unit.into()
    }

    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    };

    #[derive(Clone)]
    struct Check(Arc<AtomicBool>);

    impl Check {
        fn new() -> Self {
            Check(Arc::new(AtomicBool::new(false)))
        }

        fn is_evaluated(&self) -> bool {
            self.0.load(Ordering::Relaxed)
        }

        fn has_evaluated(&mut self) {
            self.0.store(true, Ordering::Relaxed)
        }
    }

    fn check_eval() -> (Value, Check) {
        let c = Check::new();

        let mut v_c = c.clone();

        (
            Value::dynamic(
                move || async move {
                    v_c.has_evaluated();
                    ergo_runtime::types::Unit.into()
                },
                ergo_runtime::depends![const ergo_runtime::nsid!(test::musteval)],
            ),
            c,
        )
    }

    fn script_eval_semantics(a: &str) {
        let mut musteval_checks = Vec::new();
        let mut noeval_checks = Vec::new();
        let mut script = String::new();
        let mut env = HashMap::new();

        env.insert("*id".to_string(), t_id());

        let mut iter = a.chars().peekable();
        while let Some(c) = iter.next() {
            match c {
                '+' | '-' if iter.peek() != Some(&'>') => {
                    let (v, check) = check_eval();
                    let ind = musteval_checks.len() + noeval_checks.len();
                    let name = format!("*{}", ind);
                    script.push_str(&format!("${}", &name));
                    env.insert(name, v);
                    if c == '+' {
                        &mut musteval_checks
                    } else {
                        &mut noeval_checks
                    }
                    .push((check, ind));
                }
                c => script.push(c),
            }
        }

        let runtime = make_runtime().unwrap();
        let mut script = runtime.load_string("<test>", &script).unwrap();
        script.extend_top_level_env(env);
        let mut v = runtime.block_on(script.evaluate()).unwrap();
        runtime
            .block_on(async { Context::eval(&mut v).await })
            .unwrap();

        for (c, name) in musteval_checks {
            if !c.is_evaluated() {
                panic!("expr {} not evaluated", name);
            }
        }

        for (c, name) in noeval_checks {
            if c.is_evaluated() {
                panic!("expr {} evaluated", name);
            }
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

    fn val_match<'a>(v: Value, expected: ScriptResult) -> BoxFuture<'a, Result<(), String>> {
        dbg!(&v);
        async move {
            match expected {
                SRUnit => match Context::eval_as::<types::Unit>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(_) => Ok(()),
                },
                SRUnset => match Context::eval_as::<types::Unset>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(_) => Ok(()),
                },
                SRString(s) => match Context::eval_as::<types::String>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(v) => {
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
                SRArray(expected_arr) => match Context::eval_as::<types::Array>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(arr) => {
                        let arr = arr.to_owned().0;
                        if arr.len() != expected_arr.len() {
                            Err("array length mismatch".into())
                        } else {
                            for (v, expected_v) in arr.iter().zip(expected_arr) {
                                val_match(v.clone(), expected_v.clone()).await?;
                            }
                            Ok(())
                        }
                    }
                },
                SRMap(entries) => match Context::eval_as::<types::Map>(v).await {
                    Err(e) => Err(e.to_string()),
                    Ok(map) => {
                        let mut map = map.to_owned().0;
                        dbg!(&map);
                        if map.len() != entries.len() {
                            Err("map length mismatch".into())
                        } else {
                            for (k, e) in entries.iter() {
                                let k: ergo_runtime::IdentifiedValue =
                                    types::String::from(*k).into();
                                let v = map.remove(&k).ok_or("missing expected key")?;
                                val_match(v, e.clone()).await?;
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

    fn script_eval(runtime: &Runtime, s: &str) -> Result<Value, String> {
        runtime
            .evaluate_string("<test>", s)
            .map_err(|e| format!("{:?}", e))
    }
}
