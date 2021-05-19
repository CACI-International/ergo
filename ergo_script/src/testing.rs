//! Testing helpers.

use crate::{Script, StringSource};
use ergo_runtime::{
    traits::value_by_content, type_system::ErgoType, Context, Error, Source, Value,
};

pub struct Test {
    ctx: Context,
    env: ScriptEnv,
}

impl Test {
    pub fn new(
        plugin_entry: extern "C" fn(
            ergo_runtime::plugin::Context,
            &Context,
        )
            -> abi_stable::std_types::RResult<Source<Value>, Error>,
    ) -> Self {
        let (ctx, _) = crate::script_context(
            Context::builder().threads(Some(1)).keep_going(false),
            vec![],
        )
        .expect("failed to create runtime");

        let plugin = plugin_entry(ergo_runtime::plugin::Context::get(), &ctx);

        let mut env = ScriptEnv::default();
        env.insert(
            ergo_runtime::types::String::from("self").into(),
            (plugin, None.into()).into(),
        );

        Test { ctx, env }
    }

    pub fn eval(&self, script: &str) -> EvalResult {
        let source = Source::new(StringSource::new("<test>", script.to_owned()));
        let mut script = Script::load(source)?;
        script.top_level_env(self.env.clone());
        let task = self.rt.task.clone();
        dbg!(task.block_on(script.evaluate(&self.rt)))
    }

    pub fn eval_success(&self, script: &str) -> Source<Value> {
        self.eval(script).expect("expected successful eval")
    }

    pub fn assert_value_eq<T: ErgoType + std::fmt::Debug + Sync + PartialEq + 'static>(
        &self,
        script: &str,
        v: &T,
    ) {
        let val = self.eval_success(script);
        let val = self
            .ctx
            .task
            .block_on(self.ctx.eval_as::<T>(val))
            .expect("type mismatch")
            .unwrap();
        let val = self.ctx.task.block_on(val).expect("value failed");
        assert_eq!(val.as_ref(), v);
    }

    pub fn assert_script_success(&self, script: &str) {
        self.eval_success(script);
    }

    pub fn assert_script_fail(&self, script: &str) {
        self.eval(script).expect_err("expected failure");
    }

    pub fn assert_success(&self, script: &str) {
        let v = self.eval_success(script).unwrap();
        self.ctx.task.block_on(v).as_ref().expect("value failed");
    }

    pub fn assert_fail(&self, script: &str) {
        let v = self.eval_success(script).unwrap();
        self.ctx
            .task
            .block_on(v)
            .as_ref()
            .expect_err("value succeeded");
    }

    pub fn assert_eq(&self, a: &str, b: &str) {
        let a = self.eval(a).expect("eval error");
        let b = self.eval(b).expect("eval error");
        assert_eq!(a, b);
    }

    pub fn assert_ne(&self, a: &str, b: &str) {
        let a = self.eval(a).expect("eval error");
        let b = self.eval(b).expect("eval error");
        assert_ne!(a, b);
    }

    pub fn assert_content_eq(&self, a: &str, b: &str) {
        let a = self.eval(a).expect("eval error").unwrap();
        let b = self.eval(b).expect("eval error").unwrap();
        let a = self
            .ctx
            .task
            .block_on(value_by_content(&self.ctx, a, true))
            .expect("value error");
        let b = self
            .ctx
            .task
            .block_on(value_by_content(&self.ctx, b, true))
            .expect("value error");
        assert_eq!(a, b);
    }
}

#[macro_export]
macro_rules! test {
    ( fn $name:ident ( $t:ident ) $b:block ) => {
        #[test]
        fn $name() {
            let $t = $crate::testing::Test::new(crate::_ergo_plugin);
            $b
        }
    };
}
