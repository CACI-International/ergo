//! Testing helpers.

use crate::{Script, StringSource};
use ergo_runtime::{source::Source, ContextExt, EvalResult, Runtime, ScriptEnv};
use grease::types::GreaseType;

pub struct Test {
    rt: Runtime,
    env: ScriptEnv,
}

impl Test {
    pub fn new(
        plugin_entry: extern "C" fn(
            ergo_runtime::plugin::Context,
            &mut Runtime,
        ) -> abi_stable::std_types::RResult<
            Source<grease::Value>,
            grease::Error,
        >,
    ) -> Self {
        let mut rt = crate::script_context(
            grease::runtime::Context::builder()
                .threads(Some(1))
                .keep_going(false),
            vec![],
        )
        .expect("failed to create runtime");

        let plugin = plugin_entry(ergo_runtime::plugin::Context::get(), &mut rt);

        let mut env = ScriptEnv::default();
        env.insert(ergo_runtime::types::String::from("self").into(), plugin);

        Test { rt, env }
    }

    pub fn eval(&self, script: &str) -> EvalResult {
        let source = Source::new(StringSource::new("<test>", script.to_owned()));
        let mut script = Script::load(source)?;
        script.top_level_env(self.env.clone());
        let task = self.rt.task.clone();
        dbg!(task.block_on(script.evaluate(&self.rt)))
    }

    pub fn eval_success(&self, script: &str) -> Source<grease::Value> {
        self.eval(script).expect("expected successful eval")
    }

    pub fn assert_value_eq<T: GreaseType + std::fmt::Debug + Sync + PartialEq + 'static>(
        &self,
        script: &str,
        v: &T,
    ) {
        let val = self.eval_success(script);
        let val = self
            .rt
            .task
            .block_on(self.rt.source_value_as::<T>(val))
            .expect("type mismatch")
            .unwrap();
        let val = self.rt.task.block_on(val).expect("value failed");
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
        self.rt.task.block_on(v).as_ref().expect("value failed");
    }

    pub fn assert_fail(&self, script: &str) {
        let v = self.eval_success(script).unwrap();
        self.rt
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
        let task = self.rt.task.clone();
        let a = task
            .block_on(self.rt.value_by_content(a, true))
            .expect("value error");
        let b = task
            .block_on(self.rt.value_by_content(b, true))
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
