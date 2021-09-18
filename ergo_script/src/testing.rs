//! Testing helpers.

use ergo_runtime::{traits, type_system::ErgoType, Context, RResult, Result, Value};
use std::collections::BTreeMap;

pub struct Test {
    env: BTreeMap<String, Value>,
    runtime: crate::Runtime,
}

impl Test {
    pub fn new(
        plugin_entry: extern "C" fn(ergo_runtime::plugin::Context) -> RResult<Value>,
    ) -> Self {
        let rt = crate::Runtime::new(
            Context::builder().threads(Some(1)).keep_going(false),
            vec![],
        )
        .expect("failed to create runtime");

        let plugin = rt
            .ctx
            .block_on(async move { plugin_entry(ergo_runtime::plugin::Context::get()) })
            .expect("plugin failed to load");

        let env = vec![("self".into(), plugin)].into_iter().collect();

        Test { env, runtime: rt }
    }

    pub fn eval(&self, script: &str) -> Result<Value> {
        let mut script = self.runtime.load_string("test", script)?;
        script.extend_top_level_env(self.env.clone());
        dbg!(self.block_on(script.evaluate()))
    }

    pub fn eval_success(&self, script: &str) -> Value {
        self.eval(script).expect("expected successful eval")
    }

    pub fn assert_value_eq<T: ErgoType + std::fmt::Debug + Sync + PartialEq + 'static>(
        &self,
        script: &str,
        v: &T,
    ) {
        let val = self.eval_success(script);
        let val = self
            .block_on(Context::eval_as::<T>(val))
            .expect("type mismatch");
        assert_eq!(val.as_ref(), v);
    }

    pub fn assert_script_success(&self, script: &str) {
        self.eval_success(script);
    }

    pub fn assert_script_fail(&self, script: &str) {
        self.eval(script).expect_err("expected failure");
    }

    pub fn assert_success(&self, script: &str) {
        let mut v = self.eval_success(script);
        self.block_on(Context::eval(&mut v)).expect("value failed");
    }

    pub fn assert_fail(&self, script: &str) {
        let mut v = self.eval_success(script);
        self.block_on(Context::eval(&mut v))
            .expect_err("value succeeded");
    }

    pub fn assert_script_eq(&self, a: &str, b: &str) {
        let a = self.eval(a).expect("eval error");
        let b = self.eval(b).expect("eval error");
        self.dbg(&a);
        self.dbg(&b);
        assert_eq!(a, b);
    }

    pub fn assert_script_ne(&self, a: &str, b: &str) {
        let a = self.eval(a).expect("eval error");
        let b = self.eval(b).expect("eval error");
        self.dbg(&a);
        self.dbg(&b);
        assert_ne!(a, b);
    }

    pub fn assert_eq(&self, a: &str, b: &str) {
        let mut a = self.eval(a).expect("eval error");
        let mut b = self.eval(b).expect("eval error");
        drop(self.block_on(Context::eval(&mut a)));
        drop(self.block_on(Context::eval(&mut b)));
        self.dbg(&a);
        self.dbg(&b);
        assert_eq!(a, b);
    }

    pub fn assert_ne(&self, a: &str, b: &str) {
        let mut a = self.eval(a).expect("eval error");
        let mut b = self.eval(b).expect("eval error");
        drop(self.block_on(Context::eval(&mut a)));
        drop(self.block_on(Context::eval(&mut b)));
        self.dbg(&a);
        self.dbg(&b);
        assert_ne!(a, b);
    }

    pub fn assert_content_eq(&self, a: &str, b: &str) {
        let a = self.eval(a).expect("eval error");
        let b = self.eval(b).expect("eval error");
        let a = self.block_on(traits::value_by_content(a, true));
        let b = self.block_on(traits::value_by_content(b, true));
        self.dbg(&a);
        self.dbg(&b);
        assert_eq!(a, b);
    }

    fn dbg(&self, v: &Value) {
        dbg!(self.block_on(async move { traits::type_name(v) }));
        match self.block_on(traits::to_string(v.clone())) {
            Ok(s) => {
                dbg!(s);
            }
            Err(e) => {
                dbg!(e);
            }
        }
    }

    fn block_on<R, Fut: std::future::Future<Output = R> + Send>(&self, fut: Fut) -> R {
        self.runtime.ctx.block_on(fut)
    }
}

#[macro_export]
macro_rules! tests {
    ( ) => {};
    ( fn $name:ident ( $t:ident ) $b:block $($rest:tt)* ) => {
        #[test]
        fn $name() {
            let $t = $crate::testing::Test::new(crate::_ergo_plugin);
            $b
        }

        $crate::tests! { $($rest)* }
    };
}
