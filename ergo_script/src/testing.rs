//! Testing helpers.

use ergo_runtime::{
    traits, type_system::ErgoType, Context, IdentifiedValue, RResult, Result, Value,
};
use std::collections::HashMap;

pub struct Test {
    env: HashMap<String, Value>,
    runtime: crate::Runtime,
}

impl Test {
    pub fn new(
        plugin_entry: extern "C" fn(ergo_runtime::plugin::Context) -> RResult<Value>,
    ) -> Self {
        let rt = crate::Runtime::new(
            Context::builder().threads(Some(1)).keep_going(false),
            vec![],
            Default::default(),
            false,
        )
        .expect("failed to create runtime");

        let plugin = rt
            .ctx
            .block_on(async move {
                plugin_entry(ergo_runtime::plugin::Context::get(
                    ergo_runtime::Source::missing(()),
                ))
            })
            .expect("plugin failed to load");

        let env = vec![("self".into(), plugin)].into_iter().collect();

        Test { env, runtime: rt }
    }

    pub fn eval(&self, script: &str) -> Result<IdentifiedValue> {
        let mut script = self.runtime.load_string("test", script)?;
        script.extend_top_level_env(self.env.clone());
        dbg!(self.block_on(async move {
            let v = script.evaluate().await;
            match v {
                Ok(v) => Ok(v.as_identified().await),
                Err(e) => Err(e),
            }
        }))
    }

    pub fn eval_success(&self, script: &str) -> IdentifiedValue {
        self.eval(script).expect("expected successful eval")
    }

    pub fn eval_success_v(&self, script: &str) -> Value {
        self.eval_success(script).into()
    }

    pub fn assert_value_eq<T: ErgoType + std::fmt::Debug + Sync + PartialEq + 'static>(
        &self,
        script: &str,
        v: &T,
    ) {
        let val = self.eval_success(script);
        let val = self
            .block_on(Context::eval_as::<T>(val.into()))
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
        let mut v = self.eval_success_v(script);
        self.block_on(Context::eval(&mut v)).expect("value failed");
    }

    pub fn assert_fail(&self, script: &str) {
        let mut v = self.eval_success_v(script);
        self.block_on(Context::eval(&mut v))
            .expect_err("value succeeded");
    }

    pub fn assert_script_eq(&self, a: &str, b: &str) {
        let a = self.eval_success(a);
        let b = self.eval_success(b);
        self.dbg(&a);
        self.dbg(&b);
        assert_eq!(a, b);
    }

    pub fn assert_script_ne(&self, a: &str, b: &str) {
        let a = self.eval_success(a);
        let b = self.eval_success(b);
        self.dbg(&a);
        self.dbg(&b);
        assert_ne!(a, b);
    }

    pub fn assert_eq(&self, a: &str, b: &str) {
        let mut a = self.eval_success_v(a);
        let mut b = self.eval_success_v(b);
        let a = self.block_on(async {
            drop(Context::eval(&mut a).await);
            a
        });
        let b = self.block_on(async {
            drop(Context::eval(&mut b).await);
            b
        });
        self.dbg(&a);
        self.dbg(&b);
        self.block_on(traits::bind_no_error(b, a))
            .expect("bind error");
    }

    pub fn assert_ne(&self, a: &str, b: &str) {
        let mut a = self.eval_success_v(a);
        let mut b = self.eval_success_v(b);
        let a = self.block_on(async {
            drop(Context::eval(&mut a).await);
            a
        });
        let b = self.block_on(async {
            drop(Context::eval(&mut b).await);
            b
        });
        self.dbg(&a);
        self.dbg(&b);
        self.block_on(traits::bind_no_error(b, a))
            .expect_err("bind unexpectedly succeeded");
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
