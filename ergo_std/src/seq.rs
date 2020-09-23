//! Execute values in order, returning the result of the last one.

use ergo_runtime::{ergo_function, traits::force_value_nested};
use grease::{depends, value::Value};

pub fn function() -> Value {
    ergo_function!(independent std::seq, |ctx| {
        let mut val = ctx.args.next().ok_or("no values provided")?;
        while let Some(next) = ctx.args.next() {
            // TODO attribute errors to correct Source
            val = next.map(|n| {
                val.map(|v| {
                    let deps = depends![v];
                    let traits = ctx.traits.clone();
                    Value::new(
                        v.grease_type(),
                        async move {
                            force_value_nested(&traits, v.clone()).await?;
                            Ok(v.await.expect("error should have been caught previously"))
                        },
                        deps,
                    )
                })
                .unwrap()
                .then(n)
            });
        }

        ctx.unused_arguments()?;

        val.unwrap()
    })
    .into()
}
