//! Execute values in order, returning the result of the last one.

use ergo_runtime::{context_ext::AsContext, ergo_function, ContextExt};
use grease::{make_value, value::Value};

pub fn function() -> Value {
    ergo_function!(independent std::seq, |ctx| {
        let mut val = ctx.args.next().ok_or("no values provided")?;
        while let Some(next) = ctx.args.next() {
            // TODO attribute errors to the correct Source
            val = next.map(|n| {
                val.map(|v| {
                    let ctx = ctx.as_context().clone();
                    Value::from(make_value!([v] {
                        ctx.force_value_nested(v).await
                    }))
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
