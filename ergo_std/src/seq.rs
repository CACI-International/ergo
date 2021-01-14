//! Execute values in order, returning the result of the last one.

use ergo_runtime::{context_ext::AsContext, ergo_function, ContextExt};
use grease::{make_value, value::Value};

pub fn function() -> Value {
    ergo_function!(independent std::seq,
    r"Execute the arguments sequentially.

Arguments: value [value...]

Returns a value that is identical to the final value, but identified as if it depends on the previous values. When
executed, the returned value will execute each of the prior values in order and deeply (i.e., the indices of arrays and
maps will be evaluated concurrently).",
    |ctx, args| {
        let mut val = args.next().ok_or("no values provided")?;
        while let Some(next) = args.next() {
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

        args.unused_arguments()?;

        val.unwrap()
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn seq(t) {
            t.assert_fail("self:seq (self:error:throw bad) ()");
            t.assert_content_eq("self:seq a b c", "c");
            t.assert_eq("self:seq a", "a");
            t.assert_ne("self:seq a b c", "c");
        }
    }
}
