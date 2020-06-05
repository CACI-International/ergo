//! Execute values in order, returning the result of the last one.

use super::builtin_function_prelude::*;
use crate::script::traits::nested::force_value_nested;
use grease::depends;

def_builtin!(ctx => {
    let mut val = ctx.args.next().ok_or("no values provided")?;
    while let Some(next) = ctx.args.next() {
        // TODO attribute errors to correct Source
        val = next.map(|n| val.map(|v| {
            let deps = depends![v];
            let traits = ctx.traits.clone();
            Value::new(v.value_type(), async move {
                force_value_nested(&traits, v.clone()).await?;
                Ok(v.await.expect("error should have been caught previously"))
            }, deps)
        }).unwrap().then(n));
    }

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    Ok(val.unwrap().into())
});
