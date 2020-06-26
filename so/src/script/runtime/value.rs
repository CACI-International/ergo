//! Evaluate a value and return a value that is content-identified.

use super::builtin_function_prelude::*;
use crate::script::traits::content_value::ContentValue;

def_builtin!(ctx => {
    let val = ctx.args.next().ok_or("value not provided")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let (source,val) = val.take();

    match ctx.traits.get::<ContentValue>(&val) {
        Some(t) => t.content_value(val).map(|v| Eval::Value(v)),
        None => {
            let err_str = format!("ContentValue not implemented for {}", grease::type_name(&ctx.traits, &val.value_type()));
            ctx.error(source.with(err_str));
            Ok(Eval::Error)
        }
    }
});
