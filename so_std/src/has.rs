//! Check whether an index exists

use super::builtin_function_prelude::*;
use grease::{match_value, IntoValue};
use std::str::FromStr;

def_builtin!(ctx => {
    let val = ctx.args.next().ok_or("value not provided")?;
    let ind = ctx.args.next().ok_or("index not provided")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let ind = eval_error!(ctx, ind
        .map(|i| {
            i.typed::<String>()
                .map_err(|_| "index must be a string".into())
                .and_then(|v| v.get())
        })
        .transpose_err());

    Ok(eval_error!(ctx, val
        .map(|v| {
            match_value!(v => {
                ScriptArray => |val| match usize::from_str(&ind) {
                    Err(_) => Ok(false),
                    Ok(ind) => val.get().map(|v| v.0.get(ind).is_some())
                }
                ScriptMap => |val| val.get().map(|v| v.0.get(ind.as_ref()).is_some()),
                => |_| Ok(false)
            })
        })
        .transpose_err())
        .into_value().into())
});
