//! Mapping over arrays.

use super::builtin_function_prelude::*;
use grease::{Plan, SplitInto};

def_builtin!(ctx,args => {
    let mut args = args.into_iter();
    let func = args.next().ok_or("map function not provided")?;
    let arr = args.next().ok_or("map array not provided")?;
    if let Some(v) = args.next() {
        ctx.error(v.with("extraneous argument to map"));
        return Ok(Eval::Error);
    }

    let func = eval_error!(ctx, func
        .map(|f| {
            f.typed::<ScriptFunction>()
                .map_err(|_| "first argument must be a function".into())
                .and_then(|v| v.get())
        })
        .transpose_err());

    let ScriptArray(arr) = eval_error!(ctx, arr
        .map(|a| {
            a.typed::<ScriptArray>()
                .map_err(|_| "second argument must be an array".into())
                .and_then(|v| v.get())
                .map(|v| v.owned())
        })
        .transpose_err());

    let arr = arr
        .into_iter()
        .map(|d| {
            let source = d.source();
            match ctx.split_map(|ctx: &mut Context<super::Context>| func.plan_join(ctx, vec![d])) {
                Ok(v) => v.map(|v| source.with(v)),
                Err(e) => {
                    ctx.error(source.with(e));
                    Eval::Error
                }
            }
        })
        .collect::<Eval<Vec<_>>>();
    Ok(arr.map(|arr| ScriptArray(arr).into()))
});
