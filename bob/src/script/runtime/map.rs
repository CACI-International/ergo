//! Mapping over arrays.

use super::builtin_function_prelude::*;
use grease::{Plan, SplitInto};

def_builtin!(ctx,args => {
    let mut args = args.into_iter();
    let func = args.next().ok_or("map function not provided")?;
    let arr = args.next().ok_or("map array not provided")?;
    if let Some(v) = args.next() {
        return Err(v.with("extraneous argument to map").into());
    }

    let func = func
        .map(|f| {
            f.typed::<ScriptFunction>()
                .map_err(|_| "first argument must be a function".into())
                .and_then(|v| v.get())
        })
        .transpose_err()?;

    let ScriptArray(arr) = arr
        .map(|a| {
            a.typed::<ScriptArray>()
                .map_err(|_| "second argument must be an array".into())
                .and_then(|v| v.get())
                .map(|v| v.owned())
        })
        .transpose_err()?;

    let arr = arr
        .into_iter()
        .map(|d| {
            let source = d.source();
            ctx.split_map(|ctx: &mut Context<super::Context>| func.plan_join(ctx, vec![d]))
                .map(|r| source.with(r))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ScriptArray(arr).into())
});
