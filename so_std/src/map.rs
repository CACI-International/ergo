//! Mapping over arrays.

use super::builtin_function_prelude::*;
use grease::{Plan, SplitInto};

def_builtin!(ctx => {
    let func = ctx.args.next().ok_or("map function not provided")?;
    let arr = ctx.args.next().ok_or("map array not provided")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let func = eval_error!(ctx, func
        .map(|f| {
            f.typed::<ScriptFunction>()
                .map_err(|_| "first argument must be a function".into())
                .and_then(|v| v.get())
        })
        .transpose());
    let func = func.as_ref().map(|v| v.as_ref());

    let source = arr.source();

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
            ctx.split_map(|ctx: &mut Context<Runtime>|
                func.clone().plan_join(ctx, (FunctionArguments::positional(vec![source.clone().with(d)]), func.source())))
        })
        .collect::<Eval<Vec<_>>>();
    Ok(arr.map(|arr| ScriptArray(arr.into_iter().map(|v| v.unwrap()).collect()).into()))
});
