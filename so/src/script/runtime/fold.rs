//! Folding over arrays.

use super::builtin_function_prelude::*;
use grease::{Plan, SplitInto};

def_builtin!(ctx => {
    let func = ctx.args.next().ok_or("fold function not provided")?;
    let orig = ctx.args.next().ok_or("fold base value not provided")?;
    let vals = ctx.args.next().ok_or("fold values not provided")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let func = eval_error!(ctx, func.map(|f|
            f.typed::<ScriptFunction>()
                .map_err(|_| "expected a function".into())
                .and_then(|v| v.get())
        ).transpose());
    let func = func.as_ref().map(|v| v.as_ref());

    let source = vals.source();

    let ScriptArray(vals) = eval_error!(ctx, vals
            .map(|a| {
                a.typed::<ScriptArray>()
                    .map_err(|_| "expected an array".into())
                    .and_then(|v| v.get())
                    .map(|v| v.owned())
            })
            .transpose_err());

    let result = vals.into_iter()
        .fold(Eval::Value(orig), |acc,v| {
            let acc = match acc {
                Eval::Error => return Eval::Error,
                Eval::Value(v) => v
            };
            ctx.split_map(|ctx: &mut Context<Runtime>|
                func.clone().plan_join(ctx, (FunctionArguments::positional(vec![acc,source.clone().with(v)]), func.source())))
        });
    Ok(result.map(|v| v.unwrap()))
});
