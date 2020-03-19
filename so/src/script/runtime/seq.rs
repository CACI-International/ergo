//! Execute values in order, returning the result of the last one.

use super::builtin_function_prelude::*;
use super::script_deep_eval;

def_builtin!(ctx => {
    let mut val = ctx.args.next().ok_or("no values provided")?;
    while let Some(next) = ctx.args.next() {
        val = next.map(|n| script_deep_eval(val).unwrap().then(n));
    }

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    Ok(val.unwrap().into())
});
