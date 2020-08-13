//! Debugging tools.

use super::builtin_function_prelude::*;

def_builtin!(ctx => {
    let val = ctx.args.next().ok_or("value not provided")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    ctx.log.debug(val.id());

    Ok(Eval::Value(val.unwrap()))
});
