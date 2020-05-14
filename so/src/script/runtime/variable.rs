//! Function to indicate that a value is variable.
//!
//! This amounts to setting a fixed identifier for the value, so that downstream values have
//! consistent identifiers even if the indicated value changes.

use super::{apply_value, builtin_function_prelude::*};
use grease::{depends, SplitInto};

def_builtin!(ctx => {
    let mut v = ctx.args.next().ok_or("no argument to variable")?;

    let is_deps_flag = eval_error!(ctx, v.clone().map(|v| {
        v.typed::<ScriptString>().map(|v| v.get().map(|s| *s == "--deps")).unwrap_or(Ok(false))
    }).transpose_err());

    let deps = if is_deps_flag {
        let deps = ctx.args.next().ok_or("no value given to --deps")?;
        v = ctx.args.next().ok_or("no variable value provided")?;
        depends![deps.unwrap()]
    }
    else { Default::default() };

    let args = std::mem::take(&mut ctx.args);

    let varies = match ctx.split_map(move |ctx| apply_value(ctx, v, args.unchecked(), true))? {
        Eval::Error => return Ok(Eval::Error),
        Eval::Value(v) => v
    };

    Ok(Eval::Value(varies.set_dependencies(deps)))
});
