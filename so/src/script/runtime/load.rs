//! Load external files.

use super::{apply_value, builtin_function_prelude::*};
use grease::SplitInto;

def_builtin!(ctx => {
    let path = ctx.args.next().ok_or("no load target provided")?;

    let (source, path) = eval_error!(ctx, path.map(|i| i.typed::<String>().map_err(|_| "target must be a string".into())
        .and_then(|v| v.get()))
        .transpose()).take();

    let val = match ctx.split_map(move |ctx: &mut Context<super::Context>| ctx.load(path.as_ref()))? {
        Eval::Error => return Ok(Eval::Error),
        Eval::Value(v) => v
    };
    let args = std::mem::take(&mut ctx.args);
    ctx.split_map(move |ctx| apply_value(ctx, source.with(val.unwrap()), args, false))
});
