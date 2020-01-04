//! Check whether an index exists

use super::script_types::*;
use super::{EvalError, FunctionContext};
use grease::{match_value, Context, IntoValue, Value};
use std::str::FromStr;

pub fn has_builtin() -> Value {
    ScriptFunction::BuiltinFunction(Box::new(has)).into()
}

fn has(ctx: &mut Context<FunctionContext>) -> Result<Value, EvalError> {
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut args = args.into_iter();
    let val = args.next().ok_or("value not provided")?;
    let ind = args.next().ok_or("index not provided")?;
    if let Some(v) = args.next() {
        return Err(v.with("extraneous arguments to has").into());
    }

    let ind = ind
        .map(|i| {
            i.typed::<String>()
                .map_err(|_| "index must be a string".into())
                .and_then(|v| v.get())
        })
        .transpose_err()?;

    Ok(val
        .map(|v| {
            match_value!(v => {
                ScriptArray => |val| match usize::from_str(&ind) {
                    Err(_) => Ok(false),
                    Ok(ind) => val.get().map(|v| v.0.get(ind).is_some())
                }
                ScriptMap => |val| val.get().map(|v| v.0.get(ind.as_ref()).is_some()),
                => |_| Err("expected an array or map".into())
            })
        })
        .transpose_err()?
        .into_value())
}
