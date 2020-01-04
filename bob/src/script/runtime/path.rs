//! Join path components.

use super::script_types::*;
use super::{EvalError, FunctionContext};
use grease::{Context, Value};

pub fn path_builtin() -> Value {
    ScriptFunction::BuiltinFunction(Box::new(path)).into()
}

fn path(ctx: &mut Context<FunctionContext>) -> Result<Value, EvalError> {
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut args = args
        .into_iter()
        .map(|sv| {
            sv.map(|v| {
                v.typed::<ScriptString>()
                    .map_err(|_| "all arguments must be strings".into())
                    .and_then(|v| v.get())
            })
            .transpose_err()
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter();
    let first = args
        .next()
        .ok_or("at least one path component is required".to_owned())?
        .owned();

    let mut path = std::path::PathBuf::from(first);
    for a in args {
        path.push(a.as_ref());
    }

    path.into_os_string()
        .into_string()
        .map(|v| v.into())
        .map_err(|_| "invalid path".into())
}
