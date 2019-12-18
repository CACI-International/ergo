//! Join path components.

use super::{Data, DataFunction, FunctionContext, FunctionError};
use grease::Context;

pub fn path_builtin() -> Data {
    Data::Function(DataFunction::BuiltinFunction(Box::new(path)).into())
}

fn path(ctx: &mut Context<FunctionContext>) -> Result<Data, FunctionError> {
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut args = args
        .into_iter()
        .map(|a| match a {
            Data::String(s) => Ok(s),
            _ => Err("all arguments must be strings".to_owned()),
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter();
    let first = args
        .next()
        .ok_or("at least one path component is required".to_owned())?;

    let mut path = std::path::PathBuf::from(first);
    for a in args {
        path.push(a);
    }
    Ok(Data::String(
        path.into_os_string()
            .into_string()
            .map_err(|_| "invalid path".to_owned())?,
    ))
}
