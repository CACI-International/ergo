//! Check whether an index exists

use super::builtin_function_prelude::*;
use grease::{match_value, IntoValue};
use std::str::FromStr;

def_builtin!(ctx,args => {
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
                => |_| Ok(false)
            })
        })
        .transpose_err()?
        .into_value())
});
