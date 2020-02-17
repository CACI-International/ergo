//! Join path components.

use super::builtin_function_prelude::*;

def_builtin!(ctx,args => {
    let args = args
        .into_iter()
        .map(|sv| {
            let ret = sv.map(|v| {
                // TODO support PathBuf?
                v.typed::<ScriptString>()
                    .map_err(|_| "all arguments must be strings".into())
                    .and_then(|v| v.get())
            })
            .transpose_err();
            match ret {
                Err(e) => {
                    ctx.error(e);
                    Eval::Error
                }
                Ok(v) => Eval::Value(v)
            }
        })
        .collect::<Eval<Vec<_>>>();
    let mut args = match args {
        Eval::Error => return Ok(Eval::Error),
        Eval::Value(v) => v.into_iter()
    };
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
        .map(|v| Eval::Value(v.into()))
        .map_err(|_| "invalid path".into())
});
