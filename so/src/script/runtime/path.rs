//! Join path components.

use super::builtin_function_prelude::*;

def_builtin!(ctx => {
    let mut args = Vec::new();
    while let Some(sv) = ctx.args.next() {
        let ret = sv.map(|v| {
            // TODO support PathBuf?
            v.typed::<ScriptString>()
                .map_err(|_| "all arguments must be strings".into())
                .and_then(|v| v.get())
        })
        .transpose_err();
        args.push(
        match ret {
            Err(e) => {
                ctx.error(e);
                Eval::Error
            }
            Ok(v) => Eval::Value(v)
        }
        )
    }

    let args: Eval<Vec<_>> = args.into_iter().collect();

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

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
