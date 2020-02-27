//! Execute commands in order.

use super::builtin_function_prelude::*;
use super::script_deep_eval;

def_builtin!(ctx,args => {
    let mut args = args.into_iter();
    let mut val = args.next().ok_or("no commands provided")?;
    while let Some(next) = args.next() {
        val = next.map(|n| script_deep_eval(val).unwrap().then(n));
    }
    Ok(val.unwrap().into())
});
