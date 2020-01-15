//! Execute commands in order.

use super::builtin_function_prelude::*;
use super::{script_deep_eval, Source};
use grease::IntoValue;

def_builtin!(ctx,args => {
    let mut args = args.into_iter();
    let arr = args.next().ok_or(EvalError::from("no commands provided"))?;
    if args.next().is_some() {
        return Err("extraneous arguments to do".into());
    }

    let (arrsource, arr) = arr.take();

    let arr: Result<Vec<Source<Value>>, EvalError> = match arr.typed::<ScriptArray>() {
        Ok(val) => val.get().map(|alias| alias.owned().0),
        Err(_) => Err("argument to do must be an array".to_owned()),
    }
    .map_err(|e| arrsource.with(e).into());

    let arr = arr?;

    let mut val = Source::builtin(().into_value());
    for a in arr {
        val = script_deep_eval(val).map(|v| v.then(a.unwrap()));
    }

    Ok(val.unwrap())
});
