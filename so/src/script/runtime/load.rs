//! Load external files.

use super::builtin_function_prelude::*;
use crate::script::{
    ast::{FileSource, Source},
    Script,
};
use grease::Plan;
use std::path::PathBuf;

def_builtin!(ctx,args => {
    let mut args = args.into_iter();
    let path = args.next().ok_or(EvalError::from("no path provided"))?;
    if args.next().is_some() {
        return Err("extraneous arguments to load".into());
    }

    let path = path.map(|i| i.typed::<String>().map_err(|_| "path must be a string".into())
        .and_then(|v| v.get()))
        .transpose_err()?;

    let p: PathBuf = path.as_ref().into();
    let script = Script::load(Source::new(FileSource(p))).map_err(|e| e.to_string())?;
    Ok(script.plan_split(ctx)?.unwrap())
});
