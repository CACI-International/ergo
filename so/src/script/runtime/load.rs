//! Load external files.

use super::{apply_value, builtin_function_prelude::*};
use crate::script::{
    ast::{FileSource, Source},
    Script,
};
use grease::{Plan, SplitInto};
use std::path::PathBuf;

def_builtin!(ctx,args => {
    let mut args = args.into_iter();
    let path = args.next().ok_or(EvalError::from("no path provided"))?;

    let (source,path) = path.map(|i| i.typed::<String>().map_err(|_| "path must be a string".into())
        .and_then(|v| v.get()))
        .transpose()?.take();

    let p: PathBuf = path.as_ref().into();
    let script = Script::load(Source::new(FileSource(p))).map_err(|e| e.to_string())?;
    let val = source.with(script.plan_split(ctx)?.unwrap());
    ctx.split_map(|ctx| apply_value(ctx, val, args, false))
});
