//! Filesystem runtime functions.

use super::builtin_function_prelude::*;
use glob::glob;
use grease::make_value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

pub fn builtin() -> Value {
    let mut map = BTreeMap::new();
    map.insert("copy".to_owned(), copy_fn());
    map.insert("exists".to_owned(), exists_fn());
    map.insert("glob".to_owned(), glob_fn());
    ScriptMap(map).into()
}

script_fn!(glob_fn, ctx => {
    let pattern = ctx.args.next().ok_or("no glob pattern provided")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let pattern_source = pattern.source();
    let pattern =
        script_value_as!(ctx, pattern, ScriptString, "glob pattern must be a string");

    match glob(pattern.as_ref()) {
        Err(e) => Err(Error::ValueError(pattern_source.with(e).into())),
        Ok(paths) => {
            let paths: Result<Vec<PathBuf>, glob::GlobError> = paths.collect();
            let paths: Vec<Value> = paths
                .map_err(|e| Error::ValueError(pattern_source.with(e).into()))?
                .into_iter()
                .map(|v| v.into())
                .collect();
            Ok(Eval::Value(ScriptArray(paths).into()))
        }
    }
});

fn recursive_link<F: AsRef<Path>, T: AsRef<Path>>(from: F, to: T) -> Result<(), std::io::Error> {
    if to.as_ref().is_dir() {
        let mut to = to.as_ref().to_owned();
        to.push(from.as_ref().file_name().expect("path ends in .."));
        return recursive_link(from, &to);
    }

    let meta = std::fs::metadata(from.as_ref())?;
    if meta.is_dir() {
        std::fs::create_dir_all(to.as_ref())?;
        for d in from.as_ref().read_dir()? {
            let d = d?;
            let name = d.file_name();
            let mut to = to.as_ref().to_owned();
            to.push(name);
            recursive_link(d.path(), &to)?;
        }
        Ok(())
    } else {
        std::fs::hard_link(from, to)
    }
}

script_fn!(copy_fn, ctx => {
    let from = ctx.args.next().ok_or("'from' missing")?;
    let to = ctx.args.next().ok_or("'to' missing")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let from = eval_error!(ctx, from.map(|f| f.typed::<PathBuf>().map_err(|_| "'from' argument must be a path"))
        .transpose_err());

    let to = eval_error!(ctx, to.map(|t| t.typed::<PathBuf>().map_err(|_| "'to' argument must be a path"))
        .transpose_err());

    let log = ctx.log.sublog("fs::copy");
    let task = ctx.task.clone();
    Ok(Eval::Value(make_value!((from,to) ["fs copy"] {
        let (from,to) = task.join(from, to).await?;

        log.debug(format!("copying {} to {}", from.display(), to.display()));

        Ok(recursive_link(from.as_ref(), to.as_ref())?)
    }).into()))
});

script_fn!(exists_fn, ctx => {
    let path = ctx.args.next().ok_or("'path' missing")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let path = eval_error!(ctx, path.map(|f| f.typed::<PathBuf>().map_err(|_| "'path' argument must be a path"))
        .transpose_err());

    Ok(Eval::Value(make_value!((path) ["fs exists"] {
        Ok(path.await?.as_ref().exists())
    }).into()))
});
