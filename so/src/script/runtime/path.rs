//! Path operations module.

use super::builtin_function_prelude::*;
use grease::{depends, item_name, make_value, match_value, Dependency, ItemName, TypedValue};
use std::path::PathBuf;

def_builtin!(ctx => {
    let which = ctx.args.next().ok_or("no path subcommand specified")?;

    let which = script_value_as!(ctx, which, ScriptString, "path subcommand must be a string");

    match which.as_ref().as_ref() {
        "new" => path_new(ctx),
        "join" => path_join(ctx),
        "relative" => path_relative(ctx),
        "split" => path_split(ctx),
        cmd => Err(format!("unrecognized subcommand: {}", cmd).into())
    }
});

fn path_new(ctx: &mut Context<FunctionContext>) -> Result<Eval<Value>, Error> {
    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let store = ctx
        .store
        .item(item_name!("path"))
        .item(item_name!("random"));

    Ok(Eval::Value(
        make_value!(["path new"] {
            use rand::random;
            use std::convert::TryInto;
            let s = format!("{:x}", random::<u64>());
            let name: &ItemName = s.as_str().try_into().unwrap();
            Ok(store.item(name).path())
        })
        .into(),
    ))
}

enum JoinComponent {
    String(TypedValue<String>),
    Path(TypedValue<PathBuf>),
}

impl From<&JoinComponent> for Dependency {
    fn from(v: &JoinComponent) -> Dependency {
        match v {
            JoinComponent::String(s) => s.into(),
            JoinComponent::Path(p) => p.into(),
        }
    }
}

fn path_join(ctx: &mut Context<FunctionContext>) -> Result<Eval<Value>, Error> {
    let mut args = Vec::new();

    while let Some(sv) = ctx.args.next() {
        let ret = sv
            .map(|v| {
                match_value!(v => {
                    ScriptString => |s| Ok(JoinComponent::String(s)),
                    PathBuf => |s| Ok(JoinComponent::Path(s)),
                    => |_| Err("all arguments must be strings or paths".to_owned())
                })
            })
            .transpose_err();
        args.push(match ret {
            Err(e) => {
                ctx.error(e);
                Eval::Error
            }
            Ok(v) => Eval::Value(v),
        })
    }

    let args: Eval<Vec<_>> = args.into_iter().collect();

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let args = match args {
        Eval::Error => return Ok(Eval::Error),
        Eval::Value(v) => v,
    };

    if args.is_empty() {
        return Err("at least one path component is required".into());
    }

    Ok(Eval::Value(
        make_value!(["path join", ^args] {
            let mut path = PathBuf::new();
            for a in args {
                match a {
                    JoinComponent::String(s) => path.push(s.await?.as_ref()),
                    JoinComponent::Path(p) => path.push(p.await?.as_ref()),
                }
            }
            Ok(path)
        })
        .into(),
    ))
}

fn path_split(ctx: &mut Context<FunctionContext>) -> Result<Eval<Value>, Error> {
    let to_split = ctx.args.next().ok_or("no path to split")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let to_split = eval_error!(
        ctx,
        to_split
            .map(|v| v
                .typed::<PathBuf>()
                .map_err(|_| "can only split path types"))
            .transpose_err()
    );

    Ok(Eval::Value(make_value!(["path split", to_split] {
        let mut vals: Vec<Value> = Vec::new();
        for c in to_split.clone().await?.iter() {
            match c.to_str() {
                Some(s) => vals.push(
                        TypedValue::constant_deps(s.to_owned(), depends!["path split", to_split, vals.len()]).into()
                    ),
                None => return Err("could not convert to path components (due to invalid component unicode)".into()),
            }
        }
        Ok(ScriptArray(vals))
    }).into()))
}

fn path_relative(ctx: &mut Context<FunctionContext>) -> Result<Eval<Value>, Error> {
    let base = ctx.args.next().ok_or("no base path")?;
    let path = ctx.args.next().ok_or("no path")?;

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let base = eval_error!(
        ctx,
        base.map(|v| v.typed::<PathBuf>().map_err(|_| "base must be a path"))
            .transpose_err()
    );
    let path = eval_error!(
        ctx,
        path.map(|v| v.typed::<PathBuf>().map_err(|_| "not a path"))
            .transpose_err()
    );

    let task = ctx.task.clone();
    Ok(Eval::Value(make_value!(["path relative", base, path] {
        let (base,path) = task.join(base,path).await?;
        path.strip_prefix(base.as_ref()).map(|p| p.to_owned()).map_err(|e| e.into())
    }).into()))
}
