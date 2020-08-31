//! Path operations module.

use grease::{
    bst::BstMap,
    depends, item_name, make_value, match_value,
    path::PathBuf,
    runtime::ItemName,
    value::{Dependency, TypedValue, Value},
};
use so_runtime::types;

pub fn module() -> Value {
    let mut map = BstMap::new();
    map.insert("new".into(), new_fn());
    map.insert("join".into(), join_fn());
    map.insert("relative".into(), relative_fn());
    map.insert("split".into(), split_fn());
    types::Map(map).into()
}

fn new_fn() -> Value {
    types::Function::new(|ctx| {
        ctx.unused_arguments()?;

        let store = ctx.store.item(item_name!("path")).item(item_name!("new"));

        Ok(ctx.call_site.clone().with(
            make_value!(["path new"] {
                use rand::random;
                use std::convert::TryInto;
                let s = format!("{:x}", random::<u64>());
                let name: &ItemName = s.as_str().try_into().unwrap();
                Ok(PathBuf::from(store.item(name).path()))
            })
            .into(),
        ))
    })
    .into()
}

enum JoinComponent {
    String(TypedValue<types::String>),
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

fn join_fn() -> Value {
    types::Function::new(|ctx| {
        let mut args = Vec::new();

        while let Some(sv) = ctx.args.next() {
            args.push(
                sv.map(|v| {
                    match_value!(v => {
                        types::String => |s| Ok(JoinComponent::String(s)),
                        PathBuf => |s| Ok(JoinComponent::Path(s)),
                        => |_| Err("all arguments must be strings or paths")
                    })
                })
                .transpose_err()
                .map_err(|e| e.into_grease_error())?,
            );
        }

        ctx.unused_arguments()?;

        if args.is_empty() {
            return Err(ctx
                .call_site
                .clone()
                .with("at least one path component is required")
                .into_grease_error());
        }

        let deps = depends![^@args];

        Ok(ctx.call_site.clone().with(
            make_value!(["path join", ^deps] {
                let mut path = std::path::PathBuf::new();
                for a in args {
                    match a {
                        JoinComponent::String(s) => path.push(s.await?.as_ref().as_str()),
                        JoinComponent::Path(p) => path.push(p.await?.as_ref().as_ref()),
                    }
                }
                Ok(PathBuf::from(path))
            })
            .into(),
        ))
    })
    .into()
}

fn split_fn() -> Value {
    types::Function::new(|ctx| {
        let to_split = ctx.args.next().ok_or("no path to split")?;

        ctx.unused_arguments()?;

        let to_split = to_split
                .map(|v| v
                    .typed::<PathBuf>()
                    .map_err(|_| "can only split path types"))
                .transpose_err()
                .map_err(|e| e.into_grease_error())?;

        Ok(ctx.call_site.clone().with(make_value!(["path split", to_split] {
            let mut vals: Vec<Value> = Vec::new();
            for c in to_split.clone().await?.owned().into_pathbuf().iter() {
                match c.to_str() {
                    Some(s) => vals.push(
                            TypedValue::constant_deps(types::String::from(s.to_owned()),
                            depends!["path split", to_split, vals.len()]).into()
                        ),
                    None => return Err("could not convert to path components (due to invalid component unicode)".into()),
                }
            }
            Ok(types::Array(vals.into()))
        }).into()))
    }).into()
}

fn relative_fn() -> Value {
    types::Function::new(|ctx| {
        let base = ctx.args.next().ok_or("no base path")?;
        let path = ctx.args.next().ok_or("no path")?;

        ctx.unused_arguments()?;

        let base = base.map(|v| v.typed::<PathBuf>().map_err(|_| "base must be a path"))
                .transpose_err()
                .map_err(|e| e.into_grease_error())?;
        let path = path.map(|v| v.typed::<PathBuf>().map_err(|_| "not a path"))
                .transpose_err()
                .map_err(|e| e.into_grease_error())?;

        let task = ctx.task.clone();
        Ok(ctx.call_site.clone().with(
            make_value!(["path relative", base, path] {
                let (base,path) = task.join(base,path).await?;
                path.as_ref().as_ref()
                    .strip_prefix(base.as_ref().as_ref()).map(|p| PathBuf::from(p.to_owned())).map_err(|e| e.into())
            })
            .into(),
        ))
    }).into()
}
