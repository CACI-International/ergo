//! Path operations module.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::{
    bst::BstMap,
    depends, item_name, make_value, match_value,
    path::PathBuf,
    runtime::ItemName,
    value::{Dependency, TypedValue, Value},
};

pub fn module() -> Value {
    let mut map = BstMap::new();
    map.insert("new".into(), new_fn());
    map.insert("join".into(), join_fn());
    map.insert("parent".into(), parent_fn());
    map.insert("relative".into(), relative_fn());
    map.insert("split".into(), split_fn());
    types::Map(map).into()
}

fn new_fn() -> Value {
    ergo_function!(std::path::new, |ctx| {
        ctx.unused_arguments()?;

        let store = ctx.store.item(item_name!("path")).item(item_name!("new"));

        make_value!({
            use rand::random;
            use std::convert::TryInto;
            let s = format!("{:x}", random::<u64>());
            let name: &ItemName = s.as_str().try_into().unwrap();
            Ok(PathBuf::from(store.item(name).path()))
        })
        .into()
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
    ergo_function!(std::path::join, |ctx| {
        let mut args = Vec::new();

        while let Some(sv) = ctx.args.next() {
            args.push(
                sv.map_async(|mut v| {
                    match_value!(v => {
                        types::String => |s| JoinComponent::String(s),
                        PathBuf => |s| JoinComponent::Path(s),
                        => |_| Err("all arguments must be strings or paths")?
                    })
                })
                .await
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

        make_value!([^deps] {
            let mut path = std::path::PathBuf::new();
            for a in args {
                match a {
                    JoinComponent::String(s) => path.push(s.await?.as_ref().as_str()),
                    JoinComponent::Path(p) => path.push(p.await?.as_ref().as_ref()),
                }
            }
            Ok(PathBuf::from(path))
        })
        .into()
    })
    .into()
}

fn split_fn() -> Value {
    ergo_function!(std::path::split, |ctx| {
        let to_split = ctx.args.next().ok_or("no path to split")?;

        ctx.unused_arguments()?;

        let to_split = ctx.source_value_as::<PathBuf>(to_split);
        let to_split = to_split.await?.unwrap();

        make_value!([to_split] {
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
            }).into()
    }).into()
}

fn parent_fn() -> Value {
    ergo_function!(std::path::parent, |ctx| {
        let path = ctx.args.next().ok_or("no path provided")?;

        ctx.unused_arguments()?;

        let path = ctx.source_value_as::<PathBuf>(path);
        let path = path.await?.unwrap();

        make_value!([path] {
            let path = path.await?;
            Ok(PathBuf::from(path.as_ref().as_ref().parent().ok_or("path does not have a parent")?.to_owned()))
        }).into()
    }).into()
}

fn relative_fn() -> Value {
    ergo_function!(std::path::relative, |ctx| {
        let base = ctx.args.next().ok_or("no base path")?;
        let path = ctx.args.next().ok_or("no path")?;

        ctx.unused_arguments()?;

        let base = ctx.source_value_as::<PathBuf>(base);
        let base = base.await?.unwrap();

        let path = ctx.source_value_as::<PathBuf>(path);
        let path = path.await?.unwrap();

        let task = ctx.task.clone();
        make_value!([base, path] {
            let (base,path) = task.join(base,path).await?;
            path.as_ref().as_ref()
                .strip_prefix(base.as_ref().as_ref()).map(|p| PathBuf::from(p.to_owned())).map_err(|e| e.into())
        }).into()
    }).into()
}
