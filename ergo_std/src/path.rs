//! Path operations module.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::{
    depends, item_name, make_value, match_value,
    path::PathBuf,
    runtime::ItemName,
    value::{Dependency, TypedValue, Value},
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of path manipulation functions:"
        "new": "Create a new, unique, and non-existent path." = new_fn(),
        "join": "Join path components into a path." = join_fn(),
        "parent": "Get the parent of a path." = parent_fn(),
        "relative": "Get a path relative to another." = relative_fn(),
        "split": "Split a path into components." = split_fn()
    }
}

fn new_fn() -> Value {
    ergo_function!(std::path::new,
    r"Create a new, unique, non-existent path.

Arguments: (none)

Returns a new Path which has a fixed identity (all calls to this function return different paths but the same identity,
since the returned value is semantically identified as a new path; the actual path doesn't matter.

This is often used for intermediate values where external programs generate the values as files.",
    |ctx| {
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
    ergo_function!(
        std::path::join,
        r"Join components into a Path.

Arguments: [component: Path-or-String...]

Return a Path that is the result of joining the individual path components together.",
        |ctx| {
            let mut args = Vec::new();

            let mut deps = depends![];

            while let Some(sv) = ctx.args.next() {
                deps += depends![*sv];
                args.push(
                    sv.map_async(|v| async move {
                        match_value!(peek v => {
                            types::String => |s| JoinComponent::String(s),
                            PathBuf => |s| JoinComponent::Path(s),
                            => |_| Err("all arguments must be strings or paths")?
                        })
                        .await
                    })
                    .await
                    .transpose()
                    // Change Source<impl Future<Result>> to impl Future<Result>
                    .map(|source_fut| {
                        futures::future::FutureExt::map(source_fut, |source_res| {
                            source_res
                                .transpose_err()
                                .map_err(|e| e.into_grease_error())
                        })
                    })
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

            let task = ctx.task.clone();

            make_value!([^deps] {
                let mut path = std::path::PathBuf::new();
                let args = task.join_all(args).await?;
                for a in args {
                    match a {
                        JoinComponent::String(s) => path.push(s.await?.as_ref().as_str()),
                        JoinComponent::Path(p) => path.push(p.await?.as_ref().as_ref()),
                    }
                }
                Ok(PathBuf::from(path))
            })
            .into()
        }
    )
    .into()
}

fn split_fn() -> Value {
    ergo_function!(std::path::split,
    r"Split a path into its components.

Arguments: <Path>

Returns an Array of Strings, where each element in the array is a (in-order, from least to most specific) component of
the argument.",
    |ctx| {
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
    ergo_function!(std::path::parent,
    r"Return the parent path of the given path.

Arguments: <Path>
Fails if the given path does not have a parent (is a root).",
    |ctx| {
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
    ergo_function!(std::path::relative,
    r"Get a relative path.

Arguments: <base: Path> <child: Path>

Returns a path that is composed of the components of `child` that are children of `base`.",
    |ctx| {
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

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn join(t) {
            let p = std::path::PathBuf::from("a");
            t.assert_value_eq("self:path:join a b c", &super::PathBuf::from(p.join("b").join("c")));
        }
    }

    ergo_script::test! {
        fn parent(t) {
            t.assert_content_eq("self:path:parent (self:path:join a b c)", "self:path:join a b");
        }
    }

    ergo_script::test! {
        fn relative(t) {
            t.assert_content_eq("self:path:relative (self:path:join a b) (self:path:join a b c d)", "self:path:join c d");
        }
    }

    ergo_script::test! {
        fn split(t) {
            t.assert_content_eq("self:path:split (self:path:join a b c)", "[a,b,c]");
        }
    }
}
