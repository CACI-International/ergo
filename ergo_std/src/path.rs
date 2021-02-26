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
        "join" = join_fn(),
        "name" = name_fn(),
        "new" = new_fn(),
        "parent" = parent_fn(),
        "relative" = relative_fn(),
        "split" = split_fn()
    }
}

fn new_fn() -> Value {
    ergo_function!(std::path::new,
    r"Create a new, unique, non-existent path.

Arguments: (none)

Returns a new Path which has a fixed identity (all calls to this function return different paths but the same identity,
since the returned value is semantically identified as a new path; the actual path doesn't matter.

This is often used for intermediate values where external programs generate the values as files.",
    |ctx, args| {
        args.unused_arguments()?;

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

Arguments: `^(PathOrString :components)`

Return a Path that is the result of joining the individual path components together.",
        |ctx, cargs| {
            let mut args = Vec::new();

            let mut deps = depends![];

            while let Some(sv) = cargs.next() {
                deps += depends![*sv];
                args.push(
                    sv.map_async(|v| async move {
                        match_value!(delay v => {
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

            cargs.unused_arguments()?;

            if args.is_empty() {
                return Err("at least one path component is required".into());
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

Arguments: `(Path :path)`

Returns an `Array` of `String`, where each element in the array is a (in-order, from least to most
specific) component of the argument.",
    |ctx, args| {
        let to_split = args.next().ok_or("no path to split")?;

        args.unused_arguments()?;

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

Arguments: `(Path :path)`
Fails if the given path does not have a parent (is a root).",
    |ctx, args| {
        let path = args.next().ok_or("no path provided")?;

        args.unused_arguments()?;

        let path = ctx.source_value_as::<PathBuf>(path);
        let path = path.await?.unwrap();

        make_value!([path] {
            let path = path.await?;
            Ok(PathBuf::from(path.as_ref().as_ref().parent().ok_or("path does not have a parent")?.to_owned()))
        }).into()
    }).into()
}

fn name_fn() -> Value {
    ergo_function!(
        std::path::name,
        r"Returns the name (final component) of the given path.

Arguments: `(Path :path)`

Returns the component as a String.
Fails if the given path ends in `..`.",
        |ctx, args| {
            let path = args.next().ok_or("no path provided")?;

            args.unused_arguments()?;

            let path = ctx.source_value_as::<PathBuf>(path);
            let path = path.await?.unwrap();

            make_value!([path] {
                let path = path.await?;
                Ok(types::String::from(path.as_ref().as_ref().file_name()
                        .ok_or("path does not have a final component")?
                        .to_string_lossy().to_owned()))
            })
            .into()
        }
    )
    .into()
}

fn relative_fn() -> Value {
    ergo_function!(std::path::relative,
    r"Get a relative path.

Arguments: `(Path :base) (Path :child)`

Returns a path that is composed of the components of `child` that are not components of `base`.",
    |ctx, args| {
        let base = args.next().ok_or("no base path")?;
        let path = args.next().ok_or("no path")?;

        args.unused_arguments()?;

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
