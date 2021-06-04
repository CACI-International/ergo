//! Path operations module.

use ergo_runtime::{
    context::{item_name, ItemName},
    traits, try_result, types,
    value::match_value,
    Source, Value,
};

pub fn module() -> Value {
    crate::make_string_map! {
        "join" = join(),
        "name" = name(),
        "new" = new(),
        "parent" = parent(),
        "relative" = relative(),
        "split" = split()
    }
}

#[types::ergo_fn]
/// Create a new, unique, non-existent path.
///
/// Arguments: (none)
///
/// Returns a new Path which has a fixed identity (all calls to this function return different paths but the same identity,
/// since the returned value is semantically identified as a new path; the actual path doesn't matter).
///
/// This is often used for intermediate values where external programs generate the values as files.
async fn new() -> Value {
    use rand::random;
    use std::convert::TryInto;

    let store = CONTEXT
        .store
        .item(item_name!("path"))
        .item(item_name!("new"));

    let s = format!("{:x}", random::<u64>());
    let name: &ItemName = s.as_str().try_into().unwrap();
    types::Path::from(store.item(name).path()).into()
}

#[types::ergo_fn]
/// Join components into a Path.
///
/// Arguments: `^((Array:Of :StringOrPath) :components)`
///
/// Return a `Path` that is the result of joining the individual path components together.
async fn join(...) -> Value {
    let mut path = std::path::PathBuf::new();

    while let Some(sv) = REST.next() {
        let (src, v) = sv.take();
        match_value! {v,
            types::String(s) => path.push(s.as_str()),
            types::Path(p) => path.push(p.as_ref()),
            v => return traits::type_error(CONTEXT, src.with(v), "String or Path").into()
        }
    }

    types::Path::from(path).into()
}

#[types::ergo_fn]
/// Split a path into its components.
///
/// Arguments: `(Path :path)`
///
/// Returns an `Array` of `String`, where each element in the array is a (in-order, from least to most
/// specific) component of the argument.
async fn split(path: types::Path) -> Value {
    let mut vals: Vec<Source<Value>> = Vec::new();
    for c in path.unwrap().to_owned().into_pathbuf().iter() {
        match c.to_str() {
            Some(s) => vals.push(
                ARGS_SOURCE
                    .clone()
                    .with(types::String::from(s.to_owned()).into()),
            ),
            None => {
                return ARGS_SOURCE
                    .with("could not convert to path components (due to invalid component unicode)")
                    .into_error()
                    .into()
            }
        }
    }
    types::Array(vals.into()).into()
}

#[types::ergo_fn]
/// Return the parent path of the given path.
///
/// Arguments: `(Path :path)`
///
/// Fails if the given path does not have a parent (is a root).
async fn parent(path: types::Path) -> Value {
    let (src, path) = path.take();
    match path.as_ref().as_ref().parent() {
        None => src.with("path does not have a parent").into_error().into(),
        Some(path) => types::Path::from(path).into(),
    }
}

#[types::ergo_fn]
/// Returns the name (final component) of the given path.
///
/// Arguments: `(Path :path)`
///
/// Returns the component as a String.
///
/// Fails if the given path ends in `..`.
async fn name(path: types::Path) -> Value {
    let (src, path) = path.take();
    match path.as_ref().as_ref().file_name() {
        None => src
            .with("path does not have a final component")
            .into_error()
            .into(),
        Some(path) => types::String::from(path.to_string_lossy().to_owned()).into(),
    }
}

#[types::ergo_fn]
/// Get a relative path.
///
/// Arguments: `(Path :base) (Path :child)`
///
/// Returns a Path that is composed of the largest component suffix of `child` that are not
/// components of `base`.
async fn relative(base: types::Path, child: types::Path) -> Value {
    let child = child.unwrap();
    try_result!(child
        .as_ref()
        .as_ref()
        .strip_prefix(base.unwrap().as_ref().as_ref())
        .map(|p| types::Path::from(p).into()))
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn join(t) {
            let p = std::path::PathBuf::from("a");
            t.assert_value_eq("self:path:join a b c", &super::types::Path::from(p.join("b").join("c")));
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
