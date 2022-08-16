//! Path operations module.

use ergo_runtime::{
    error::{Diagnostic, DiagnosticInfo},
    metadata::Source,
    traits,
    type_system::ErgoType,
    types, Value,
};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Path::ergo_type(),
        index: crate::make_string_map! {
            "from" = from(),
            "owned" = owned(),
            "join" = join(),
            "name" = name(),
            "parent" = parent(),
            "relative" = relative(),
            "split" = split()
        },
    }
    .into()
}

#[types::ergo_fn]
/// Convert a value into a Path.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    traits::into::<types::Path>(value).await?.into()
}

#[types::ergo_fn]
/// Create an owned Path.
///
/// Arguments: `Into:into $Path |> :path`
///
/// An owned Path is identical to a normal Path, however it has a side effect of deleting the Path
/// (whether a file or directory, if existing at all) at shutdown.
async fn owned(path: _) -> Value {
    let path = traits::into::<types::Path>(path).await?;
    ergo_runtime::Context::global()
        .owned_paths()
        .add(&path.as_ref().path);
    path.into()
}

#[types::ergo_fn]
/// Join components into a Path.
///
/// Arguments: `^((Array:Of Into<Path>) :components)`
///
/// Return a `Path` that is the result of joining the individual path components together.
async fn join(...) -> Value {
    let mut path = std::path::PathBuf::new();

    while let Some(v) = REST.next() {
        path.push(traits::into::<types::Path>(v).await?.as_ref().as_ref());
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
    let mut vals: Vec<Value> = Vec::new();
    for c in path.clone().into_owned().into_pathbuf().iter() {
        match c.to_str() {
            Some(s) => vals.push(Source::imbue(
                ARGS_SOURCE
                    .clone()
                    .with(types::String::from(s.to_owned()).into()),
            )),
            None => Err(Diagnostic::from(
                "could not convert to path components (due to invalid component unicode)",
            )
            .add_primary_label(Source::get(&path).with("")))?,
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
    match path.as_ref().as_ref().parent() {
        None => Source::get(&path)
            .with("path does not have a parent")
            .into_error()
            .into(),
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
    match path.as_ref().as_ref().file_name() {
        None => Source::get(&path)
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
    types::Path::from(
        child
            .as_ref()
            .as_ref()
            .strip_prefix(base.as_ref().as_ref())
            .add_note(format_args!("base path was {}", base.as_ref().display()))
            .add_note(format_args!("child path was {}", child.as_ref().display()))?,
    )
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn join(t) {
            let p = std::path::PathBuf::from("a");
            t.assert_value_eq("self:Path:join a b c", &super::types::Path::from(p.join("b").join("c")));
        }

        fn from(t) {
            t.assert_value_eq("self:Path:from a/b/c", &super::types::Path::from(std::path::PathBuf::from("a").join("b").join("c")));
        }

        fn parent(t) {
            t.assert_eq("self:Path:parent (self:Path:join a b c)", "self:Path:join a b");
        }

        fn relative(t) {
            t.assert_eq("self:Path:relative (self:Path:join a b) (self:Path:join a b c d)", "self:Path:join c d");
        }

        fn split(t) {
            t.assert_eq("self:Path:split (self:Path:join a b c)", "[a,b,c]");
        }
    }
}
