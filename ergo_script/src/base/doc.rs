//! Doc-related functions.

use ergo_runtime::{
    depends, err_return_value, metadata::Doc, nsid, traits, types, value::match_value, Context,
    Error, Result, Source, Value,
};
use futures::future::FutureExt;
use std::path::PathBuf;

/*
#[derive(Clone, StableAbi)]
#[repr(C)]
struct DocPath {
    root: PathBuf,
    relative: PathBuf,
}

impl TaskLocal for DocPath {
    fn task_local_key() -> u128 {
        task_local_key!(ergo::doc::path)
    }
}

impl DocPath {
    pub fn new(root: PathBuf) -> Self {
        DocPath {
            root,
            relative: Default::default(),
        }
    }

    pub fn join(&mut self, p: &std::path::Path) {
        self.relative = self.relative.as_ref().join(p).into();
    }

    pub fn current(&self) -> PathBuf {
        self.root.as_ref().join(self.relative.as_ref()).into()
    }
}
*/

fn doc_root_key() -> Value {
    types::String::from("doc:root").into()
}

fn doc_path_key() -> Value {
    types::String::from("doc:path").into()
}

struct DocPath {
    pub root: Source<PathBuf>,
    pub path: Source<PathBuf>,
}

impl DocPath {
    pub async fn get(ctx: &Context) -> Result<Option<Self>> {
        match ctx.dynamic_scope.get(&doc_root_key()).cloned() {
            None => Ok(None),
            Some(v) => {
                let root: Source<PathBuf> = ctx
                    .eval_as::<types::Path>(v)
                    .await?
                    .map(|v| v.to_owned().0.into());
                let path = if let Some(path) = ctx.dynamic_scope.get(&doc_path_key()).cloned() {
                    ctx.eval_as::<types::Path>(path)
                        .await?
                        .map(|p| p.to_owned().0.into())
                } else {
                    root.source().with(Default::default())
                };
                Ok(Some(DocPath { root, path }))
            }
        }
    }

    pub fn new(root: Source<PathBuf>) -> Self {
        let path = root.source().with(Default::default());
        DocPath { root, path }
    }

    pub fn join(&mut self, p: Source<&std::path::Path>) {
        let (src, p) = p.take();
        self.path = src.with(self.path.as_ref().join(p));
    }

    pub fn current(&self) -> PathBuf {
        self.root.as_ref().join(self.path.as_ref().unwrap())
    }

    pub fn context(&self, ctx: &Context, key_source: Source<()>) -> Context {
        let root = self
            .root
            .source()
            .with(types::Path::from(self.root.as_path()).into());
        let path = self
            .path
            .source()
            .with(types::Path::from(self.path.as_path()).into());
        ctx.with_dynamic_binding(key_source.clone().with(doc_root_key().clone()), root)
            .with_dynamic_binding(key_source.with(doc_path_key().clone()), path)
    }
}

/// The doc function, supporting a number of indexed functions as well.
pub fn doc() -> Value {
    let path: Value = types::ergo_fn_value! {
        /// Get the current documentation path, if any.
        ///
        /// Arguments: (none)
        ///
        /// Returns the doc Path, or Unset if no Path is present (documentation is not being
        /// written to the filesystem).
        async fn path() -> Value {
            match err_return_value!(DocPath::get(CONTEXT).await) {
                None => Error::from("no doc root set").into(),
                Some(p) => types::Path::from(p.current()).into()
            }
        }
    };

    let write: Value = types::ergo_fn_value! {
        /// Write documentation to the given path.
        ///
        /// Arguments: `(StringOrPath :path) :doc-value`
        ///
        /// Returns the `Path` to the written documentation.
        async fn write(path: _, value: _) -> Value {
            let (path_source, path) = path.take();
            let mut doc_path = match_value!{path,
                types::String(s) => s.as_str().into(),
                types::Path(p) => p.into_pathbuf(),
                v => return traits::type_error(CONTEXT, path_source.with(v), "String or Path").into()
            };

            if let Some(parent) = doc_path.parent() {
                err_return_value!(std::fs::create_dir_all(parent));
            }
            let doc = {
                let doc_ctx = DocPath::new(path_source.with(doc_path.clone())).context(CONTEXT, ARGS_SOURCE);
                Doc::get(&doc_ctx, &value).await
            };

            if doc_path.is_dir() {
                doc_path.push("index.md");
            } else {
                doc_path.set_extension("md");
            }
            err_return_value!(std::fs::write(&doc_path, doc.as_bytes()));
            types::Path::from(doc_path).into()
        }
    };

    let child: Value = types::ergo_fn_value! {
        /// Write child documentation relative to the current documentation path.
        ///
        /// Arguments: `(StringOrPath :path) :doc-value`
        ///
        /// Returns the `Path` to the written documentation. If no documentation path is set, returns `path`
        /// (without writing anything).
        async fn child(path: _, value: _) -> Value {
            let (path_source, path) = path.take();
            let path = match_value!{path,
                types::String(s) => s.as_str().into(),
                types::Path(p) => p.into_pathbuf(),
                v => return traits::type_error(CONTEXT, path_source.with(v), "String or Path").into()
            };

            match err_return_value!(DocPath::get(CONTEXT).await) {
                None => types::Path::from(path).into(),
                Some(mut doc_path) => {
                    let mut rel_path = std::path::PathBuf::new();
                    for component in path.components() {
                        use std::path::Component::*;
                        match component {
                            Prefix(_) | RootDir | ParentDir => return path_source.with("invalid components specified (may only be relative descendant paths)").into_error().into(),
                            CurDir => (),
                            Normal(c) => rel_path.push(c),
                        }
                    }
                    let old_doc_path = doc_path.current();
                    doc_path.join(path_source.with(&rel_path));
                    let new_path = doc_path.current();
                    if let Some(parent) = new_path.parent() {
                        err_return_value!(std::fs::create_dir_all(parent));
                    }
                    let doc = {
                        let doc_ctx = doc_path.context(CONTEXT, ARGS_SOURCE);
                        Doc::get(&doc_ctx, &value).await
                    };

                    if new_path.is_dir() {
                        rel_path.push("index.md");
                    } else {
                        rel_path.set_extension("md");
                    }
                    let output_file = old_doc_path.join(&rel_path);
                    err_return_value!(std::fs::write(&output_file, doc.as_bytes()));
                    types::Path::from(rel_path).into()
                }
            }
        }
    };

    types::Unbound::new(
        move |ctx, v| {
            let path = path.clone();
            let write = write.clone();
            let child = child.clone();
            async move {
                let (source, v) = v.take();
                match_value!{v,
                    types::Args { mut args } => {
                        let to_doc = err_return_value!(args.next().ok_or("no value to document"));
                        err_return_value!(args.unused_arguments());

                        types::String::from(Doc::get(ctx, &to_doc).await).into()
                    },
                    types::Index(ind) => {
                        let (src, ind) = err_return_value!(ctx.eval_as::<types::String>(ind).await).take();
                        let s = ind.as_ref().as_str();
                        if s == "path" {
                            path
                        } else if s == "child" {
                            child
                        } else if s == "write" {
                            write
                        } else {
                            src.with("unknown index").into_error().into()
                        }
                    },
                    v => traits::type_error(ctx, source.with(v), "function call or index").into()
                }
            }
            .boxed()
        },
        depends![nsid!(ergo::doc)],
            "Get the documentation for a value.

Arguments: `:value`

Returns the documentation string.

## Functions
* `child` - Write documentation to a relative path.
* `path` - Get the documentation output path, if set.
* `write` - Write documentation to the given output path."
    )
    .into()
}
