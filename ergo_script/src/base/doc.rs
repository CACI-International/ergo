//! Doc-related functions.

use ergo_runtime::{
    context::{DynamicScopeKey, DynamicScopeRef},
    depends,
    io::AddContext,
    metadata::{self, Doc},
    nsid, traits, try_result, types,
    value::match_value,
    Context, Source, Value,
};
use futures::future::FutureExt;
use std::path::PathBuf;

struct DocPathKey;

impl DynamicScopeKey for DocPathKey {
    type Value = DocPath;

    fn id(&self) -> u128 {
        nsid!(doc::key).as_u128()
    }
}

#[derive(Clone, Debug)]
struct DocPath {
    pub root: Source<PathBuf>,
    pub path: Source<PathBuf>,
}

impl DocPath {
    pub fn get() -> Option<DynamicScopeRef<Self>> {
        Context::with(|ctx| ctx.dynamic_scope.get(&DocPathKey))
    }

    pub fn owned() -> Option<Self> {
        Self::get().map(|r| r.as_ref().clone())
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

    pub fn context(self, ctx: &mut Context, key_source: Source<()>) {
        ctx.dynamic_scope.set(&key_source.with(DocPathKey), self);
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
            match DocPath::get() {
                None => types::Unset.into(),
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
        async fn write(mut path: _, value: _) -> Value {
            try_result!(Context::eval(&mut path).await);
            let path_source = metadata::Source::get(&path);
            let mut doc_path = match_value!{path,
                types::String(s) => s.as_str().into(),
                types::Path(p) => p.into_pathbuf(),
                v => return traits::type_error(v, "String or Path").into()
            };

            if let Some(parent) = doc_path.parent() {
                try_result!(std::fs::create_dir_all(&parent).add_context_str(parent.display()));
            }
            let doc = try_result!(Context::fork(
                    |ctx| DocPath::new(path_source.with(doc_path.clone())).context(ctx, ARGS_SOURCE),
                    async move { Doc::get(&value).await }
                ).await);

            if doc_path.is_dir() {
                doc_path.push("index.md");
            } else {
                doc_path.set_extension("md");
            }
            try_result!(std::fs::write(&doc_path, doc.as_bytes()).add_context_str(doc_path.display()));
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
        async fn child(mut path: _, value: _) -> Value {
            try_result!(Context::eval(&mut path).await);
            let path_source = metadata::Source::get(&path);
            let path = match_value!{path,
                types::String(s) => s.as_str().into(),
                types::Path(p) => p.into_pathbuf(),
                v => return traits::type_error(v, "String or Path").into()
            };

            match DocPath::owned() {
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
                        try_result!(std::fs::create_dir_all(&parent).add_context_str(parent.display()));
                    }
                    let doc = try_result!(Context::fork(
                            move |ctx| doc_path.context(ctx, ARGS_SOURCE),
                            async move { Doc::get(&value).await }
                        ).await);

                    if new_path.is_dir() {
                        rel_path.push("index.md");
                    } else {
                        rel_path.set_extension("md");
                    }
                    let output_file = old_doc_path.join(&rel_path);
                    try_result!(std::fs::write(&output_file, doc.as_bytes()).add_context_str(output_file.display()));
                    types::Path::from(rel_path).into()
                }
            }
        }
    };

    types::Unbound::new(
        move |v| {
            let path = path.clone();
            let write = write.clone();
            let child = child.clone();
            async move {
                match_value! {v,
                    types::Args { mut args } => {
                        let to_doc = try_result!(args.next().ok_or("no value to document"));
                        try_result!(args.unused_arguments());

                        types::String::from(try_result!(Doc::get(&to_doc).await)).into()
                    },
                    types::Index(ind) => {
                        let ind = try_result!(Context::eval_as::<types::String>(ind).await);
                        let s = ind.as_ref().as_str();
                        if s == "path" {
                            path
                        } else if s == "child" {
                            child
                        } else if s == "write" {
                            write
                        } else {
                            metadata::Source::get(&ind).with("unknown index").into_error().into()
                        }
                    },
                    v => traits::type_error(v, "function call or index").into()
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
* `write` - Write documentation to the given output path.",
    )
    .into()
}
