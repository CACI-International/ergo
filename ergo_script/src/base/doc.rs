//! Doc-related functions.

use ergo_runtime::{
    context::{DynamicScopeKey, DynamicScopeRef},
    depends,
    error::DiagnosticInfo,
    metadata::{self, Doc, DocValueKey},
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
        nsid!(doc::path).as_u128()
    }

    fn value_id(value: &DocPath) -> u128 {
        let mut h = ergo_runtime::hash::HashFn::default();
        use std::hash::Hash;
        value.root.hash(&mut h);
        value.path.hash(&mut h);
        h.finish_ext()
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

fn write_doc(path: &std::path::Path, md: &str) -> Result<(), String> {
    use horrorshow::{helper::doctype, html, prelude::*};

    struct Markdown<'a>(&'a str);

    impl<'a> RenderOnce for Markdown<'a> {
        fn render_once(self, tmpl: &mut TemplateBuffer) {
            use pulldown_cmark::{html, Parser};
            let p = Parser::new(self.0);
            let mut s = String::new();
            html::push_html(&mut s, p);
            tmpl.write_raw(&s);
        }
    }

    let mut f = std::io::BufWriter::new(std::fs::File::create(path).map_err(|e| e.to_string())?);
    let template = html! {
        : doctype::HTML;
        html {
            head {
                title: "docs";
            }
            body {
                : Markdown(md);
            }
        }
    };
    template.write_to_io(&mut f).map_err(|e| e.to_string())
}

/// The doc function, supporting a number of indexed functions as well.
pub fn doc() -> Value {
    let value: Value = types::ergo_fn_value! {
        /// Get the value currently being documented, if any.
        ///
        /// Arguments: `()`
        ///
        /// Returns the value being documented, or Unset if no value is being documented.
        async fn value(_: types::Unit) -> Value {
            Context::with(|ctx| ctx.dynamic_scope.get(&DocValueKey).map(|v| v.as_ref().clone()))
                .unwrap_or(types::Unset.into())
        }
    };

    let path: Value = types::ergo_fn_value! {
        /// Get the current documentation path, if any.
        ///
        /// Arguments: `()`
        ///
        /// Returns the doc Path, or Unset if no Path is present (documentation is not being
        /// written to the filesystem).
        async fn path(_: types::Unit) -> Value {
            match DocPath::get() {
                None => types::Unset.into(),
                Some(p) => types::Path::from(p.current()).into()
            }
        }
    };

    let write: Value = types::ergo_fn_value! {
        /// Write documentation to the given path.
        ///
        /// Arguments: `(Into<Path> :path) :doc-value`
        ///
        /// Returns the `Path` to the written documentation.
        async fn write(path: _, value: _) -> Value {
            let path_source = metadata::Source::get(&path);
            let mut doc_path = traits::into::<types::Path>(path).await?.to_owned().into_pathbuf();

            if let Some(parent) = doc_path.parent() {
                std::fs::create_dir_all(&parent)
                    .add_primary_label(path_source.with("while creating directory from this value"))
                    .add_note(format_args!("directory was {}", parent.display()))?;
            }
            let doc = Context::fork(
                    |ctx| DocPath::new(path_source.with(doc_path.clone())).context(ctx, ARGS_SOURCE),
                    async move { Doc::get(&value).await }
                ).await?;

            if doc_path.is_dir() {
                doc_path.push("index");
            }
            doc_path.set_extension("html");
            write_doc(&doc_path, &doc)
                .add_primary_label(path_source.with("while writing to path from this value"))
                .add_note(format_args!("path was {}", doc_path.display()))?;
            types::Path::from(doc_path).into()
        }
    };

    let child: Value = types::ergo_fn_value! {
        /// Write child documentation relative to the current documentation path.
        ///
        /// Arguments: `(Into<Path> :path) :doc-value`
        ///
        /// Returns the `Path` to the written documentation. If no documentation path is set, returns `path`
        /// (without writing anything).
        async fn child(path: _, value: _) -> Value {
            let path_source = metadata::Source::get(&path);
            let path = traits::into::<types::Path>(path).await?.to_owned().into_pathbuf();

            match DocPath::owned() {
                None => types::Path::from(path).into(),
                Some(mut doc_path) => {
                    let mut rel_path = std::path::PathBuf::new();
                    for component in path.components() {
                        use std::path::Component::*;
                        match component {
                            Prefix(_) | RootDir | ParentDir => Err(ergo_runtime::error! {
                                labels: [ primary(path_source.with("in this path")) ],
                                error: "invalid components specified (may only be relative descendant paths)"
                            })?,
                            CurDir => (),
                            Normal(c) => rel_path.push(c),
                        }
                    }
                    let old_doc_path = doc_path.current();
                    doc_path.join(path_source.with(&rel_path));
                    let new_path = doc_path.current();
                    if let Some(parent) = new_path.parent() {
                        std::fs::create_dir_all(&parent)
                            .add_primary_label(path_source.with("while creating directory from this value"))
                            .add_note(format_args!("directory was {}", parent.display()))?;
                    }
                    let doc = Context::fork(
                            move |ctx| doc_path.context(ctx, ARGS_SOURCE),
                            async move { Doc::get(&value).await }
                        ).await?;

                    if new_path.is_dir() {
                        rel_path.push("index");
                    }
                    rel_path.set_extension("html");
                    let output_file = old_doc_path.join(&rel_path);
                    write_doc(&output_file, &doc)
                        .add_primary_label(path_source.with("while writing to path from this value"))
                        .add_note(format_args!("path was {}", output_file.display()))?;
                    types::Path::from(rel_path).into()
                }
            }
        }
    };

    types::Unbound::new(
        move |v| {
            let value = value.clone();
            let path = path.clone();
            let write = write.clone();
            let child = child.clone();
            let source = metadata::Source::get(&v);
            async move {
                match_value! {v,
                    types::Args { mut args } => {
                        let to_doc = try_result!(args.next_or_error("value to document", source));
                        try_result!(args.unused_arguments());

                        types::String::from(try_result!(Doc::get(&to_doc).await)).into()
                    },
                    types::Index(ind) => {
                        let ind = try_result!(Context::eval_as::<types::String>(ind).await);
                        let s = ind.as_ref().as_str();
                        if s == "value" {
                            value
                        } else if s == "path" {
                            path
                        } else if s == "child" {
                            child
                        } else if s == "write" {
                            write
                        } else {
                            metadata::Source::get(&ind).with("unknown index").into_error().into()
                        }
                    },
                    v => traits::type_error(v, "function call or index").into_error().into()
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
* `path` - Get the documentation output path, if any.
* `value` - Get the value being documented, if any.
* `write` - Write documentation to the given output path.",
    )
    .into()
}
