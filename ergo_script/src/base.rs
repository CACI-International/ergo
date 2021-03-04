//! Base environment.

use super::runtime::Error;
use crate::constants::{DIR_NAME, EXTENSION, PLUGIN_ENTRY, WORKSPACE_NAME};
use abi_stable::{
    external_types::RMutex,
    std_types::{ROption, RResult},
    StableAbi,
};
use ergo_runtime::{
    metadata::Doc, namespace_id, source::FileSource, traits, types, ContextExt, EvalResult,
    Runtime, Source,
};
use futures::FutureExt;
use grease::{
    depends, make_value, match_value,
    path::PathBuf,
    runtime::TaskLocal,
    task_local_key,
    types::GreaseType,
    value::{IntoValue, Value},
};
use libloading as dl;
use std::collections::BTreeMap;
use std::path;
use std::sync::Arc;

/// Documentation for the load function.
pub const LOAD_DOCUMENTATION: &'static str = r"Load a script, with optional additional arguments with which to call the result.

Arguments: `(StringOrPath :to-load) ^:call-args`

## Script resolution
When loading a script, the following resolution process occurs for the first argument (if present):
1. Filesystem Name Resolution
   a. If the passed script is an existing path in one of the load path directories, it is used.
   b. If the passed script with the `.ergo` extension appended is an existing path in one of the
     load path directories, it is used.

   The load path is checked from first to last, and is determined by the location of `ergo` and the currently-executing
   script. By default, the load path contains the directory containing the currently-executing script (or if there is no
   script, the current working directory), followed by user and system directories.
2. Filesystem Directory Resolution
   a. If the name-resolved script exists as a file, it is used.
   b. If the name-resolved script exists as a directory, and the directory contains `dir.ergo`,
      that path is used and step (2) is repeated.
   c. If the name-resolved script exists as a directory, and the directory contains `workspace.ergo`,
      that path is used and step (2) is repeated.

If the directory-resolved script exists as a file, it is loaded. If additional arguments were
provided, the resulting value is called with them.";

#[derive(Clone)]
pub struct LoadData {
    pub loading: Arc<RMutex<Vec<path::PathBuf>>>,
    pub load_cache: Arc<RMutex<BTreeMap<path::PathBuf, EvalResult>>>,
}

impl LoadData {
    fn new() -> Self {
        LoadData {
            loading: Arc::new(RMutex::new(Default::default())),
            load_cache: Arc::new(RMutex::new(Default::default())),
        }
    }

    /// Load a script at the given path.
    ///
    /// The path should already be verified as an existing file.
    pub async fn load_script(&self, ctx: &mut Runtime, path: &path::Path) -> EvalResult {
        // Check whether path is already being loaded.
        for l_path in self.loading.lock().iter() {
            if l_path == path {
                return Err(Error::LoadFailed {
                    was_loading: true,
                    load_path: path.to_owned(),
                }
                .into());
            }
        }

        debug_assert!(path.is_file());
        let path = path.canonicalize().unwrap(); // unwrap because is_file() should guarantee that canonicalize will succeed.

        // FIXME if multiple threads are calling load, they may load the same script more than
        // once. This could be changed into a cache of futures that yield the resulting value,
        // though we need to clone contexts and the like. This was already attempted once but some
        // recursive type errors also came up with async blocks.
        let cache = self.load_cache.clone();

        // Since we are in an async function, we need to access the cache in a somewhat odd
        // pattern.
        let cached = {
            let cache = cache.lock();
            let p = path.to_owned();
            let ret = cache.get(&p).cloned();
            if ret.is_none() {
                // Exclude path from loading in nested calls.
                self.loading.lock().push(p);
            }
            ret
        };

        match cached {
            None => {
                let result = if !is_plugin(&path) {
                    let mut script = super::Script::load(Source::new(FileSource(path.clone())))?;
                    script.file_path(path.clone());
                    script.evaluate(ctx).await
                } else {
                    let lib = dl::Library::new(&path)?;
                    let f: dl::Symbol<
                        extern "C" fn(
                            ergo_runtime::plugin::Context,
                            &mut Runtime,
                        )
                            -> RResult<Source<Value>, grease::Error>,
                    > = unsafe { lib.get(PLUGIN_ENTRY.as_bytes()) }?;
                    let result = f(ergo_runtime::plugin::Context::get(), ctx);
                    // Leak loaded libraries rather than storing them and dropping them in
                    // the context, as this can cause issues if the thread pool hasn't shut
                    // down.
                    // ctx.lifetime(lib);
                    std::mem::forget(lib);
                    result.into()
                };

                let cache = self.load_cache.clone();
                let mut cache = cache.lock();
                cache.insert(path.into(), result.clone());
                self.loading.lock().pop();
                result
            }
            Some(v) => v,
        }
    }
}

pub struct LoadFunctions {
    pub load: Value,
    pub std: Value,
    pub workspace: Value,
    pub load_data: LoadData,
}

/// Return the load functions (which are all created with a shared cache).
pub fn load_functions() -> LoadFunctions {
    let load_data = LoadData::new();

    let ld = load_data.clone();
    let load = types::Unbound::new(
        move |ctx, v| {
            let ld = ld.clone();
            async move {
                let (source, args_val) = ctx.source_value_as::<types::Args>(v).await?.take();
                let mut args = args_val.await?.owned().args;

                let target = args.next().ok_or("no load target")?;

                let (target_source, target) = target.take();
                let target: std::path::PathBuf = match_value!(target => {
                    types::String => |v| <&str>::from(v.await?.as_ref()).into(),
                    PathBuf => |v| v.await?.owned().into(),
                    => |v| traits::type_error(ctx, target_source.with(v), "String or Path").await?
                })
                .await?;

                // Try to find target in the load path.
                let target = match resolve_script_path(ctx, &target) {
                    Some(path) => path,
                    None => {
                        return Err(Error::LoadFailed {
                            was_loading: false,
                            load_path: target,
                        }
                        .into());
                    }
                };

                // Load if some module was found.
                let loaded = ld.load_script(ctx, &target).await?;

                let loaded_context = source
                    .clone()
                    .with(format!("loaded from '{}'", target.display()));

                // If there are remaining arguments apply them immediately.
                if !args.is_empty() {
                    traits::bind(ctx, loaded, source.with(types::Args { args }.into_value())).await
                } else {
                    args.unused_arguments()?;
                    Ok(loaded)
                }
                .map(|v| loaded_context.imbue_error_context(v.unwrap()))
            }
            .boxed()
        },
        depends![namespace_id!(ergo::load)],
        LOAD_DOCUMENTATION,
    )
    .into();

    let ld = load_data.clone();
    let std = types::Unbound::new(
        move |ctx, v| {
            let ld = ld.clone();
            async move {
                let path = match resolve_script_path(ctx, "std".as_ref()) {
                    Some(path) => path,
                    None => {
                        return Err(Error::LoadFailed {
                            was_loading: false,
                            load_path: "std".into(),
                        }
                        .into());
                    }
                };

                let lib = ld.load_script(ctx, &path).await?;

                let (src, v) = v.take();
                match_value!(v => {
                    types::Args => |args| {
                        let args = args.await?.owned().args;
                        if args.is_empty() {
                            // If called with no args, return the loaded library.
                            lib.unwrap()
                        } else {
                            traits::bind(ctx, lib, src.with(types::Args { args }.into())).await.map(Source::unwrap)?
                        }
                    }
                    => |v| traits::bind(ctx, lib, src.with(v)).await.map(Source::unwrap)?
                }).await
            }
            .boxed()
        },
        depends![namespace_id!(ergo::std)],
        "Get the value as if `ergo std` was run, and apply any bindings to it. Return the library if called with no arguments."
    )
    .into();

    let ld = load_data.clone();
    let workspace = types::Unbound::new(
        move |ctx, v| {
            let ld = ld.clone();
            async move {
                // If the current file is a workspace, allow it to load from parent workspaces.
                let (path_basis, check_for_workspace) = if let ROption::RSome(v) = &ctx.mod_path {
                    (v.clone().into(), true)
                } else {
                    (ctx.mod_dir(), false)
                };

                let resolved = ctx.shared_state(|| Ok(ResolvedWorkspaces::default()))?;

                let path = {
                    let mut guard = resolved.map.lock();
                    match guard.get(&path_basis) {
                        Some(v) => v.clone(),
                        None => {
                            let within_workspace = check_for_workspace && path_basis.file_name().map(|v| v == WORKSPACE_NAME).unwrap_or(false);

                            let mut ancestors = path_basis.ancestors().peekable();
                            if within_workspace {
                                while let Some(v) = ancestors.peek().and_then(|a| a.file_name()) {
                                    if v == WORKSPACE_NAME {
                                        ancestors.next();
                                    } else {
                                        break;
                                    }
                                }
                                // Skip one more to drop parent directory of top-most workspace, which would find
                                // the same workspace as the original.
                                ancestors.next();
                            }

                            let result = ancestors.find_map(script_path_exists(WORKSPACE_NAME, false));
                            guard.insert(path_basis, result.clone());
                            result
                        }
                    }
                }.ok_or("no ancestor workspace found")?;

                let lib = ld.load_script(ctx, &path).await?;

                let (src, v) = v.take();
                match_value!(v => {
                    types::Args => |args| {
                        let args = args.await?.owned().args;
                        if args.is_empty() {
                            // If called with no args, return the loaded library.
                            lib.unwrap()
                        } else {
                            traits::bind(ctx, lib, src.with(types::Args { args }.into())).await.map(Source::unwrap)?
                        }
                    }
                    => |v| traits::bind(ctx, lib, src.with(v)).await.map(Source::unwrap)?
                }).await
            }
            .boxed()
        },
        depends![namespace_id!(ergo::workspace)],
        "Get the value as if `ergo path/to/ancestor/workspace.ergo` was run, and apply any bindings to it.
Return the workspace value if called with no arguments.

Note that this only retrieves the active workspace _when bound_, so if you want to use `workspace`
within a function that is to be used outside of the workspace, you should retrieve the workspace
outside of the function and reference that."
    )
    .into();

    LoadFunctions {
        load,
        std,
        workspace,
        load_data,
    }
}

/// A binding function returning an Args.
pub fn pat_args_to_args() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move {
                let args_val = ctx.source_value_as::<types::PatternArgs>(v).await?.unwrap();
                let args = args_val.await?.owned().args;
                Ok(types::Args { args }.into_value())
            }
            .boxed()
        },
        depends![namespace_id!(ergo::fn)],
        "The 'fn' binding function, which takes all PatternArgs and returns an Args to be bound.",
    )
    .into()
}

/// A binding function returning a PatternArgs.
pub fn pat_args_to_pat_args() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move { Ok(ctx.source_value_as::<types::PatternArgs>(v).await?.unwrap().into()) }.boxed()
        },
        depends![namespace_id!(ergo::pat)],
        "The 'pat' binding function, which takes all PatternArgs and returns a PatternArgs to be bound."
    )
    .into()
}

/// A binding function returning an Index.
pub fn pat_args_to_index() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move {
                let args_val = ctx.source_value_as::<types::PatternArgs>(v).await?.unwrap();
                let mut args = args_val.await?.owned().args.checked();
                let ind_v = args.next().ok_or("missing index argument")?;
                args.unused_arguments()?;
                Ok(types::Index(ind_v).into_value())
            }.boxed()
        },
        depends![namespace_id!(ergo::index)],
        "The 'index' binding function, which takes a single argument and returns an Index to be bound."
    )
    .into()
}

#[derive(GreaseType)]
struct ResolvedWorkspaces {
    map: Arc<RMutex<BTreeMap<path::PathBuf, Option<path::PathBuf>>>>,
}

impl Default for ResolvedWorkspaces {
    fn default() -> Self {
        ResolvedWorkspaces {
            map: Arc::new(RMutex::new(Default::default())),
        }
    }
}

// TODO This should probably be scoped in the context itself, not as a task local value.
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

/// The doc function, supporting a number of indexed functions as well.
pub fn doc() -> Value {
    let path: Value = ergo_runtime::ergo_function!(independent ergo::doc::path,
       r"Get the current documentation path, if any.

Arguments: (none)

Returns the doc Path, or Unset if no Path is present (documentation is not being written to the
filesystem).",
    |ctx, args| {
        args.unused_arguments()?;

        match DocPath::task_local().map(|v| v.current()) {
            None => types::Unset::new().into(),
            Some(doc_path) => make_value!([namespace_id!(ergo::doc::path)] Ok(doc_path)).into(),
        }
    })
    .into();

    let write: Value = ergo_runtime::ergo_function!(independent ergo::doc::write,
             r"Write documentation to the given path.

Arguments: `(StringOrPath :path) :doc-value`

Returns the `Path` to the written documentation.",
        |ctx, args| {
            let path = args.next().ok_or("no path provided")?;
            let value = args.next().ok_or("no value provided")?.unwrap();

            args.unused_arguments()?;

            let path = path.map_async(|v|
                match_value!(v => {
                    types::String => |s| s.map(|v| PathBuf::from(std::path::PathBuf::from(v.owned().to_string()))),
                    PathBuf => |p| p,
                    => |_| Err("argument must be a string or path")?
                })
            ).await.transpose_err().map_err(|e| e.into_grease_error())?;

            let mut ctx = ctx.empty();
            make_value!([path, value] {
                let path = path.await?.owned();
                let mut doc_path = path.clone().into_pathbuf();
                if let Some(parent) = doc_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                let doc = DocPath::new(path).scoped(async {
                   Doc::get(&mut ctx, &value).await?.await
                }).await?;
                if doc_path.is_dir() {
                    doc_path.push("index.md");
                } else {
                    doc_path.set_extension("md");
                }
                std::fs::write(&doc_path, doc.0.as_str().as_bytes())?;
                Ok(PathBuf::from(doc_path))
            }).into()
        }).into();

    let child: Value = ergo_runtime::ergo_function!(independent ergo::doc::child,
             r"Write child documentation relative to the current documentation path.

Arguments: `(StringOrPath :path) :doc-value`

Returns the `Path` to the written documentation. If no documentation path is set, returns `path`
(without writing anything).",
        |ctx, args| {
            let path = args.next().ok_or("no path provided")?;
            let value = args.next().ok_or("no value provided")?;

            args.unused_arguments()?;

            let (path_source, path) = path.map_async(|v|
                match_value!(v => {
                    types::String => |s| s.map(|v| PathBuf::from(std::path::PathBuf::from(v.owned().to_string()))),
                    PathBuf => |p| p,
                    => |_| Err("argument must be a string or path")?
                })
            ).await.transpose().map_err(|e| e.into_grease_error())?.take();
            let doc = Doc::get(ctx, &value).await?;

            match DocPath::task_local() {
               None => path.into(),
               Some(local_doc_path) => {
                  make_value!([path, doc] {
                     let path = path.await?;
                     let doc_path = local_doc_path.current().into_pathbuf();
                     let mut rel_path = std::path::PathBuf::new();
                     for component in path.as_ref().as_ref().components() {
                        use std::path::Component::*;
                        match component {
                           Prefix(_) | RootDir | ParentDir => return Err(path_source.with("invalid components specified (may only be relative descendant paths)").into_grease_error()),
                           CurDir => (),
                           Normal(c) => rel_path.push(c),
                        }
                     }
                     let new_doc_path = doc_path.join(&rel_path);
                     if let Some(parent) = new_doc_path.parent() {
                        std::fs::create_dir_all(parent)?;
                     }
                     let mut local_doc_path = local_doc_path.as_ref().clone();
                     local_doc_path.join(&rel_path);
                     let doc_val = local_doc_path.scoped(doc).await?;
                     if new_doc_path.is_dir() {
                        rel_path.push("index.md");
                     } else {
                        rel_path.set_extension("md");
                     }
                     let output_file = doc_path.join(&rel_path);
                     std::fs::write(&output_file, doc_val.0.as_str().as_bytes())?;
                     Ok(PathBuf::from(rel_path))
                  }).into()
               }
            }
        }).into();

    types::Unbound::new(
        move |ctx, v| {
            let path = path.clone();
            let write = write.clone();
            let child = child.clone();
            async move {
                let (source, v) = v.take();
                match_value!(peek v => {
                    types::Args => |args| {
                        let mut args = args.await?.owned().args;
                        let to_doc = args.next().ok_or("no argument to doc")?;
                        args.unused_arguments()?;

                        Doc::get(ctx, &to_doc).await?.into()
                    },
                    types::Index => |index| {
                        let ind = index.await?.owned().0;
                        let (ind_source, ind) = ctx.source_value_as::<types::String>(ind).await?.take();
                        let ind = ind.await?;
                        if ind.0.as_str() == "path" {
                            path
                        } else if ind.0.as_str() == "child" {
                            child
                        } else if ind.0.as_str() == "write" {
                            write
                        } else {
                            Err(ind_source.with("unrecognized index").into_grease_error())?
                        }
                    },
                    => |v| traits::type_error(ctx, source.with(v), "function call or index").await?
                })
                .await
            }
            .boxed()
        },
        depends![namespace_id!(ergo::doc)],
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

/// Look at the file contents to determine if the file is a plugin (dynamic library).
fn is_plugin(f: &path::Path) -> bool {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(f).expect("could not open file for reading");
    if cfg!(target_os = "macos") {
        let mut magic: [u8; 4] = [0; 4];
        if file.read_exact(&mut magic).is_err() {
            return false;
        }
        return &magic == &[0xfe, 0xed, 0xfa, 0xce]
            || &magic == &[0xfe, 0xed, 0xfa, 0xcf]
            || &magic == &[0xcf, 0xfa, 0xed, 0xfe]
            || &magic == &[0xce, 0xfa, 0xed, 0xfe]
            || &magic == &[0xca, 0xfe, 0xba, 0xbe];
    } else if cfg!(target_os = "windows") {
        use std::io::{Seek, SeekFrom};

        // DOS header
        let mut m1: [u8; 2] = [0; 2];
        if file.read_exact(&mut m1).is_err() {
            return false;
        }
        if &m1 != b"MZ" && &m1 != b"ZM" {
            return false;
        }

        // PE header offset
        if file.seek(SeekFrom::Start(0x3c)).is_err() {
            return false;
        }
        let mut offset: [u8; 4] = [0; 4];
        if file.read_exact(&mut offset).is_err() {
            return false;
        }
        let offset = u32::from_ne_bytes(offset);

        // PE header
        if file.seek(SeekFrom::Start(offset as _)).is_err() {
            return false;
        }
        let mut magic: [u8; 4] = [0; 4];
        if file.read_exact(&mut magic).is_err() {
            return false;
        }
        return &magic == b"PE\0\0";
    } else if cfg!(target_os = "linux") {
        let mut magic: [u8; 4] = [0; 4];
        if let Err(_) = file.read_exact(&mut magic) {
            return false;
        }
        return &magic == b"\x7fELF";
    } else {
        panic!("unsupported operating system");
    }
}

fn script_path_exists<'a, P: 'a + AsRef<path::Path>>(
    name: P,
    try_add_extension: bool,
) -> impl FnMut(&path::Path) -> Option<path::PathBuf> + 'a {
    move |path| {
        if try_add_extension {
            if let Some(file_name) = name.as_ref().file_name() {
                let mut p = file_name.to_owned();
                p.push(".");
                p.push(EXTENSION);
                let path_with_extension = path.join(name.as_ref()).with_file_name(p);
                if path_with_extension.exists() {
                    Some(path_with_extension)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
        .or_else(|| {
            let path_exact = path.join(name.as_ref());
            if path_exact.exists() {
                Some(path_exact)
            } else {
                None
            }
        })
        .and_then(|mut p| {
            while p.is_dir() {
                // Prefer dir script to workspace
                let dir = p.join(DIR_NAME);
                if dir.exists() {
                    p = dir;
                } else {
                    let ws = p.join(WORKSPACE_NAME);
                    if ws.exists() {
                        p = ws
                    } else {
                        break;
                    }
                }
            }
            if p.is_file() {
                Some(p)
            } else {
                None
            }
        })
    }
}

/// Resolve a path to the full script path, based on the current load path.
///
/// If resolution succeeds, the returned path will be to a file (not directory).
pub fn resolve_script_path(ctx: &Runtime, path: &path::Path) -> Option<path::PathBuf> {
    ctx.current_load_path
        .iter()
        .map(|p| p.as_ref())
        .find_map(|p| script_path_exists(path, true)(p.as_ref()))
}
