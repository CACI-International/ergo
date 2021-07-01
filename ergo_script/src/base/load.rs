//! Loading-related functions.

use crate::constants::{DIR_NAME, EXTENSION, PLUGIN_ENTRY, WORKSPACE_NAME};
use ergo_runtime::abi_stable::external_types::RMutex;
use ergo_runtime::{
    depends, error::RResult, metadata, nsid, source::FileSource, traits, try_result,
    type_system::ErgoType, types, value::match_value, Context, Source, Value,
};
use futures::future::FutureExt;
use libloading as dl;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Clone)]
pub struct LoadData {
    // The load cache `Value` is always a dynamic value which (when evaluated once) will do the
    // actual loading of the script/plugin to take advantage of evaluation caching.
    pub load_cache: Arc<RMutex<BTreeMap<PathBuf, Value>>>,
    pub load_path: Arc<Vec<PathBuf>>,
    pub top_level_env: Arc<RMutex<BTreeMap<String, Value>>>,
    pub ast_context: Arc<RMutex<crate::ast::Context>>,
}

impl LoadData {
    fn new(load_path: Vec<PathBuf>) -> Self {
        LoadData {
            load_cache: Arc::new(RMutex::new(BTreeMap::default())),
            load_path: Arc::new(load_path),
            top_level_env: Arc::new(RMutex::new(Default::default())),
            ast_context: Arc::new(RMutex::new(Default::default())),
        }
    }

    /// Reset the inner state of the load data.
    pub fn reset(&self) {
        *self.load_cache.lock() = Default::default();
        *self.top_level_env.lock() = Default::default();
        *self.ast_context.lock() = Default::default();
    }

    /// Set the top-level environment used when loading scripts.
    pub fn set_top_level_env(&self, env: BTreeMap<String, Value>) {
        *self.top_level_env.lock() = env;
    }

    /// Resolve a path to the full script path, based on the load path.
    ///
    /// If resolution succeeds, the returned path will be to a file (not directory).
    pub fn resolve_script_path<P: AsRef<Path>>(
        &self,
        working_dir: Option<P>,
        path: &Path,
    ) -> Option<PathBuf> {
        let get = script_path_exists(path, true);
        match working_dir {
            Some(dir) => get(dir.as_ref()),
            None => std::env::current_dir().ok().and_then(|p| get(p.as_ref())),
        }
        .or_else(|| self.load_path.iter().find_map(|p| get(p.as_ref())))
    }

    /// Load a script at the given path.
    ///
    /// The path should already be verified as an existing file.
    pub async fn load_script(&self, ctx: &Context, path: &Path) -> Value {
        debug_assert!(path.is_file());
        let path = path.canonicalize().unwrap(); // unwrap because is_file() should guarantee that canonicalize will succeed.

        let source = Source::new(FileSource(path.clone()));

        let mut loaded = self
            .load_cache
            .lock()
            .entry(path.clone())
            .or_insert_with(|| {
                let me = self.clone();
                let source = source.clone();
                let deps = depends![nsid!(load), path];
                Value::dyn_new(
                    move |ctx| async move {
                        if !is_plugin(&path) {
                            let script_result = {
                                let mut guard = me.ast_context.lock();
                                crate::Script::load(source, &mut *guard)
                            };
                            match script_result {
                                Err(e) => Err(e),
                                Ok(mut s) => {
                                    s.top_level_env(me.top_level_env.lock().clone());
                                    s.evaluate(ctx).await
                                }
                            }
                        } else {
                            dl::Library::new(&path)
                                .map_err(|e| e.into())
                                .and_then(|lib| {
                                    // Leak loaded libraries rather than storing them and dropping them in
                                    // the context, as this can cause issues if the thread pool hasn't shut
                                    // down.
                                    let l: &'static dl::Library =
                                        unsafe { std::mem::transmute(&lib) };
                                    std::mem::forget(lib);

                                    let f: dl::Symbol<
                                        extern "C" fn(
                                            ergo_runtime::plugin::Context,
                                            &Context,
                                        )
                                            -> RResult<Value>,
                                    > = unsafe { l.get(PLUGIN_ENTRY.as_bytes()) }?;
                                    f(ergo_runtime::plugin::Context::get(), ctx).into()
                                })
                        }
                        .into()
                    },
                    deps,
                )
            })
            .clone();

        loaded.eval_once(ctx).await;

        metadata::Source::imbue(source.with(loaded))
    }
}

pub struct LoadFunctions {
    pub load: Value,
    pub std: Value,
    pub workspace: Value,
    pub load_data: LoadData,
}

impl LoadFunctions {
    /// Return the load functions (which are all created with a shared cache).
    pub fn new(load_path: Vec<PathBuf>) -> Self {
        let load_data = LoadData::new(load_path);

        let ld = load_data.clone();
        let load = types::ergo_fn_value! {
            /// Load a script, with optional additional arguments with which to call the result.
            ///
            /// Arguments: `(StringOrPath :to-load) ^:call-args`
            ///
            /// ## Script resolution
            /// When loading a script, the following resolution process occurs for the first argument (if present):
            /// 1. Filesystem Name Resolution
            ///    a. If the passed script is an existing path in one of the load path directories, it is used.
            ///    b. If the passed script with the `.ergo` extension appended is an existing path in one of the
            ///      load path directories, it is used.
            ///
            ///    The load path is checked from first to last, and is determined by the location of `ergo` and the currently-executing
            ///    script. By default, the load path contains the directory containing the currently-executing script (or if there is no
            ///    script, the current working directory), followed by user and system directories.
            /// 2. Filesystem Directory Resolution
            ///    a. If the name-resolved script exists as a file, it is used.
            ///    b. If the name-resolved script exists as a directory, and the directory contains `dir.ergo`,
            ///       that path is used and step (2) is repeated.
            ///    c. If the name-resolved script exists as a directory, and the directory contains `workspace.ergo`,
            ///       that path is used and step (2) is repeated.
            ///
            /// If the directory-resolved script exists as a file, it is loaded. If additional arguments were
            /// provided, the resulting value is called with them.
            #[cloning(ld)]
            async fn load(mut path: _, ...) -> Value {
                ergo_runtime::try_result!(CONTEXT.eval(&mut path).await);
                let target_source = metadata::Source::get(&path);
                let target = match_value!{path,
                    types::String(s) => s.as_str().into(),
                    types::Path(p) => p.into_pathbuf(),
                    v => return traits::type_error(CONTEXT, v, "String or Path").into()
                };

                let working_dir = ARGS_SOURCE.path();
                let working_dir = working_dir.as_ref().and_then(|p| p.parent());

                // Try to find target in the load path.
                let target = match ld.resolve_script_path(working_dir, &target) {
                    Some(path) => path,
                    None => {
                        return target_source.with(format!("could not resolve script path: {}", target.display())).into_error().into();
                    }
                };

                // Load if some module was found.
                let loaded = ld.load_script(CONTEXT, &target).await;

                // If there are remaining arguments apply them immediately.
                if !REST.is_empty() {
                    traits::bind(CONTEXT, loaded,
                        metadata::Source::imbue(ARGS_SOURCE.with(types::Args { args: REST }.into()))).await
                } else {
                    loaded
                }
            }
        };

        let ld = load_data.clone();
        let std = types::Unbound::new(
            move |ctx, v| {
                let ld = ld.clone();
                async move {
                    let src = metadata::Source::get(&v);
                    let working_dir = src.path();
                    let working_dir = working_dir.as_ref().and_then(|p| p.parent());

                    let path = match ld.resolve_script_path(working_dir, "std".as_ref()) {
                        Some(path) => path,
                        None => {
                            return src.with("could not find std library").into_error().into();
                        }
                    };

                    let lib = ld.load_script(ctx, &path).await;

                    match_value!{v,
                        types::Args { args } => {
                            if args.is_empty() {
                                // If called with no args, return the loaded library.
                                lib
                            } else {
                                traits::bind(ctx, lib, metadata::Source::imbue(src.with(types::Args { args }.into()))).await
                            }
                        }
                        v => traits::bind(ctx, lib, v).await
                    }
                }
                .boxed()
            },
            depends![nsid!(func::std)],
            "Get the value as if `ergo std` were run, and apply any bindings to it. Return the library if called with no arguments."
        )
        .into();

        let ld = load_data.clone();
        let workspace = types::Unbound::new(
            move |ctx, v| {
                let ld = ld.clone();
                async move {
                    let src = metadata::Source::get(&v);

                    // If the current file is a workspace, allow it to load from parent workspaces.
                    let (path_basis, check_for_workspace) = match src.path() {
                        Some(p) => (p, true),
                        None => (std::env::current_dir().expect("couldn't get current directory"), false)
                    };

                    let resolved = ctx.shared_state.get(|| Ok(ResolvedWorkspaces::default())).unwrap();

                    let path = try_result!({
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
                    }.ok_or("no ancestor workspace found"));

                    let lib = ld.load_script(ctx, &path).await;

                    match_value!{v,
                        types::Args { args } => {
                            if args.is_empty() {
                                // If called with no args, return the loaded library.
                                lib
                            } else {
                                traits::bind(ctx, lib, metadata::Source::imbue(src.with(types::Args { args }.into()))).await
                            }
                        }
                        v => traits::bind(ctx, lib, v).await
                    }
                }
                .boxed()
            },
            depends![nsid!(func::workspace)],
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
}

/// Look at the file contents to determine if the file is a plugin (dynamic library).
fn is_plugin(f: &Path) -> bool {
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

#[derive(ErgoType)]
struct ResolvedWorkspaces {
    map: Arc<RMutex<BTreeMap<PathBuf, Option<PathBuf>>>>,
}

impl Default for ResolvedWorkspaces {
    fn default() -> Self {
        ResolvedWorkspaces {
            map: Arc::new(RMutex::new(Default::default())),
        }
    }
}

fn script_path_exists<'a, P: 'a + AsRef<Path>>(
    name: P,
    try_add_extension: bool,
) -> impl Fn(&Path) -> Option<PathBuf> + 'a {
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
                let dir = p.join(DIR_NAME);
                if dir.exists() {
                    p = dir;
                } else {
                    break;
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
