//! Base environment.

use super::runtime::Error;
use crate::constants::{DIR_NAME, EXTENSION, PLUGIN_ENTRY, WORKSPACE_NAME};
use abi_stable::std_types::{ROption, RResult};
use ergo_runtime::{
    namespace_id, source::FileSource, traits, types, ContextExt, EvalResult, Runtime, Source,
};
use futures::FutureExt;
use grease::{
    depends, match_value,
    path::PathBuf,
    value::{IntoValue, TypedValue, Value},
};
use libloading as dl;
use std::path;

/// Documentation for the load function.
pub const LOAD_DOCUMENTATION: &'static str = r"Load a script, with optional additional arguments with which to call the result.

The first argument, if present, must be a string or path.

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

If the directory-resolved script exists as a file, it is loaded. If additional arguments were provided, they are applied
to the resulting value.";

/// Return the load function.
pub fn load() -> Value {
    types::Unbound::new(
        |ctx, v| {
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
                let loaded = load_script(ctx, &target).await?;

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
        Some(TypedValue::constant(LOAD_DOCUMENTATION.into())),
    )
    .into()
}

/// A binding function returning an Args.
pub fn bind_args_to_args() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move {
                let args_val = ctx.source_value_as::<types::BindArgs>(v).await?.unwrap();
                let args = args_val.await?.owned().args;
                Ok(types::Args { args }.into_value())
            }
            .boxed()
        },
        depends![namespace_id!(ergo::fn)],
        Some(TypedValue::constant(
            "The 'fn' binding function, which takes all BindArgs and returns an Args to be bound."
                .into(),
        )),
    )
    .into()
}

/// A binding function returning a BindArgs.
pub fn bind_args_to_bind_args() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move { Ok(ctx.source_value_as::<types::BindArgs>(v).await?.unwrap().into()) }.boxed()
        },
        depends![namespace_id!(ergo::pat)],
        Some(TypedValue::constant(
            "The 'pat' binding function, which takes all BindArgs and returns a BindArgs to be bound."
                .into(),
        )),
    )
    .into()
}

/// A binding function returning an Index.
pub fn bind_args_to_index() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move {
                let args_val = ctx.source_value_as::<types::BindArgs>(v).await?.unwrap();
                let mut args = args_val.await?.owned().args.checked();
                let ind_v = args.next().ok_or("missing index argument")?;
                args.unused_arguments()?;
                Ok(types::Index(ind_v).into_value())
            }.boxed()
        },
        depends![namespace_id!(ergo::index)],
        Some(TypedValue::constant(
            "The 'index' binding function, which takes a single argument and returns an Index to be bound."
                .into(),
        )),
    )
    .into()
}

/// A value that behaves as if `ergo std` was run just prior to binding.
pub fn load_std() -> Value {
    types::Unbound::new(
        |ctx, v| {
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

                let lib = load_script(ctx, &path).await?;

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
        depends![namespace_id!(ergo::load)],
        Some(TypedValue::constant(
            "Get the value as if `ergo std` was run, and apply any bindings to it. Return the library if called with no arguments.".into(),
        )),
    )
    .into()
}

/// A value that behaves as if `ergo path/to/ancestor/workspace` was run just prior to binding.
pub fn load_workspace() -> Value {
    types::Unbound::new(
        |ctx, v| {
            async move {
                // If the current file is a workspace, allow it to load from parent workspaces.
                let (path_basis, check_for_workspace) = if let ROption::RSome(v) = &ctx.mod_path {
                    (v.clone().into(), true)
                } else {
                    (ctx.mod_dir(), false)
                };

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

                let path = ancestors.find_map(script_path_exists(WORKSPACE_NAME, false)).ok_or("no ancestor workspace found")?;

                let lib = load_script(ctx, &path).await?;

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
        depends![namespace_id!(ergo::load)],
        Some(TypedValue::constant(
            "Get the value as if `ergo path/to/ancestor/workspace.ergo` was run, and apply any bindings to it.
Return the workspace value if called with no arguments.".into(),
        )),
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

/// Load a script at the given path.
///
/// The path should already be verified as an existing file.
async fn load_script(ctx: &mut Runtime, path: &path::Path) -> EvalResult {
    // Check whether path is already being loaded.
    for l_path in ctx.loading.lock().iter() {
        if l_path.as_ref() == path {
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
    let cache = ctx.load_cache.clone();

    // Since we are in an async function, we need to access the cache in a somewhat odd
    // pattern.
    let cached = {
        let cache = cache.lock();
        let p = PathBuf::from(path.to_owned());
        let ret = cache.get(&p).cloned();
        if ret.is_none() {
            // Exclude path from loading in nested calls.
            ctx.loading.lock().push(p);
        }
        ret
    };

    match cached {
        None => {
            let result = if !is_plugin(&path) {
                let mut script = super::Script::load(Source::new(FileSource(path.clone())))?;
                script.file_path(path.clone());
                script.evaluate(ctx).await.into()
            } else {
                let lib = dl::Library::new(&path)?;
                let f: dl::Symbol<
                    extern "C" fn(
                        ergo_runtime::plugin::Context,
                        &mut Runtime,
                    ) -> RResult<Source<Value>, grease::Error>,
                > = unsafe { lib.get(PLUGIN_ENTRY.as_bytes()) }?;
                let result = f(ergo_runtime::plugin::Context::get(), ctx);
                // Leak loaded libraries rather than storing them and dropping them in
                // the context, as this can cause issues if the thread pool hasn't shut
                // down.
                // ctx.lifetime(lib);
                std::mem::forget(lib);
                result
            };

            let cache = ctx.load_cache.clone();
            let mut cache = cache.lock();
            cache.insert(PathBuf::from(path), result.clone());
            ctx.loading.lock().pop();
            result
        }
        Some(v) => v,
    }
    .into_result()
}
