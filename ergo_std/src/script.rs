//! Script runtime module.

use ergo_runtime::{ergo_function, types, ContextExt, ResultIterator};
use grease::{path::PathBuf, value::Value};

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of script-related functions:"
        "bindings": "Get all of the current bindings in the script at the call site." = bindings_fn(),
        "dir": "Get the parent directory of the currently-executing script." = dir_fn(),
        "load-path": "Get the load path for any `ergo` calls." = load_path_fn(),
        "path": "Get the path of the currently-executing script." = path_fn(),
        "set-load-path": "Set the load path for any `ergo` calls." = set_load_path_fn()
    }
}

fn bindings_fn() -> Value {
    ergo_function!(independent std::script::bindings,
    r"Return the current bindings as a map.

Arguments: (none)

All bindings at the call site are returned as a Map.",
    |ctx, args| {
        args.unused_arguments()?;

        let env = ctx.env_flatten();

        types::Map(env.into_iter()
            .map(|(k,v)| Ok((k,v.into_result()?.unwrap())))
            .collect_result()?
        ).into_value(ctx)
    })
    .into()
}

fn load_path_fn() -> Value {
    ergo_function!(independent std::script::load_path,
            r"Get the load path of the currently-executing script.

Arguments: (None)

Returns an Array of Path.",
        |ctx, args| {
            args.unused_arguments()?;
            types::Array(ctx.current_load_path.clone().into_iter().map(|p| p.into()).collect()).into()
        }
    ).into()
}

fn set_load_path_fn() -> Value {
    ergo_function!(independent std::script::set_load_path,
    r"Set the load path of the currently-executing script.

Arguments: <Array of Path>

This function immediately executes.
Any load calls (`ergo ...`) following this call will use the given array of paths, where paths are checked in-order.",
    |ctx, args| {
        let path = args.next().ok_or("no load path provided")?;

        args.unused_arguments()?;

        let path = ctx.source_value_as::<types::Array>(path).await?;

        let source = path.source();
        let ps: Vec<_> = path.unwrap().await?.owned().0.into_iter().map(|v| source.clone().with(v)).collect();

        let mut paths: abi_stable::std_types::RVec<PathBuf> = Default::default();
        for p in ps {
            paths.push(ctx.source_value_as::<PathBuf>(p).await?.unwrap().await?.owned());
        }

        ctx.current_load_path = paths;

        types::Unit.into()
    })
    .into()
}

fn dir_fn() -> Value {
    ergo_function!(
        independent std::script::dir,
        r"Get the parent directory of the currently-executing script.

Arguments: (none)

Returns a Path.",
        |ctx, args| {
            args.unused_arguments()?;
            grease::path::PathBuf::from(ctx.mod_dir()).into()
        }
    )
    .into()
}

fn path_fn() -> Value {
    ergo_function!(
        independent std::script::path,
        r"Get the path of the currently-executing script.

Arguments: (none)

Returns a Path.",
        |ctx, args| {
            args.unused_arguments()?;
            ctx.mod_path.clone().into_option().ok_or("no script path available")?.into()
        }
    )
    .into()
}
