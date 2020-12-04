//! Environment variable functions.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::{
    make_value,
    path::PathBuf,
    value::{TypedValue, Value},
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of program environment functions:"
        "get": "Get an environment variable." = get_fn(),
        "home": "Get the user's home directory." = home_path(),
        "path-search": "Find a binary in the system path." = path_search_fn(),
        "current-dir": "Get the program's present working directory." = current_dir_path()
    }
}

pub fn get_fn() -> Value {
    ergo_function!(independent std::env::get,
    r"Get an environment variable.

Arguments: <environment variable name: String>
If the environment variable is not set, returns `()`. Otherwise returns the value of the environment variable as a
string, where the string is identified by the environment variable's content.",
    |ctx| {
        let name = ctx
            .args
            .next()
            .ok_or("environment variable name not provided")?;

        ctx.unused_arguments()?;

        let name = ctx.source_value_as::<types::String>(name);
        let name = name.await?.unwrap();

        match std::env::var_os(name.await?.as_ref().as_str()) {
            None => types::Unit.into(),
            Some(v) => types::String::from(
                v.into_string()
                    .map_err(|_| "environment variable value is not valid unicode")?,
            )
            .into(),
        }
    })
    .into()
}

pub fn current_dir_path() -> Value {
    let path = std::env::current_dir().ok();
    let mut v = make_value!([ergo_runtime::namespace_id!(std::env::current_dir), path] {
        path.map(|d| PathBuf::from(d)).ok_or("current directory path could not be retrieved".into())
    });
    v.set_metadata(
        &ergo_runtime::metadata::Doc,
        TypedValue::constant("The current working directory of the process, as a path.".into()),
    );
    v.into()
}

pub fn home_path() -> Value {
    let path = directories::BaseDirs::new().map(|d| d.home_dir().to_owned());
    let mut v = make_value!([ergo_runtime::namespace_id!(std::env::home), path] {
        path.map(|d| PathBuf::from(d)).ok_or("home path could not be retrieved".into())
    });
    v.set_metadata(
        &ergo_runtime::metadata::Doc,
        TypedValue::constant("The current user's home directory path.".into()),
    );
    v.into()
}

pub fn path_search_fn() -> Value {
    ergo_function!(
        std::env::path_search,
        r"Find a file in the binary lookup path.

Arguments: <name: String>
If the value is not found, an error occurs. Otherwise the path of the resolved file is returned.",
        |ctx| {
            let name = ctx.args.next().ok_or("no search argument provided")?;

            ctx.unused_arguments()?;

            let name = ctx.source_value_as::<types::String>(name);
            let name = name.await?.unwrap();

            let paths = std::env::var_os("PATH")
                .map(|path| std::env::split_paths(&path).collect())
                .unwrap_or(vec![]);

            make_value!([paths, name] {
                let name = name.await?;

                for p in paths {
                    let path = p.join(name.as_ref().as_str());
                    if path.is_file() {
                        return Ok(PathBuf::from(path));
                    }
                }

                Err(format!("could not find {} in PATH", name.as_ref().as_str()).into())
            })
            .into()
        }
    )
    .into()
}
