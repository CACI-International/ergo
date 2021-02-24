//! Environment variable functions.

use ergo_runtime::{ergo_function, traits, types, ContextExt};
use grease::{
    make_value, match_value,
    path::PathBuf,
    value::{TypedValue, Value},
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "get" = get_fn(),
        "home" = home_path(),
        "path-search" = path_search_fn(),
        "current-dir" = current_dir_path(),
        "user-cache" = user_cache_path(),
        "system-cache" = system_cache_path(),
        "os" = os_string()
    }
}

fn get_fn() -> Value {
    ergo_function!(independent std::env::get,
    r"Get an environment variable.

Arguments: <environment variable name: String>
If the environment variable is not set, returns `Unset`. Otherwise returns the value of the environment variable as a
string, where the string is identified by the environment variable's content.",
    |ctx,args| {
        let name = args.next()
            .ok_or("environment variable name not provided")?;

        args.unused_arguments()?;

        let name = ctx.source_value_as::<types::String>(name);
        let name = name.await?.unwrap();

        match std::env::var_os(name.await?.as_ref().as_str()) {
            None => types::Unset::new().into(),
            Some(v) => types::String::from(
                v.into_string()
                    .map_err(|_| "environment variable value is not valid unicode")?,
            )
            .into(),
        }
    })
    .into()
}

fn current_dir_path() -> Value {
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

fn user_cache_path() -> Value {
    let path = directories::ProjectDirs::from("", "", "ergo").map(|d| d.cache_dir().to_owned());
    let mut v = make_value!([ergo_runtime::namespace_id!(std::env::user_cache), path] {
        path.map(|d| PathBuf::from(d)).ok_or("user cache path could not be retrieved".into())
    });
    v.set_metadata(
        &ergo_runtime::metadata::Doc,
        TypedValue::constant("A user-level cache directory path.".into()),
    );
    v.into()
}

fn system_cache_path() -> Value {
    let mut path = if cfg!(unix) {
        Some(std::path::Path::new("/var/cache/ergo"))
    } else if cfg!(windows) {
        Some(std::path::Path::new("C:\\Program Files\\ergo"))
    } else {
        None
    };
    if let Some(p) = &path {
        match p.metadata() {
            Err(_) => path = None,
            Ok(m) => {
                if !m.is_dir() || m.permissions().readonly() {
                    path = None;
                }
            }
        }
    }

    let path = path.map(|d| d.to_owned()).or_else(|| {
        directories::ProjectDirs::from("", "", "ergo").map(|d| d.cache_dir().to_owned())
    });

    let mut v = make_value!([ergo_runtime::namespace_id!(std::env::system_cache), path] {
        path.map(|d| PathBuf::from(d)).ok_or("system cache path could not be retrieved".into())
    });
    v.set_metadata(
        &ergo_runtime::metadata::Doc,
        TypedValue::constant("A system-level cache directory path.".into()),
    );
    v.into()
}

fn home_path() -> Value {
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

fn path_search_fn() -> Value {
    ergo_function!(
        std::env::path_search,
        r"Find a file in the binary lookup path.

Arguments: <name: String or Path>
If a Path is passed, it will simply be returned as-is.
If a String is passed, a Path value is returned that, when evaluated, will searched for the string
in PATH. If not found, an error occurs. Otherwise the path of the resolved file is returned.",
        |ctx, args| {
            let arg = args.next().ok_or("no search argument provided")?;

            args.unused_arguments()?;

            let (arg_source, arg) = arg.take();
            match_value!(arg => {
                PathBuf => |v| v,
                types::String => |name| {
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
                }
                => |other| traits::type_error(ctx, arg_source.with(other), "String or Path").await?
            })
            .await?
            .into()
        }
    )
    .into()
}

fn os_string() -> Value {
    let mut v: Value = types::String::from(std::env::consts::OS).into();
    v.set_metadata(
        &ergo_runtime::metadata::Doc,
        TypedValue::constant(
            "The OS running ergo.

Possible values include `linux`, `macos`, and `windows`."
                .into(),
        ),
    );
    v
}
