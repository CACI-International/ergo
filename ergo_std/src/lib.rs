///! Ergo standard module plugin.
use ergo_runtime::{plugin_entry, types, Context, Value};

mod array;
mod bool;
mod env;
mod error;
mod exec;
mod fs;
mod getopt;
mod io;
mod iter;
mod json;
mod log;
mod map;
#[path = "match.rs"]
mod match_;
mod net;
mod number;
mod path;
mod string;
mod sync;
pub mod task;
#[path = "type.rs"]
mod type_;
mod value;

//pub use exec::ExitStatus;

fn make_string(s: &str) -> Value {
    types::String::from(s).into()
}

#[macro_export]
macro_rules! make_string_map {
    ( source $src:expr, $( $s:literal = $v:expr ),* ) => {
        {
            let src = $src;
            use ergo_runtime::{Value};
            use ergo_runtime::metadata::Source;
            let mut m: ergo_runtime::abi_stable::bst::BstMap<Value,Value> = Default::default();
            $( m.insert(Source::imbue(src.clone().with(crate::make_string($s))), Source::imbue(src.clone().with($v))); )*
            Source::imbue(src.with(types::Map(m).into()))
        }
    };
    ( $( $s:literal = $v:expr ),* ) => {
        $crate::make_string_map! { source ergo_runtime::Source::missing(()), $( $s = $v ),* }
    };
}

#[plugin_entry]
fn entry() -> ergo_runtime::Result<Value> {
    // Add trait implementations
    {
        let traits = &Context::global().traits;
        exec::ergo_traits(traits);
        net::ergo_traits(traits);
        sync::ergo_traits(traits);
    }

    Ok(make_string_map! {
        "array" = array::module(),
        "bool" = bool::module(),
        "env" = env::module(),
        "error" = error::module(),
        "exec" = exec::function(),
        "fs" = fs::module(),
        "getopt" = getopt::function(),
        "io" = io::module(),
        "iter" = iter::module(),
        "json" = json::module(),
        "log" = log::function(),
        "map" = map::module(),
        "match" = match_::function(),
        "net" = net::module(),
        "number" = number::module(),
        "path" = path::module(),
        "string" = string::module(),
        "sync" = sync::module(),
        "task" = task::function(),
        "type" = type_::module(),
        "value" = value::module()
    })
}
