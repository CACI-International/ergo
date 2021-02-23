///! Ergo standard module plugin.
use ergo_runtime::{plugin_entry, source::Source, EvalResult, Runtime};

mod bool;
mod collection;
mod env;
mod error;
mod exec;
mod fs;
mod io;
mod log;
#[path = "match.rs"]
mod match_;
mod net;
mod path;
mod script;
mod seq;
mod string;
pub mod task;
#[path = "type.rs"]
mod type_;
mod value;

pub use exec::ExitStatus;

fn grease_string(s: &str) -> grease::value::Value {
    ergo_runtime::types::String::from(s).into()
}

#[macro_export]
macro_rules! grease_string_map {
    ( $map_doc:literal $( $s:literal : $doc:literal = $v:expr ),* ) => {
        {
            let mut m: grease::bst::BstMap<grease::value::Value,grease::value::Value> = Default::default();
            $( m.insert(crate::grease_string($s), $v); )*
            let mut v = ergo_runtime::types::Map(m).into_value_no_doc();
            let mut doc = String::from($map_doc);
            doc.push('\n');
            $( doc.push_str(&format!("\n* {}: {}", $s, $doc)); )*
            v.set_metadata(
                &ergo_runtime::metadata::Doc,
                grease::TypedValue::constant(ergo_runtime::types::String::from(doc)),
            );
            v
        }
    };
}

#[plugin_entry]
fn entry(ctx: &mut Runtime) -> EvalResult {
    // Add trait implementations
    exec::traits(&mut ctx.traits);

    Ok(Source::builtin(grease_string_map! {
        r"The ergo standard library.

This library provides a number of submodules that group related functions, as well as a few top-level functions.
Generally, it is most useful to merge the library into the top-level environment, for instance with `^ergo std`.

The library's map includes:"
        "bool": "Boolean values" = bool::module(),
        "collection": "Utilities for manipulating arrays and maps." = collection::module(),
        "env": "Access to the program's environment." = env::module(),
        "error": "Error handling functions." = error::module(),
        "exec": "The exec function, to launch external programs." = exec::function(),
        "fs": "Filesystem manipulations." = fs::module(),
        "io": "Program IO functions." = io::module(),
        "log": "The log function, to log values to the runtime log." = log::function(ctx),
        "match": "The match function, to attempt multiple bindings of a value." = match_::function(),
        "net": "Network functionality." = net::module(),
        "path": "Path creation and manipulation." = path::module(),
        "script": "Script runtime functions." = script::module(),
        "seq": "The seq function, to create sequential dependencies." = seq::function(),
        "string": "String creation and manipulation." = string::module(),
        "task": "The task function, to evaluate a value as a concurrent task." = task::function(),
        "type": "Type creation and checking." = type_::module(),
        "value": "Value manipulation functions." = value::module()
    }))
}
