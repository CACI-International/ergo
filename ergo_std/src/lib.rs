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
    ( $( $s:literal = $v:expr ),* ) => {
        {
            let mut m: grease::bst::BstMap<grease::value::Value,grease::value::Value> = Default::default();
            $( m.insert(crate::grease_string($s), $v); )*
            ergo_runtime::types::Map(m).into_value_no_doc()
        }
    };
}

#[plugin_entry]
fn entry(ctx: &mut Runtime) -> EvalResult {
    // Add trait implementations
    exec::traits(&mut ctx.traits);

    Ok(Source::builtin(grease_string_map! {
        "bool" = bool::module(),
        "collection" = collection::module(),
        "env" = env::module(),
        "error" = error::module(),
        "exec" = exec::function(),
        "fs" = fs::module(),
        "io" = io::module(),
        "log" = log::function(ctx),
        "match" = match_::function(),
        "net" = net::module(),
        "path" = path::module(),
        "script" = script::module(),
        "seq" = seq::function(),
        "string" = string::module(),
        "task" = task::function(),
        "type" = type_::module(),
        "value" = value::module()
    }))
}
