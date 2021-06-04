///! Ergo standard module plugin.
use ergo_runtime::{plugin_entry, types, Context, Source, Value};

mod array;
mod bool;
mod env;
mod exec;
mod fs;
mod io;
mod iter;
mod log;
mod map;
#[path = "match.rs"]
mod match_;
mod net;
mod path;
mod string;
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
    ( $( $s:literal = $v:expr ),* ) => {
        {
            use ergo_runtime::{Source,Value};
            let mut m: ergo_runtime::abi_stable::bst::BstMap<Source<Value>,Source<Value>> = Default::default();
            $( m.insert(Source::builtin(crate::make_string($s)), Source::builtin($v)); )*
            types::Map(m).into()
        }
    };
}

#[plugin_entry]
fn entry(ctx: &Context) -> ergo_runtime::Result<Source<Value>> {
    // Add trait implementations
    exec::ergo_traits(&ctx.traits);

    Ok(Source::builtin(make_string_map! {
        "array" = array::module(),
        "bool" = bool::module(),
        "env" = env::module(),
        "exec" = exec::function(),
        "fs" = fs::module(),
        "io" = io::module(),
        "iter" = iter::module(),
        "log" = log::function(ctx),
        "map" = map::module(),
        "match" = match_::function(),
        "net" = net::module(),
        "path" = path::module(),
        "string" = string::module(),
        "task" = task::function(),
        "type" = type_::module(),
        "value" = value::module()
    }))
}
