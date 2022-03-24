///! Ergo standard module plugin.
use ergo_runtime::{plugin_entry, types, Context, IdentifiedValue, Value};

mod array;
mod bool;
mod env;
mod error;
mod exec;
mod fs;
mod function;
mod getopt;
mod io;
mod iter;
mod json;
mod log;
mod map;
mod map_entry;
mod r#match;
mod net;
mod number;
mod order;
mod path;
mod string;
mod sync;
pub mod task;
mod r#type;
mod unit;
mod unset;
mod value;

//pub use exec::ExitStatus;

fn make_string_src(s: ergo_runtime::Source<&str>) -> IdentifiedValue {
    let (src, s) = s.take();
    let mut v: IdentifiedValue = types::String::from(s).into();
    // Safety: adding metadata doesn't change the identity state.
    ergo_runtime::metadata::Source::set(unsafe { v.value_mut() }, src);
    v
}

fn make_string(s: &str) -> IdentifiedValue {
    types::String::from(s).into()
}

#[macro_export]
macro_rules! make_string_map {
    ( source $src:expr, $( $s:literal = $v:expr ),* ) => {
        {
            let src = $src;
            use ergo_runtime::{IdentifiedValue, Value, types};
            use ergo_runtime::metadata::Source;
            let mut m: ergo_runtime::abi_stable::bst::BstMap<IdentifiedValue,Value> = Default::default();
            $(m.insert(crate::make_string_src(src.clone().with($s)), Source::imbue(src.clone().with($v)));)*
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
        order::ergo_traits(traits);
        sync::ergo_traits(traits);
    }

    Ok(make_string_map! {
        "Array" = array::r#type(),
        "Bool" = bool::r#type(),
        "Error" = error::r#type(),
        "Function" = function::r#type(),
        "Iter" = iter::r#type(),
        "Map" = map::r#type(),
        "MapEntry" = map_entry::r#type(),
        "Number" = number::r#type(),
        "Order" = order::r#type(),
        "Path" = path::r#type(),
        "String" = string::r#type(),
        "Unit" = unit::r#type(),
        "Unset" = unset::r#type(),
        "Type" = r#type::r#type(),
        "env" = env::module(),
        "exec" = exec::function(),
        "fs" = fs::module(),
        "getopt" = getopt::function(),
        "io" = io::module(),
        "json" = json::module(),
        "log" = log::function(),
        "match" = r#match::function(),
        "net" = net::module(),
        "sync" = sync::module(),
        "task" = task::function(),
        "value" = value::module()
    })
}
