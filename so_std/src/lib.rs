/*
mod cache;
mod debug;
mod exec;
mod fold;
mod fs;
mod has;
#[path="if.rs"]
mod if_;
mod load;
mod map;
mod path;
mod seq;
mod string;
mod track;
mod value;
mod variable;
*/

use grease::{bst::BstMap, runtime::Context};
use so_runtime::{plugin_entry, source::Source, types, EvalResult, Runtime};

mod string;

#[plugin_entry]
fn entry(_ctx: &mut Context<Runtime>) -> EvalResult {
    let mut m = BstMap::default();
    m.insert("string".into(), string::module());
    Ok(Source::builtin(types::Map(m).into()))
}
