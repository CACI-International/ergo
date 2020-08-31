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

mod collection;
mod exec;
mod fs;
#[path = "if.rs"]
mod if_;
mod path;
mod seq;
mod string;
mod task;
mod value;

pub use exec::{ByteStream, ExitStatus};

#[plugin_entry]
fn entry(ctx: &mut Context<Runtime>) -> EvalResult {
    // Add trait implementations
    exec::traits(&mut ctx.traits);

    let mut m = BstMap::default();
    m.insert("collection".into(), collection::module());
    m.insert("exec".into(), exec::function());
    m.insert("fs".into(), fs::module());
    m.insert("if".into(), if_::function());
    m.insert("path".into(), path::module());
    m.insert("seq".into(), seq::function());
    m.insert("string".into(), string::module());
    m.insert("task".into(), task::function());
    m.insert("value".into(), value::module());
    Ok(Source::builtin(types::Map(m).into()))
}
