use ergo_runtime::{plugin_entry, source::Source, types, EvalResult, Runtime};
/// Ergo standard module plugin.
use grease::bst::BstMap;

mod collection;
mod env;
mod error;
mod exec;
mod fs;
#[path = "if.rs"]
mod if_;
mod log;
mod net;
mod path;
mod seq;
mod string;
mod task;
mod value;

pub use exec::ExitStatus;

#[plugin_entry]
fn entry(ctx: &mut Runtime) -> EvalResult {
    // Add trait implementations
    exec::traits(&mut ctx.traits);

    let mut m = BstMap::default();
    m.insert("collection".into(), collection::module());
    m.insert("env".into(), env::module());
    m.insert("error".into(), error::module());
    m.insert("exec".into(), exec::function());
    m.insert("fs".into(), fs::module());
    m.insert("if".into(), if_::function());
    m.insert("log".into(), log::function(ctx));
    m.insert("net".into(), net::module());
    m.insert("net".into(), net::module());
    m.insert("path".into(), path::module());
    m.insert("seq".into(), seq::function());
    m.insert("string".into(), string::module());
    m.insert("task".into(), task::function());
    m.insert("value".into(), value::module());
    Ok(Source::builtin(types::Map(m).into()))
}

#[cfg(test)]
mod test {

    /*
    #[test]
    fn if_expr() -> Result<(), String> {
        script_eval_to("if () a b", SRString("b"))?;
        script_eval_to("if r a b", SRString("a"))?;
        Ok(())
    }
    */

    /*
    #[test]
    fn string_format() -> Result<(), String> {
        script_eval_to("string format \"hello {}\" world", SRString("hello world"))?;
        script_eval_to("string format \"{1}{}{2}{0}\" a b c d", SRString("baca"))?;
        script_eval_to(
            "string format \"{my_named_arg} {}\" ^{my_named_arg = howdy} hi",
            SRString("howdy hi"),
        )?;
        script_eval_to("string format \"{{{{}}\"", SRString("{{}"))?;
        script_fail("string format \"{\"")?;
        script_fail("string format \"}\"")?;
        script_fail("string format \"{}\"")?;
        script_fail("string format \"{named}\" ^{not-named=1}")?;
        script_fail("string format \"{{{}}\" a")?;
        Ok(())
    }

    #[test]
    fn exec() -> Result<(), String> {
        script_eval_to("(exec echo hello) stdout", SRAny)
    }

    #[test]
    fn exec_failure() -> Result<(), String> {
        script_result_fail("(exec false) stdout")
    }
    */
}
