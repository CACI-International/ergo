///! Ergo standard module plugin.
use ergo_runtime::{plugin_entry, source::Source, EvalResult, Runtime};

mod collection;
mod env;
mod error;
mod exec;
mod fs;
#[path = "if.rs"]
mod if_;
mod io;
mod log;
mod net;
mod path;
mod seq;
mod string;
mod task;
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
            grease::value::Value::from(ergo_runtime::types::Map(m))
        }
    };
}

#[plugin_entry]
fn entry(ctx: &mut Runtime) -> EvalResult {
    // Add trait implementations
    exec::traits(&mut ctx.traits);

    Ok(Source::builtin(grease_string_map! {
        "collection" = collection::module(),
        "env" = env::module(),
        "error" = error::module(),
        "exec" = exec::function(),
        "fs" = fs::module(),
        "if" = if_::function(),
        "io" = io::module(),
        "log" = log::function(ctx),
        "net" = net::module(),
        "path" = path::module(),
        "seq" = seq::function(),
        "string" = string::module(),
        "task" = task::function(),
        "value" = value::module()
    }))
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
