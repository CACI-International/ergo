//! The getopt function.

use ergo_runtime::{try_result, types, Value};
use std::collections::BTreeMap;

#[types::ergo_fn]
/// Convert an Array of Strings to positional and keyed arguments.
///
/// This is very vaguely reminiscent of GNU getopt, however rather than taking a specification of
/// the arguments to expect, we simply convert strings to arguments using the following simple
/// rules:
/// * Any string starting with `--` is considered a keyed argument. Optionally, a keyed argument
/// can contain `=` to set the value of the argument. The key is the portion of the string
/// following `--` and before `=` if present, the value is the portion of the string after `=`, or
/// Unit if no value is given.
/// * Any other string is considered a positonal argument.
///
/// Arguments: `(Array:Of :String) :args`
///
/// Returns an Args type with the positional and keyed arguments.
pub async fn function(args: types::Array) -> Value {
    let args = args.unwrap().to_owned().0;

    let mut positional = Vec::new();
    let mut keyed = BTreeMap::new();

    for a in args {
        let s = try_result!(CONTEXT.eval_as::<types::String>(a).await);
        match s.value().as_ref().as_str().strip_prefix("--") {
            None => positional.push(s.map(|v| v.into())),
            Some(rest) => {
                let src = s.source();
                match rest.split_once("=") {
                    None => keyed.insert(
                        src.clone().with(types::String::from(rest).into()),
                        src.with(types::Unit.into()),
                    ),
                    Some((a, b)) => keyed.insert(
                        src.clone().with(types::String::from(a).into()),
                        src.with(types::String::from(b).into()),
                    ),
                };
            }
        }
    }

    types::Args {
        args: types::args::Arguments::new(positional, keyed).unchecked(),
    }
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn getopt(t) {
            t.assert_content_eq("self:getopt [a,b,c,d]", "fn ^:args -> :args |> a b c d");
            t.assert_content_eq("self:getopt [--flag,pos1,'--key=a',pos2,pos3,'--key2=123=b']",
                "fn ^:args -> :args |> ^flag (key=a) (key2='123=b') pos1 pos2 pos3");
        }
    }
}