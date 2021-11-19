//! The getopt function.

use ergo_runtime::{metadata::Source, types, Context, Value};
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
/// Keyed Arguments:
/// * `:consecutive` (optional) - If present, only parse consecutive flags at the beginning of the
/// array, i.e., consider all arguments after the first positional argument to be positional.
///
/// Returns an Args type with the positional and keyed arguments.
pub async fn function(args: types::Array, (consecutive): [_]) -> Value {
    let args = args.to_owned().0;
    let consecutive = consecutive.is_some();

    let mut positional = Vec::new();
    let mut keyed = BTreeMap::new();

    let mut force_positional = false;

    for a in args {
        let s = Context::eval_as::<types::String>(a).await?;
        if force_positional {
            positional.push(s.into());
            continue;
        }
        match s.as_ref().as_str().strip_prefix("--") {
            None => {
                positional.push(s.into());
                force_positional = consecutive;
            }
            Some(rest) => {
                let src = Source::get(&s);
                match rest.split_once("=") {
                    None => keyed.insert(
                        Source::imbue(src.clone().with(types::String::from(rest).into())),
                        Source::imbue(src.with(types::Unit.into())),
                    ),
                    Some((a, b)) => keyed.insert(
                        Source::imbue(src.clone().with(types::String::from(a).into())),
                        Source::imbue(src.with(types::String::from(b).into())),
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
    ergo_script::tests! {
        fn getopt(t) {
            t.assert_content_eq("self:getopt [a,b,c,d]", "fn ^:args -> :args |> a b c d");
            t.assert_content_eq("self:getopt [--flag,pos1,\"--key=a\",pos2,pos3,\"--key2=123=b\"]",
                "fn ^:args -> :args |> ^flag (key=a) (key2=\"123=b\") pos1 pos2 pos3");
        }

        fn consecutive(t) {
            t.assert_content_eq("self:getopt ^consecutive [--a,--b,c,--d]", "fn ^:args -> :args |> ^a ^b c --d");
        }
    }
}
