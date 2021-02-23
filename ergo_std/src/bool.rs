//! Bool functions.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::Value;

pub fn module() -> Value {
    crate::grease_string_map! {
        "Bool values."
        "true": "true value" = true_val(),
        "false": "false value" = false_val(),
        "from": "Convert from a value to Bool." = from_fn()
    }
}

fn true_val() -> Value {
    types::Bool(true).into()
}

fn false_val() -> Value {
    types::Bool(false).into()
}

fn from_fn() -> Value {
    ergo_function!(independent std::bool::from,
        "Convert from another value to Bool.

Arguments: value",
    |ctx, args| {
        let val = args.next().ok_or("missing value")?;
        args.unused_arguments()?;

        ctx.into_sourced::<types::Bool>(val).await?.unwrap().into()
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn from(t) {
            t.assert_success("self:bool:false = !self:bool:from self:type:Unset:");
            t.assert_success("self:bool:true = !self:bool:from hello");
            t.assert_script_fail("self:bool:false = !self:bool:from ()"); // () is true, too
        }
    }
}
