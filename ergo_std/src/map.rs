//! Map functions.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::Value;

pub fn module() -> Value {
    crate::grease_string_map! {
        "from" = from_fn()
    }
}

fn from_fn() -> Value {
    ergo_function!(independent std::map::from,
    r"Convert a value into an Map.

Arguments: value",
    |ctx, args| {
        let value = args.next().ok_or("value not provided")?;

        args.unused_arguments()?;

        ctx.into_sourced::<types::Map>(value).await?.unwrap().into()
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn from(t) {
            t.assert_content_eq("self:map:from <| self:iter:from {a=1,b=2}", "{a=1,b=2}");
        }
    }
}
