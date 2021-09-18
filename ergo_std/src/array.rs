//! Array functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "from" = from()
    }
}

#[types::ergo_fn]
/// Convert a value into an Array.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    traits::into::<types::Array>(value).await?.into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_content_eq("self:array:from <| self:iter:from [1,2,3]", "[1,2,3]");
        }

        fn from_args(t) {
            t.assert_eq("fn ^:args -> self:array:from :args |> 1 2 3", "[1,2,3]");
            t.assert_fail("fn ^:args -> self:array:from :args |> 1 2 (k=3)");
        }
    }
}
