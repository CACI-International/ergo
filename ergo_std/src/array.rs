//! Array functions.

use ergo_runtime::{traits, type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Array::ergo_type(),
        index: crate::make_string_map! {
            "from" = from()
        },
    }
    .into()
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
            t.assert_eq("self:Array:from <| self:Iter:from [1,2,3]", "[1,2,3]");
        }

        fn from_args(t) {
            t.assert_eq("fn ^:args -> self:Array:from $args |> 1 2 3", "[1,2,3]");
            t.assert_fail("fn ^:args -> self:Array:from $args |> 1 2 (k=3)");
        }
    }
}
