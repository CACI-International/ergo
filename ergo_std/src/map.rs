//! Map functions.

use ergo_runtime::{traits, type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Map::ergo_type(),
        index: crate::make_string_map! {
            "from" = from()
        },
    }
    .into()
}

#[types::ergo_fn]
/// Convert a value into an Map.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    traits::into::<types::Map>(value).await?.into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_eq("self:Map:from <| self:Iter:from {a=1,b=2}", "{a=1,b=2}");
        }

        fn iter(t) {
            t.assert_eq("self:Map:from <| self:Iter:map (fn (self:MapEntry:@ :key :value) -> self:MapEntry:new $key ()) {a=1,b=2}", "{a=(),b=()}");
        }

        fn from_args(t) {
            t.assert_eq("fn ^:args -> self:Map:from $args |> (a=1) (b=2)", "{a=1,b=2}");
            t.assert_fail("fn ^:args -> self:Map:from $args |> (a=1) 2");
        }
    }
}
