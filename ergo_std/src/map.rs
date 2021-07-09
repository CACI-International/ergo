//! Map functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "from" = from()
    }
}

#[types::ergo_fn]
/// Convert a value into an Map.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    ergo_runtime::try_result!(traits::into::<types::Map>(value).await).into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_content_eq("self:map:from <| self:iter:from {a=1,b=2}", "{a=1,b=2}");
        }

        fn iter(t) {
            t.assert_content_eq("self:map:from <| self:iter:map (fn (self:type:MapEntry: :key :value) -> self:type:MapEntry: :key ()) {a=1,b=2}", "{a=(),b=()}");
        }
    }
}
