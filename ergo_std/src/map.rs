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
    ergo_runtime::try_result!(traits::into_sourced::<types::Map>(CONTEXT, value).await)
        .unwrap()
        .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn from(t) {
            t.assert_content_eq("self:map:from <| self:iter:from {a=1,b=2}", "{a=1,b=2}");
        }
    }

    ergo_script::test! {
        fn iter(t) {
            t.assert_content_eq("self:map:from <| self:iter:map (fn (self:type:MapEntry: :key :value) -> self:type:MapEntry: :key ()) {a=1,b=2}", "{a=(),b=()}");
        }
    }
}
