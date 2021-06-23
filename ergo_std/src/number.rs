//! Number functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "from" = from()
    }
}

#[types::ergo_fn]
/// Convert a value into a Number.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    ergo_runtime::try_result!(traits::into_sourced::<types::Number>(CONTEXT, value).await)
        .unwrap()
        .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn from(t) {
            t.assert_content_eq("self:type:Number: 1/4", "self:number:from 0.25");
            t.assert_ne("self:type:Number: 1/3", "self:number:from 0.333333333333333333333333");
        }
    }
}
