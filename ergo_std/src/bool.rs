//! Bool functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "true" = true_val(),
        "false" = false_val(),
        "from" = from()
    }
}

fn true_val() -> Value {
    types::Bool(true).into()
}

fn false_val() -> Value {
    types::Bool(false).into()
}

#[types::ergo_fn]
/// Convert a value into a Bool.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    ergo_runtime::try_result!(traits::into_sourced::<types::Bool>(CONTEXT, value).await)
        .unwrap()
        .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn from(t) {
            t.assert_content_eq("self:bool:false", "self:bool:from self:type:Unset:");
            t.assert_content_eq("self:bool:true", "self:bool:from hello");
            t.assert_ne("self:bool:false", "self:bool:from ()"); // () is true, too
        }
    }
}
