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
    ergo_runtime::try_result!(traits::into::<types::Bool>(value).await).into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_content_eq("self:bool:false", "self:bool:from :unset");
            t.assert_content_eq("self:bool:true", "self:bool:from hello");
            t.assert_ne("self:bool:false", "self:bool:from ()"); // () is true, too
        }
    }
}
