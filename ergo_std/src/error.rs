//! Error functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "display" = display()
    }
}

#[types::ergo_fn]
/// Display an error as a String.
///
/// Arguments: `Error :value`
async fn display(value: _) -> Value {
    let mut s = String::new();
    traits::display_any(value, &mut traits::Formatter::new(&mut s)).await?;
    types::String::from(s).into()
}
