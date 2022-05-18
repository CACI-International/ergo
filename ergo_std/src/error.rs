//! Error functions.

use ergo_runtime::{error::DiagnosticInfo, traits, type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Error::ergo_type(),
        index: crate::make_string_map! {
            "display" = display(),
            "new" = new()
        },
    }
    .into()
}

#[types::ergo_fn]
/// Display an error as a String.
///
/// Arguments: `(Error :value)`
async fn display(value: _) -> Value {
    let mut s = String::new();
    traits::display_any(value, &mut traits::Formatter::new(&mut s)).await?;
    types::String::from(s).into()
}

#[types::ergo_fn]
/// Create a new Error.
///
/// Arguments: `:message`
///
/// Keyed Arguments:
/// * `source` - The value whose source should be used as the source of the error. If omitted, the
/// source of `msg` is used.
///
/// `message` is displayed as a string in the Error.
async fn new(message: _, (source): [_]) -> Value {
    let source = source.unwrap_or_else(|| message.clone());

    let message = traits::to_string(message).await?;
    ergo_runtime::diagnostic! {
        message: message
    }
    .add_value_info("source", &source)
    .await
    .into_error()
    .into()
}
