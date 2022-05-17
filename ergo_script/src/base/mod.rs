//! Base environment.

use ergo_runtime::{traits, types, Value};

mod doc;
mod load;

pub use doc::*;
pub use load::*;

#[types::ergo_fn]
/// The 'fn' binding function, which takes all arguments and returns an Args to be bound.
pub async fn fn_(...) -> Value {
    types::Args { args: REST }.into()
}

#[types::ergo_fn]
/// The 'index' function, which supports binding to Index values.
pub async fn index(binding: _) -> Value {
    types::Index(binding).into()
}

#[types::ergo_fn]
/// Bind the first argument using the value of the second argument.
pub async fn bind(to: _, from: _) -> Value {
    traits::bind(to, from).await
}

/// An `Unset` value.
pub fn unset() -> Value {
    types::Unset.into()
}

#[types::ergo_fn]
#[eval_for_id]
/// Mark the given value as pertinent to the identity of the expression.
///
/// Arguments: `:value`
///
/// This means that the given value will be evaluated when the identity is needed.
pub async fn eval_for_id(mut v: _) -> Value {
    drop(ergo_runtime::Context::eval(&mut v).await);
    v
}

#[types::ergo_fn]
/// Mark the given value as not pertinent to the identity of the expression.
///
/// Arguments: `:value`
///
/// The given value will _still_ be evaluated, but if it still indicates that it is pertinent to
/// the identity, the propagation of this indication will cease.
///
/// For example, `(!no-id $!id) abc` will effectively disable the properties of `!id` on `abc`.
pub async fn no_eval_for_id(v: _) -> Value {
    let mut id = ergo_runtime::value::ValueId::immediate(ergo_runtime::depends![v]).await;
    id.set_eval_id(false);
    Value::dynamic(move || async move { v }, id)
}
