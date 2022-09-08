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

#[types::ergo_fn]
#[eval_for_id]
/// Set late bindings of a value.
///
/// Arguments: `(Map :bindings) :value`
///
/// Returns the value with late bindings applied.
pub async fn late_bind(bindings: types::Map, mut v: _) -> Value {
    let mut scope = ergo_runtime::value::LateScope::default();
    for (k, v) in &bindings.as_ref().0 {
        scope.scope.insert((*k.id()).into(), v.clone());
    }
    let mut bound = Default::default();
    let mut context = ergo_runtime::value::LateBindContext {
        scope: &scope,
        bound: &mut bound,
    };
    v.late_bind(&mut context);
    v
}

/// An `Unset` value.
pub fn unset() -> Value {
    types::Unset.into()
}

#[types::ergo_fn]
#[eval_for_id]
/// Evaluate the given value when calculating the identity of the expression.
///
/// Arguments: `:value`
///
/// Keyed Arguments:
/// * `into Bool |> :set` - if present, the resulting value will be marked (based on the `Bool`)
/// such that it forcibly will (`true`) or won't (`false`) cause expressions containing the value
/// to be further evaluated when the identity is calculated.
///
/// Returns the evaluated value, possibly with the identity calculating semantics changed (based on
/// `set`).
pub async fn eval_for_id(mut v: _, (set): [_]) -> Value {
    drop(ergo_runtime::Context::eval(&mut v).await);
    if let Some(set) = set {
        let eval_for_id = traits::into::<types::Bool>(set).await?.into_owned().0;
        let mut id = v.immediate_id().await;
        id.eval_for_id = eval_for_id;
        v.set_identity(id);
    }
    v
}
