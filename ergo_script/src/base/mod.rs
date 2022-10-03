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
/// Change the identity of a value.
///
/// Arguments: `:value`
///
/// Keyed Arguments:
/// * `:set` - if present, the resulting value will copy the identity of the value specified.
/// * `into Bool |> :eval` - if present, the resulting value will be marked (based on the `Bool`)
/// such that it will (`true`) or won't (`false`) be further evaluated when the identity is
/// calculated. Note that this will only apply a `true` setting if the value is _not_ yet
/// evaluated; use the value `force` to override this behavior.
///
/// Returns the altered value.
pub async fn id(mut v: _, (eval): [_], (set): [_]) -> Value {
    if let Some(set) = set {
        v.set_identity(set);
    }
    if let Some(mut eval) = eval {
        ergo_runtime::Context::eval(&mut eval).await?;
        let force_eval = eval
            .as_ref::<types::String>()
            .map(|t| t.as_str() == "force")
            .unwrap_or(false);
        let eval_for_id = traits::into::<types::Bool>(eval).await?.into_owned().0;
        let new_setting = if force_eval || (eval_for_id && !v.is_evaluated()) {
            Some(true)
        } else if !eval_for_id {
            Some(false)
        } else {
            None
        };
        if let Some(eval_for_id) = new_setting {
            let mut id = v.immediate_id().await;
            id.eval_for_id = eval_for_id;
            v.set_identity(id);
        }
    }
    v
}
