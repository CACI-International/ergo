//! The Functor ergo trait.
//!
//! This trait is used to map a function over all Values within a type, retaining the structure of
//! the type.

use super::{bind, NestedValues};
use crate as ergo_runtime;
use crate::abi_stable::type_erase::Eraseable;
use crate::context::Traits;
use crate::metadata::Source;
use crate::type_system::{ergo_trait, ergo_trait_impl, ErgoType};
use crate::{try_result, try_value, Context, Value};

#[ergo_trait]
pub trait Functor {
    async fn map(self, f: Value) -> Value;
}

impl Functor {
    /// Add an implementation using the NestedValues to mutably change the contents.
    pub fn add_nested_impl<T: NestedValues + ErgoType + Eraseable + Clone + Into<Value>>(
        traits: &Traits,
    ) {
        traits.add_impl_for_type::<T, Functor>(ergo_trait_impl! {
            impl<T: NestedValues + ErgoType + Eraseable + Clone + Into<Value>> Functor for T {
                async fn map(self, f: Value) -> Value {
                    let mut me = self.into_owned();
                    try_result!(Context::global().task.join_all(
                        me.nested_values_mut().into_iter().map(|v| async {
                            *v = try_value!(bind(f.clone(), v.clone()).await);
                            Ok(())
                        })
                    ).await);
                    me.into()
                }
            }
        });
    }
}

/// Map the contents of the given value with the given function.
///
/// If the value's type is not a Functor, returns the value as-is.
///
/// Preserves the value's metadata when mapping.
pub async fn map(mut v: Value, f: Value) -> crate::Result<Value> {
    Context::eval(&mut v).await?;

    match Context::get_trait::<Functor>(&v) {
        None => Ok(v),
        Some(t) => {
            let mut ret = t.map(v.clone(), f).await;
            ret.copy_metadata(&v);

            Context::eval(&mut ret).await?;
            if ret.ergo_type().unwrap() != v.ergo_type().unwrap() {
                let actual_t = super::type_name(&ret);
                let expected_t = super::type_name(&v);
                Err(ergo_runtime::error! {
                    labels: [
                        primary(Source::get(&v).with(format!("value has type {}", expected_t)))
                    ],
                    notes: [
                        format!("expected {}", expected_t),
                        format!("got {}", actual_t)
                    ],
                    error: format!("bad Functor<{}> implementation", expected_t)
                })
            } else {
                Ok(ret)
            }
        }
    }
}

/// Evaluate a value and all inner values recursively, returning the value with inner values replaced.
pub async fn deep_eval(v: Value) -> crate::Result<Value> {
    use futures::future::{BoxFuture, FutureExt};
    fn deep_eval_impl(v: Value) -> BoxFuture<'static, crate::Result<Value>> {
        async move {
            let deep = crate::types::Unbound::new_no_doc(
                |v| async move { deep_eval_impl(v).await.into() },
                crate::depends![const crate::nsid!(Functor::deep_eval)],
            )
            .into();
            map(v, deep).await
        }
        .boxed()
    }
    deep_eval_impl(v).await
}
