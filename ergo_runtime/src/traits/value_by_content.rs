//! The ValueByContent ergo trait.

use super::{type_name, NestedValues};
use crate as ergo_runtime;
use crate::abi_stable::type_erase::Eraseable;
use crate::context::Traits;
use crate::type_system::{ergo_trait, ergo_trait_impl, ErgoType};
use crate::{dependency::GetDependencies, Value};
use log::debug;

/// An ergo trait which identifies a value by its content.
#[ergo_trait]
pub trait ValueByContent {
    async fn value_by_content(self, deep: bool) -> Value;
}

impl ValueByContent {
    /// Implement ValueByContent for the given type.
    pub fn add_impl<T: ErgoType + GetDependencies + Eraseable + Clone>(traits: &Traits) {
        traits.add_impl_for_type::<T, ValueByContent>(ergo_trait_impl! {
            impl<T: ErgoType + GetDependencies + Eraseable + Clone> ValueByContent for T {
                async fn value_by_content(self, _deep: bool) -> Value {
                    Value::constant(self.to_owned())
                }
            }
        });
    }

    /// Implement ValueByContent for the given type, taking into account nested values.
    pub fn add_nested_impl<T: ErgoType + GetDependencies + NestedValues + Eraseable + Clone>(
        traits: &Traits,
    ) {
        traits.add_impl_for_type::<T, ValueByContent>(ergo_trait_impl! {
            impl<T: ErgoType + GetDependencies + NestedValues + Eraseable + Clone> ValueByContent for T {
                async fn value_by_content(self, deep: bool) -> Value {
                    let mut v = self.to_owned();
                    if deep {
                        CONTEXT.task.join_all(v.nested_values_mut().into_iter().map(|v| async move {
                            let old_v = std::mem::replace(v, crate::types::Unset.into());
                            *v = super::value_by_content(CONTEXT, old_v, deep).await;
                            Ok(())
                        })).await.unwrap();
                    }
                    Value::constant(v)
                }
            }
        });
    }
}

pub async fn value_by_content(ctx: &crate::Context, mut v: Value, deep: bool) -> Value {
    drop(ctx.eval(&mut v).await);
    match ctx.get_trait::<ValueByContent>(&v) {
        None => {
            debug!(
                "no ValueByContent trait for {}, returning the value as-is",
                type_name(ctx, &v)
            );
            v
        }
        Some(t) => t.value_by_content(ctx, v, deep).await,
    }
}
