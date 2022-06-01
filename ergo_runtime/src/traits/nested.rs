//! The Nested ergo trait.
//!
//! This trait is used to expose nested Values within Value types.

use crate as ergo_runtime;
use crate::abi_stable::std_types::RVec;
use crate::context::Traits;
use crate::type_system::{ergo_trait, ergo_trait_impl, ErgoType};
use crate::{Context, Value};
use futures::future::{BoxFuture, FutureExt};
use futures::stream::{futures_unordered::FuturesUnordered, StreamExt};

/// An ergo trait which exposes nested Values within a single Value's data.
///
/// This essentially requires the type to be a functor.
#[ergo_trait]
pub trait Nested {
    async fn nested(&self) -> RVec<Value>;
}

/// Get the nested values for a type.
pub trait NestedValues {
    fn nested_values(&self) -> Vec<&Value>;
    fn nested_values_mut(&mut self) -> Vec<&mut Value>;
}

impl Nested {
    /// Implement Nested for the given type.
    pub fn add_impl<T: NestedValues + ErgoType>(traits: &Traits) {
        traits.add_impl_for_type::<T, Nested>(ergo_trait_impl! {
            impl<T: NestedValues + ErgoType> Nested for T {
                async fn nested(&self) -> RVec<Value> {
                    self.nested_values().into_iter().cloned().collect()
                }
            }
        });
    }
}

fn add_value<'a>(
    values: &mut FuturesUnordered<BoxFuture<'a, crate::Result<RVec<Value>>>>,
    mut v: Value,
) {
    values.push(
        async move {
            Context::eval(&mut v).await?;
            Ok(match Context::get_trait::<Nested>(&v) {
                None => Default::default(),
                Some(n) => n.nested(v).await,
            })
        }
        .boxed(),
    );
}

/// Recursively evaluate the given value.
pub async fn eval_nested(v: Value) -> crate::Result<()> {
    let mut values = FuturesUnordered::new();
    let mut errs = vec![];

    add_value(&mut values, v);
    while let Some(result) = values.next().await {
        match result {
            Ok(vals) => {
                for v in vals {
                    add_value(&mut values, v);
                }
            }
            Err(e) => errs.push(e),
        }
    }
    if errs.is_empty() {
        Ok(())
    } else {
        Err(crate::Error::aggregate(errs))
    }
}
