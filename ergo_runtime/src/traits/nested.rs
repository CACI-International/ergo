//! The Nested grease trait.
//!
//! This trait is used to expose nested Values within Value types.

use crate::types;
use abi_stable::{std_types::RVec, StableAbi};
use futures::future::{BoxFuture, FutureExt};
use futures::stream::{futures_unordered::FuturesUnordered, TryStreamExt};
use grease::{grease_trait, grease_traits_fn, runtime::Context, value::Value};

/// A grease trait which exposes nested Values within a single Value's data.
#[grease_trait]
pub trait Nested {
    async fn nested(&self) -> RVec<Value>;
}

grease_traits_fn! {
    impl Nested for types::Array {
        async fn nested(&self) -> RVec<Value> {
            self.0.iter().cloned().collect()
        }
    }

    impl Nested for types::Iter {
        async fn nested(&self) -> RVec<Value> {
            self.clone().collect()
        }
    }

    impl Nested for types::Map {
        async fn nested(&self) -> RVec<Value> {
            self.0.iter().map(|t| t.1).cloned().collect()
        }
    }
}

fn add_value<'a>(
    values: &mut FuturesUnordered<BoxFuture<'a, grease::Result<RVec<Value>>>>,
    ctx: &'a Context,
    v: Value,
) {
    values.push(
        async move {
            let nested = ctx
                .get_trait::<Nested, _, _>(&v, |_| async {
                    Ok(grease::grease_trait_impl! {
                        impl Nested for _ {
                            async fn nested(&self) -> RVec<Value> {
                                Default::default()
                            }
                        }
                    })
                })
                .await;
            match nested {
                Ok(mut n) => n.nested(v).await,
                Err(e) => Err(e),
            }
        }
        .boxed(),
    );
}

/// Force all Values, and all recursive Values from the Nested grease trait.
pub async fn force_value_nested(ctx: &Context, v: Value) -> grease::Result<()> {
    let mut values = FuturesUnordered::new();
    let mut errs = vec![];

    add_value(&mut values, ctx, v);
    loop {
        match values.try_next().await {
            Ok(Some(vals)) => {
                for v in vals {
                    add_value(&mut values, ctx, v);
                }
            }
            Ok(None) => break,
            Err(e) => {
                if ctx.task.aggregate_errors() {
                    errs.push(e);
                } else {
                    return Err(e);
                }
            }
        }
    }
    if errs.len() == 1 {
        Err(errs.into_iter().next().unwrap())
    } else if errs.len() > 1 {
        Err(errs.into_iter().collect())
    } else {
        Ok(())
    }
}
