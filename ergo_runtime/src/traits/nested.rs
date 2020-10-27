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

    impl Nested for types::Map {
        async fn nested(&self) -> RVec<Value> {
            self.0.iter().map(|t| t.1).cloned().collect()
        }
    }
}

#[derive(Debug)]
struct NoNested;

impl std::fmt::Display for NoNested {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "no nested value")
    }
}

impl std::error::Error for NoNested {}

impl NoNested {
    pub fn handle<T: Default>(r: grease::Result<T>) -> grease::Result<T> {
        match r {
            Ok(v) => Ok(v),
            Err(e) => {
                if Self::within(e.error_ref()) {
                    Ok(Default::default())
                } else {
                    Err(e)
                }
            }
        }
    }

    fn within(e: &(dyn std::error::Error + 'static)) -> bool {
        match grease::error::downcast_ref::<NoNested>(e) {
            Some(_) => true,
            None => match e.source() {
                Some(e) => Self::within(e),
                None => false,
            },
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
                .get_trait::<Nested, _, _>(&v, |_| async { NoNested.into() })
                .await;
            NoNested::handle(match nested {
                Ok(mut n) => n.nested(v).await,
                Err(e) => Err(e),
            })
        }
        .boxed(),
    );
}

/// Force all Values, and all recursive Values from the Nested grease trait.
pub async fn force_value_nested(ctx: &Context, v: Value) -> grease::Result<()> {
    let mut values = FuturesUnordered::new();

    add_value(&mut values, ctx, v);
    while let Some(vals) = values.try_next().await? {
        for v in vals {
            add_value(&mut values, ctx, v);
        }
    }
    Ok(())
}
