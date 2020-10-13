//! The ValueByContent grease trait.

use crate::{types, ResultIterator};
use abi_stable::{
    std_types::{RArc, ROption, RResult, RVec},
    StableAbi,
};
use grease::{
    depends,
    future::BoxFuture,
    path::PathBuf,
    runtime::{Context, Traits},
    traits::GreaseTrait,
    type_erase::Erased,
    types::GreaseType,
    value::{Error, Value},
};

/// A grease trait which identifies a value by its content.
#[derive(Clone, GreaseTrait, StableAbi)]
#[repr(C)]
pub struct ValueByContent {
    value_by_content: for<'a> extern "C" fn(
        &'a Context,
        Value,
        &'a RArc<Erased>,
    ) -> BoxFuture<'a, RResult<Value, Error>>,
}

impl ValueByContent {
    /// Get the value identified by content.
    pub async unsafe fn value_by_content_unsafe(
        &self,
        ctx: &Context,
        v: Value,
        data: &RArc<Erased>,
    ) -> Result<Value, Error> {
        (self.value_by_content)(ctx, v, data).await.into()
    }

    /// Get the given value by content.
    /// The value _must_ have already been forced (and likewise for nested values).
    pub async fn value_by_content(&self, ctx: &Context, v: Value) -> Result<Value, Error> {
        unsafe { self.value_by_content_unsafe(ctx, v.clone(), &v.forced_value()) }.await
    }

    /// Implement ValueByContent for the given type.
    pub fn add_impl<T: GreaseType + std::hash::Hash + Sync>(traits: &mut Traits) {
        extern "C" fn value_by_content<'a, T: std::hash::Hash + Sync>(
            _: &'a Context,
            v: Value,
            data: &'a RArc<Erased>,
        ) -> BoxFuture<'a, RResult<Value, Error>> {
            BoxFuture::new(async move {
                let deps = depends![unsafe { data.as_ref().as_ref::<T>() }];
                RResult::ROk(v.set_dependencies(deps))
            })
        }

        traits.add_impl_for_type::<T, ValueByContent>(ValueByContent {
            value_by_content: value_by_content::<T>,
        });
    }

    /// Create a new ValueByContent implementation with the given function.
    pub fn new(
        value_by_content: for<'a> extern "C" fn(
            &'a Context,
            Value,
            &'a RArc<Erased>,
        ) -> BoxFuture<'a, RResult<Value, Error>>,
    ) -> Self {
        ValueByContent { value_by_content }
    }
}

pub fn traits(traits: &mut Traits) {
    ValueByContent::add_impl::<types::Unit>(traits);
    ValueByContent::add_impl::<types::String>(traits);
    ValueByContent::add_impl::<bool>(traits);
    ValueByContent::add_impl::<PathBuf>(traits);

    // types::Array
    {
        extern "C" fn value_by_content<'a>(
            ctx: &'a Context,
            _: Value,
            data: &'a RArc<Erased>,
        ) -> BoxFuture<'a, RResult<Value, Error>> {
            BoxFuture::new(async move {
                let types::Array(vals) = unsafe { data.as_ref().as_ref::<types::Array>() };
                let vals: Result<RVec<_>, Error> = vals
                    .iter()
                    .map(|v| match ctx.traits.get::<ValueByContent>(v) {
                        Some(t) => Ok(async move { t.value_by_content(ctx, v.clone()).await }),
                        None => Err(format!(
                            "ValueByContent not implemented for {}",
                            super::type_name(&ctx.traits, v.grease_type().as_ref())
                        )
                        .into()),
                    })
                    .collect_result();
                let vals = match vals {
                    Ok(v) => v,
                    Err(e) => return RResult::RErr(e),
                };
                futures::future::join_all(vals)
                    .await
                    .into_iter()
                    .collect_result()
                    .map(|v| types::Array(v).into())
                    .into()
            })
        }

        traits
            .add_impl_for_type::<types::Array, ValueByContent>(ValueByContent { value_by_content });
    }

    // types::Map
    {
        extern "C" fn value_by_content<'a>(
            ctx: &'a Context,
            _: Value,
            data: &'a RArc<Erased>,
        ) -> BoxFuture<'a, RResult<Value, Error>> {
            BoxFuture::new(async move {
                let types::Map(vals) = unsafe { data.as_ref().as_ref::<types::Map>() };
                let vals: Result<Vec<_>, Error> =
                    vals.iter()
                        .map(|(k, v)| match ctx.traits.get::<ValueByContent>(v) {
                            Some(t) => Ok(async move {
                                Ok((k.clone(), t.value_by_content(ctx, v.clone()).await?))
                            }),
                            None => Err(format!(
                                "ValueByContent not implemented for {}",
                                super::type_name(&ctx.traits, &v.grease_type())
                            )
                            .into()),
                        })
                        .collect_result();
                let vals = match vals {
                    Ok(v) => v,
                    Err(e) => return RResult::RErr(e),
                };
                futures::future::join_all(vals)
                    .await
                    .into_iter()
                    .collect_result()
                    .map(|vals| types::Map(vals).into())
                    .into()
            })
        }

        traits.add_impl_for_type::<types::Map, ValueByContent>(ValueByContent { value_by_content });
    }

    // types::Either
    {
        extern "C" fn value_by_content<'a>(
            ctx: &'a Context,
            _: Value,
            data: &'a RArc<Erased>,
        ) -> BoxFuture<'a, RResult<Value, Error>> {
            BoxFuture::new(async move {
                let e = unsafe { data.as_ref().as_ref::<types::Either>() };
                let v = e.value();
                match ctx.traits.get::<ValueByContent>(&v) {
                    Some(t) => t.value_by_content(ctx, v).await,
                    None => Err(format!(
                        "ValueByContent not implemented for {}",
                        super::type_name(&ctx.traits, &v.grease_type())
                    )
                    .into()),
                }
                .into()
            })
        }

        traits.add_generator_by_trait_for_trait(|_traits, tp| {
            if !types::Either::matches_grease_type(tp) {
                ROption::RNone
            } else {
                ROption::RSome(ValueByContent { value_by_content })
            }
        });
    }
}
