//! The IntoTyped grease trait.

use super::type_name;
use crate::source::Source;
use abi_stable::{
    std_types::{ROption, RString},
    StableAbi,
};
use grease::closure::*;
use grease::path::PathBuf;
use grease::runtime::{Context, Traits};
use grease::traits::*;
use grease::type_erase::Erased;
use grease::types::{GreaseType, Type, TypeParameters};
use grease::value::{IntoValue, TypedValue, Value};
use grease::*;

/// A grease trait for converting to different types.
#[grease_trait]
pub trait IntoTyped<T: StableAbi + 'static> {
    async fn into_typed(self) -> Value;
}

impl<T: GreaseType + StableAbi + 'static> IntoTyped<T> {
    /// Add an implementation of the IntoTyped grease trait.
    pub fn add_impl<U>(traits: &mut Traits)
    where
        U: GreaseType + Send + Sync + Clone + 'static,
        T: From<U> + GreaseType + Send + Sync + 'static,
    {
        traits.add_impl_for_type::<U, IntoTyped<T>>(grease_trait_impl! {
            impl<T, U> IntoTyped<T> for U
                where T: From<U> + GreaseType + Send + Sync + 'static,
                      U: GreaseType + Send + Sync + Clone + 'static
            {
                async fn into_typed(self) -> Value {
                    let v = self;
                    make_value!([v] {
                        Ok(T::from(v.await?.owned()))
                    }).into()
                }
            }
        });
    }
}

/// Convert the given value into another type.
pub async fn into_sourced<T: GreaseType + StableAbi + Send + Sync + 'static>(
    ctx: &Context,
    v: Source<Value>,
) -> grease::Result<Source<TypedValue<T>>> {
    v.map_async(|v| into::<T>(ctx, v))
        .await
        .transpose()
        .map_err(|e| e.into_grease_error())
}

/// Convert the given value into another type.
pub async fn into<T: GreaseType + StableAbi + Send + Sync + 'static>(
    ctx: &Context,
    mut v: Value,
) -> grease::Result<TypedValue<T>> {
    match ctx.get_trait::<IntoTyped<T>>(&v) {
        None => Err(format!(
            "cannot convert {} into {}",
            type_name(ctx, v.grease_type().await?).await?,
            type_name(ctx, &T::grease_type()).await?
        )
        .into()),
        Some(mut t) => {
            let ctx = ctx.clone();
            t.into_typed(v)
                .await?
                .typed::<T, _, _>(move |t| {
                    let ctx = ctx.clone();
                    let t = t.clone();
                    async move {
                        let into_t = match type_name(&ctx, &T::grease_type()).await {
                            Ok(v) => v,
                            Err(e) => return e,
                        };
                        let from_t = match type_name(&ctx, &t).await {
                            Ok(v) => v,
                            Err(e) => return e,
                        };
                        format!("bad IntoTyped<{}> implementation, got {}", into_t, from_t).into()
                    }
                })
                .await
        }
    }
}

grease_traits_fn! {
    // Identity: T -> T
    {
        extern "C" fn id_f<'a>(_ctx: &'a grease::runtime::Context, v: Value) ->
            grease::future::BoxFuture<'a, grease::error::RResult<Value>> {
            grease::future::BoxFuture::new(async move {
                grease::error::RResult::ROk(v)
            })
        }
        extern "C" fn id(_traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == IntoTyped::<()>::grease_trait().id {
                let TypeParameters(trait_types) = trt.data.clone().into();
                if tp == &trait_types[0] {
                    return ROption::RSome(Erased::new(IntoTypedImpl::<()> {
                        into_typed: FnPtr::from_fn(id_f as _),
                        _phantom0: Default::default(),
                    }));
                }
            }
            ROption::RNone
        }
        unsafe { traits.add_generator(id) };
    }

    // Anything -> bool (true)
    {
        extern "C" fn to_bool<'a>(_ctx: &'a grease::runtime::Context, v: Value) ->
            grease::future::BoxFuture<'a, grease::error::RResult<Value>> {
            grease::future::BoxFuture::new(async move {
                grease::error::RResult::ROk(v.then(true.into_value()))
            })
        }
        traits.add_generator_by_trait_for_trait::<IntoTyped<bool>>(|_traits, _type| {
            ROption::RSome(IntoTypedImpl::<bool> {
                into_typed: FnPtr::from_fn(to_bool as _),
                _phantom0: Default::default(),
            })
        });
    }

    // () -> bool (false)
    {
        traits.add_impl_for_type::<(), IntoTyped<bool>>(
            grease_trait_impl! {
                impl IntoTyped<bool> for () {
                    async fn into_typed(self) -> Value {
                        Value::from(self).then(false.into_value())
                    }
                }
            }
        );
    }

    // Path -> String
    {
        traits.add_impl_for_type::<PathBuf, IntoTyped<RString>>(
            grease_trait_impl! {
                impl IntoTyped<RString> for PathBuf {
                    async fn into_typed(self) -> Value {
                        self.map(|v| RString::from(v.as_ref().as_ref().to_string_lossy().into_owned())).into()
                    }
                }
            }
        );
    }
}
