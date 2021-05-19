//! The IntoTyped ergo trait.

use super::{type_name, type_name_for};
use crate as ergo_runtime;
use crate::abi_stable::{
    closure::*,
    future::BoxFuture,
    std_types::ROption,
    type_erase::{Eraseable, Erased},
    StableAbi,
};
use crate::context::{Context, Traits};
use crate::depends;
use crate::source::Source;
use crate::type_system::{
    ergo_trait, ergo_trait_impl, ergo_traits_fn, ErgoTrait, ErgoType, Trait, Type, TypeParameters,
};
use crate::value::{TypedValue, Value};

/// An ergo trait for converting to different types.
#[ergo_trait]
pub trait IntoTyped<T: StableAbi + 'static> {
    async fn into_typed(self) -> Value;
}

impl<T: ErgoType + StableAbi + Eraseable> IntoTyped<T> {
    /// Add an implementation of the IntoTyped ergo trait.
    pub fn add_impl<U>(traits: &Traits)
    where
        U: ErgoType + Eraseable + Clone,
        T: From<U> + ErgoType + Eraseable,
    {
        traits.add_impl_for_type::<U, IntoTyped<T>>(ergo_trait_impl! {
            impl<T, U> IntoTyped<T> for U
                where T: From<U> + ErgoType + Eraseable,
                      U: ErgoType + Eraseable + Clone,
            {
                async fn into_typed(self) -> Value {
                    let v = self;
                    // TODO use Value::constant instead?
                    let deps = depends![v];
                    Value::constant_deps(T::from(v.to_owned()), deps)
                }
            }
        });
    }
}

/// Convert the given value into another type.
pub async fn into_sourced<T: ErgoType + StableAbi + Eraseable>(
    ctx: &Context,
    v: Source<Value>,
) -> crate::Result<Source<TypedValue<T>>> {
    v.map_async(|v| into::<T>(ctx, v))
        .await
        .transpose()
        .map_err(|e| e.into_error())
}

/// Convert the given value into another type.
pub async fn into<T: ErgoType + StableAbi + Eraseable>(
    ctx: &Context,
    v: Value,
) -> crate::Result<TypedValue<T>> {
    let t = ctx.get_trait::<IntoTyped<T>>(&v).ok_or_else(|| {
        let from_t = type_name(ctx, &v);
        let to_t = type_name_for(&ctx, &T::ergo_type());
        format!("cannot convert {} into {}", from_t, to_t)
    })?;
    t.into_typed(v).await.as_type::<T>().map_err(|v| {
        let actual_t = type_name(ctx, &v);
        let into_t = type_name_for(ctx, &T::ergo_type());
        format!("bad IntoTyped<{}> implementation, got {}", into_t, actual_t).into()
    })
}

ergo_traits_fn! {
    // Identity: T -> T
    {
        extern "C" fn id_f<'a>(_data: &'a Erased, _ctx: &'a Context, v: Value) ->
            BoxFuture<'a, Value> {
            BoxFuture::new(async move { v })
        }
        extern "C" fn id(_traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == IntoTyped::<crate::types::Unit>::ergo_trait().id {
                let TypeParameters(trait_types) = trt.data.clone().into();
                if tp == &trait_types[0] {
                    return ROption::RSome(Erased::new(IntoTypedImpl::<()> {
                        into_typed: unsafe { FnPtr::new(id_f) },
                        ergo_trait_data: Default::default(),
                        _phantom0: Default::default(),
                    }));
                }
            }
            ROption::RNone
        }
        unsafe { traits.add_generator(id) };
    }
}
