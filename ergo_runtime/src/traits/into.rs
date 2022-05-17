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
use crate::dependency::GetDependencies;
use crate::depends;
use crate::error::DiagnosticInfo;
use crate::metadata::Source;
use crate::type_system::{
    ergo_trait, ergo_trait_impl, ergo_traits_fn, ErgoTrait, ErgoType, Ref, Trait, Type,
    TypeParameters,
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
        T: From<U> + ErgoType + Eraseable + GetDependencies,
    {
        traits.add_impl_for_type::<U, IntoTyped<T>>(ergo_trait_impl! {
            impl<T, U> IntoTyped<T> for U
                where T: From<U> + ErgoType + Eraseable + GetDependencies,
                      U: ErgoType + Eraseable + Clone,
            {
                async fn into_typed(self) -> Value {
                    Value::evaluated(T::from(self.to_owned()))
                }
            }
        });
    }

    /// Add an implementation of the IntoTyped ergo trait, where the produced value has
    /// dependencies based on the `from` value.
    pub fn add_depending_impl<U>(traits: &Traits)
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
                    let deps: crate::Dependencies = depends![self];
                    Value::with_id(T::from(self.to_owned()), deps)
                }
            }
        });
    }
}

/// Create an IntoTyped trait for the given type.
pub fn into_trait(tp: Type) -> Trait {
    let mut trt = IntoTyped::<crate::types::Unit>::ergo_trait();
    trt.data = TypeParameters(vec![tp].into()).into();
    trt
}

/// Convert the given value into another type.
pub async fn into<T: ErgoType + StableAbi + Eraseable>(v: Value) -> crate::Result<TypedValue<T>> {
    into_for(&T::ergo_type(), v)
        .await
        .map(|v| v.as_type::<T>().unwrap())
}

/// Convert the given value into another type.
pub async fn into_for(tp: &Type, mut v: Value) -> crate::Result<Value> {
    Context::eval(&mut v).await?;
    let src = Source::get(&v); // Update source in case it changed
    let from_t = type_name(&v);
    let trt = into_trait(tp.clone());
    let t = match Context::global()
        .traits
        .get_impl(v.ergo_type().unwrap(), &trt)
    {
        Some(t) => t,
        None => {
            let to_t = type_name_for(tp);
            return Err(crate::diagnostic! {
                message: format!("cannot convert value into {}", to_t)
            }
            .add_value_info("value", &v)
            .await
            .into());
        }
    };
    let t = IntoTyped::<crate::types::Unit>::create(unsafe { Ref::new(t) });
    let mut v = t.into_typed(v).await;
    Context::eval(&mut v).await?;
    if v.ergo_type().unwrap() != tp {
        let actual_t = type_name(&v);
        let to_t = type_name_for(tp);
        Err(crate::error! {
            labels: [
                primary(src.with(format!("value has type {}", from_t)))
            ],
            notes: [
                format!("expected {}", to_t),
                format!("got {}", actual_t)
            ],
            error: format!("bad IntoTyped<{}> implementation", to_t)
        })
    } else {
        Source::set_if_missing(&mut v, src);
        Ok(v)
    }
}

ergo_traits_fn! {
    // Identity: T -> T
    {
        extern "C" fn id_f<'a>(_data: &'a Erased, v: Value) ->
            BoxFuture<'a, Value> {
            BoxFuture::new(async move { v })
        }
        #[allow(improper_ctypes_definitions)]
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
