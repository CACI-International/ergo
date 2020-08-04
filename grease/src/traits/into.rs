//! The IntoTyped grease trait.

use crate::path::PathBuf as AbiPathBuf;
use crate::traits::*;
use crate::types::{GreaseType, Type};
use crate::value::{IntoValue, TypedValue, Value};
use abi_stable::{
    std_types::{ROption, RString},
    StableAbi,
};

#[derive(StableAbi)]
#[repr(C)]
pub struct IntoTyped<T> {
    into: extern "C" fn(Value) -> Value,
    _phantom: std::marker::PhantomData<extern "C" fn() -> T>,
}

impl<T: GreaseType + StableAbi + 'static> GreaseTrait for IntoTyped<T> {
    fn grease_trait() -> Trait {
        Trait::with_data(
            grease_trait_uuid(b"std::IntoTyped"),
            T::grease_type().into(),
        )
    }
}

pub fn impl_into<T, U>(builder: &mut crate::runtime::TraitsBuilder)
where
    T: GreaseType + Send + Sync + Clone + 'static,
    U: From<T> + GreaseType + Send + Sync + StableAbi + 'static,
{
    extern "C" fn do_conversion<T, U>(v: Value) -> Value
    where
        T: GreaseType + Send + Sync + Clone + 'static,
        U: From<T> + GreaseType + Send + Sync + 'static,
    {
        v.typed::<T>().unwrap().map(|v| U::from(v.clone())).into()
    }

    builder.add_impl_for_type::<T, IntoTyped<U>>(IntoTyped {
        into: do_conversion::<T, U>,
        _phantom: Default::default(),
    })
}

impl<T: GreaseType> IntoTyped<T> {
    pub fn into_typed(&self, v: Value) -> TypedValue<T> {
        (self.into)(v).typed::<T>().unwrap()
    }
}

pub fn traits(builder: &mut crate::runtime::TraitsBuilder) {
    // Identity: T -> T
    {
        extern "C" fn id(
            _iface: &crate::runtime::TraitsInterface,
            tp: &Type,
            trt: &Trait,
        ) -> ROption<Erased> {
            extern "C" fn id_into(v: Value) -> Value {
                v
            }
            if trt.id == grease_trait_uuid(b"std::IntoTyped") {
                let trait_type: Type = trt.data.clone().into();
                if tp == &trait_type {
                    return ROption::RSome(Erased::new(IntoTyped::<()> {
                        into: id_into,
                        _phantom: Default::default(),
                    }));
                }
            }
            ROption::RNone
        }
        unsafe { builder.add_generator(id) };
    }

    // () -> bool (false)
    {
        extern "C" fn conv(v: Value) -> Value {
            v.then(false.into_value())
        }
        builder.add_impl_for_type::<(), IntoTyped<bool>>(IntoTyped {
            into: conv,
            _phantom: Default::default(),
        });
    }

    // TODO ExitStatus -> bool (ExitStatus::success())

    // PathBuf -> String
    {
        extern "C" fn conv(v: Value) -> Value {
            v.typed::<AbiPathBuf>()
                .unwrap()
                .map(|v| RString::from(v.owned().into_pathbuf().to_string_lossy().into_owned()))
                .into()
        }
        builder.add_impl_for_type::<AbiPathBuf, IntoTyped<RString>>(IntoTyped {
            into: conv,
            _phantom: Default::default(),
        });
    }
}
