//! The IntoTyped grease trait.

use abi_stable::{
    std_types::{ROption, RString},
    StableAbi,
};
use grease::path::PathBuf;
use grease::runtime::Traits;
use grease::traits::*;
use grease::type_erase::Erased;
use grease::types::{GreaseType, Type};
use grease::value::{IntoValue, TypedValue, Value};

#[derive(GreaseTrait, StableAbi)]
#[repr(C)]
pub struct IntoTyped<T: StableAbi + 'static> {
    into: extern "C" fn(Value) -> Value,
    _phantom: std::marker::PhantomData<extern "C" fn() -> *const T>,
}

impl<T: GreaseType + StableAbi + 'static> IntoTyped<T> {
    pub fn into_typed(&self, v: Value) -> TypedValue<T> {
        (self.into)(v).typed::<T>().unwrap()
    }

    /// Add an implementation of the IntoTyped grease trait.
    pub fn add_impl<U>(traits: &mut Traits)
    where
        U: GreaseType + Send + Sync + Clone + 'static,
        T: From<U> + GreaseType + Send + Sync + 'static,
    {
        extern "C" fn do_conversion<T, U>(v: Value) -> Value
        where
            U: GreaseType + Send + Sync + Clone + 'static,
            T: From<U> + GreaseType + Send + Sync + 'static,
        {
            v.typed::<U>().unwrap().map(|v| T::from(v.clone())).into()
        }

        traits.add_impl_for_type::<U, IntoTyped<T>>(IntoTyped {
            into: do_conversion::<T, U>,
            _phantom: Default::default(),
        })
    }
}

pub fn traits(traits: &mut Traits) {
    // Identity: T -> T
    {
        extern "C" fn id(_traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            extern "C" fn id_into(v: Value) -> Value {
                v
            }
            if trt.id == grease_trait_uuid(b"so_types::traits::IntoTyped") {
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
        unsafe { traits.add_generator(id) };
    }

    // () -> bool (false)
    {
        extern "C" fn conv(v: Value) -> Value {
            v.then(false.into_value())
        }
        traits.add_impl_for_type::<(), IntoTyped<bool>>(IntoTyped {
            into: conv,
            _phantom: Default::default(),
        });
    }

    // TODO ExitStatus -> bool (ExitStatus::success())

    // Path -> String
    {
        extern "C" fn conv(v: Value) -> Value {
            v.typed::<PathBuf>()
                .unwrap()
                .map(|v| RString::from(v.as_ref().as_ref().to_string_lossy().into_owned()))
                .into()
        }
        traits.add_impl_for_type::<PathBuf, IntoTyped<RString>>(IntoTyped {
            into: conv,
            _phantom: Default::default(),
        });
    }
}
