//! The IntoTyped grease trait.

use crate::uuid::*;
use crate::value::{GetValueType, IntoValue, TypedValue, Value, ValueType};
use crate::{match_value_type, CreateTrait, Trait, TraitImpl, TraitImplRef, TraitType, Traits};

pub struct IntoTrait {
    tp: std::sync::Arc<ValueType>,
    into: fn(Value) -> Value,
}

impl From<IntoTrait> for TraitImpl {
    fn from(v: IntoTrait) -> Self {
        TraitImpl::new(into_trait_type(v.tp.as_ref().clone()), v.into)
    }
}

fn into_trait_type(to: ValueType) -> TraitType {
    let mut data = Vec::new();
    data.extend(to.id.as_bytes());
    data.extend(to.data);
    TraitType::with_data(type_uuid(b"grease::IntoTyped"), data)
}

pub struct IntoTyped<T> {
    into: fn(Value) -> Value,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GetValueType> IntoTyped<T> {
    pub fn into_typed(&self, v: Value) -> TypedValue<T> {
        (self.into)(v).typed::<T>().unwrap()
    }
}

impl<T: GetValueType> Trait for IntoTyped<T> {
    fn trait_type() -> TraitType {
        into_trait_type(T::value_type())
    }
}

impl<T: GetValueType> CreateTrait for IntoTyped<T> {
    type Storage = fn(Value) -> Value;

    fn create(
        _traits: Traits,
        _value_type: std::sync::Arc<ValueType>,
        imp: TraitImplRef<Self::Storage>,
    ) -> Self {
        Self {
            into: *imp,
            _phantom: Default::default(),
        }
    }
}

pub fn impl_into<T, U>() -> TraitImpl
where
    T: GetValueType + Send + Sync + Clone + 'static,
    U: From<T> + GetValueType,
{
    TraitImpl::for_trait::<IntoTyped<U>>(|v| {
        v.typed::<T>().unwrap().map(|v| U::from(v.clone())).into()
    })
}

pub(crate) fn trait_generator(tp: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    let mut b = vec![IntoTrait {
        into: std::convert::identity,
        tp: tp.clone(),
    }
    .into()];

    match_value_type!(*tp => {
        () => {
            // () should convert to a bool (always false)
            b.push(
                IntoTrait {
                    into: |_| false.into_value(),
                    tp: bool::value_type().into(),
                }
                .into(),
            );
        },
        std::process::ExitStatus => {
            // ExitStatus should convert to bool, true if the status was successful
            b.push(
                IntoTrait {
                    into: |v| v.typed::<std::process::ExitStatus>().unwrap().map(|v| v.success()).into(),
                    tp: bool::value_type().into(),
                }.into()
            );
        },
        std::path::PathBuf => {
            // PathBuf should convert to String
            b.push(
                IntoTrait {
                    into: |v| v.typed::<std::path::PathBuf>().unwrap().map(|v| v.to_string_lossy().into_owned()).into(),
                    tp: String::value_type().into(),
                }.into()
            );
        },
        => ()
    });
    b
}
