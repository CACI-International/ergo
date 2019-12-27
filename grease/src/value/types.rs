//! GetValueType implementations for std types.

use super::{GetValueType, TypedValue, Value, ValueType};
use crate::uuid::*;
use crate::{Trait, TraitImpl, TraitType};

// Primitive types

macro_rules! impl_prim {
    ( $t:ty ) => {
        impl GetValueType for $t {
            fn value_type() -> ValueType {
                ValueType::new(type_uuid(concat!["std::", stringify!($t)].as_bytes()))
            }
        }
    };
}

macro_rules! impl_simple {
    ( $t:ty ) => {
        impl GetValueType for $t {
            fn value_type() -> ValueType {
                ValueType::new(type_uuid(stringify!($t).as_bytes()))
            }
        }
    };
}

impl GetValueType for () {
    fn value_type() -> ValueType {
        ValueType::new(type_uuid(b"std::unit"))
    }
}

impl_prim!(u8);
impl_prim!(i8);
impl_prim!(u16);
impl_prim!(i16);
impl_prim!(u32);
impl_prim!(i32);
impl_prim!(u64);
impl_prim!(i64);
impl_prim!(u128);
impl_prim!(i128);
impl_prim!(usize);
impl_prim!(isize);
impl_prim!(char);
impl_prim!(bool);

// Complex types

impl_simple!(std::string::String);
impl_simple!(std::path::PathBuf);
impl_simple!(std::process::ExitStatus);

impl<T: GetValueType> GetValueType for Vec<T> {
    fn value_type() -> ValueType {
        ValueType::with_data(
            type_uuid(b"std::vec::Vec"),
            T::value_type().id.as_bytes().as_ref().into(),
        )
    }
}

pub struct IntoTrait<T> {
    pub into: fn(Value) -> TypedValue<T>,
}

impl<T: GetValueType> Trait for IntoTrait<T> {
    fn trait_type() -> TraitType {
        let to = T::value_type();
        let mut data = Vec::new();
        data.extend(to.id.as_bytes());
        data.extend(to.data);
        TraitType::with_data(type_uuid(b"grease::Into"), data)
    }
}

pub fn impl_into<T, U>() -> IntoTrait<U>
where
    T: GetValueType + Send + Sync + Clone + 'static,
    U: From<T> + GetValueType,
{
    IntoTrait {
        into: |v| v.typed::<T>().unwrap().map(|v| Ok(U::from(v.clone()))),
    }
}

/*
pub(crate) fn trait_generator(tp: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    vec![IntoTrait {
        into: |v| v.typed().unwrap()
    }.into()]
}
*/
