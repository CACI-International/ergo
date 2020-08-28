//! Grease types, which have a UUID identifier and optional additional data.

use crate::path::PathBuf;
use crate::type_erase::ErasedTrivial;
use crate::uuid::*;
use abi_stable::{
    std_types::{RString, RVec},
    StableAbi,
};
use lazy_static::lazy_static;
use std::convert::TryInto;

pub use grease_macro::GreaseType;

lazy_static! {
    /// The type namespace UUID.
    pub static ref NAMESPACE_TYPE: Uuid = grease_uuid(b"type");
}

/// Create a new type Uuid with the given string digest.
pub fn grease_type_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*NAMESPACE_TYPE, name)
}

/// A grease type.
///
/// The type is composed of an identifier and optional type-specific data.
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
pub struct Type {
    /// The identifier for the type.
    pub id: Uuid,
    /// Optional type data.
    pub data: ErasedTrivial,
}

impl Type {
    /// Create a new Type.
    pub fn new(id: Uuid) -> Self {
        Self::with_data(id, Default::default())
    }

    /// Create a new Type with the given name.
    ///
    /// Uses `grease_type_uuid` to generate a type id from the given name.
    pub fn named(name: &[u8]) -> Self {
        Self::new(grease_type_uuid(name))
    }

    /// Create a new Type with the given id and additional data.
    pub fn with_data(id: Uuid, data: ErasedTrivial) -> Self {
        Type { id, data }
    }
}

/// A trait for rust types that have associated grease `Type`s.
pub trait GreaseType {
    /// Return the associated grease `Type`.
    fn grease_type() -> Type;
}

#[derive(Clone, Debug, Default)]
pub struct TypeParameters(pub Vec<Type>);

impl From<TypeParameters> for ErasedTrivial {
    fn from(params: TypeParameters) -> Self {
        let mut bytes: Vec<u8> = Vec::new();
        for tp in params.0 {
            ErasedTrivial::from(tp).serialize(&mut bytes).unwrap();
        }
        ErasedTrivial::from_slice(bytes.into_boxed_slice())
    }
}

impl From<ErasedTrivial> for TypeParameters {
    fn from(e: ErasedTrivial) -> Self {
        let mut ret = TypeParameters::default();
        let bytes = unsafe { e.to_boxed_slice::<u8>() };
        let mut remaining: &[u8] = &bytes;
        while !remaining.is_empty() {
            ret.0.push(
                ErasedTrivial::deserialize(&mut remaining)
                    .expect("invalid serialized type")
                    .into(),
            );
        }
        ret
    }
}

impl From<Type> for ErasedTrivial {
    fn from(tp: Type) -> Self {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.extend(tp.id.as_bytes());
        tp.data.serialize(&mut bytes).unwrap();
        ErasedTrivial::from_slice(bytes.into_boxed_slice())
    }
}

impl From<ErasedTrivial> for Type {
    fn from(e: ErasedTrivial) -> Self {
        let bytes = unsafe { e.to_boxed_slice::<u8>() };
        let id = Uuid::from_bytes(bytes[..16].try_into().expect("invalid erased type"));
        let mut slice = &bytes[16..];
        let data = ErasedTrivial::deserialize(&mut slice).expect("invalid serialized type data");
        Type { id, data }
    }
}

// Primitive types
macro_rules! impl_grease_type {
    ( $name:ident , $t:ty ) => {
        impl GreaseType for $t {
            fn grease_type() -> Type {
                Type::named(concat!["std::", stringify!($name)].as_bytes())
            }
        }
    };

    ( $t:ident ) => {
        impl_grease_type!($t, $t);
    };
}

impl_grease_type!(unit, ());
impl_grease_type!(u8);
impl_grease_type!(i8);
impl_grease_type!(u16);
impl_grease_type!(i16);
impl_grease_type!(u32);
impl_grease_type!(i32);
impl_grease_type!(u64);
impl_grease_type!(i64);
impl_grease_type!(usize);
impl_grease_type!(isize);
impl_grease_type!(char);
impl_grease_type!(bool);

impl<T: GreaseType> GreaseType for RVec<T> {
    fn grease_type() -> Type {
        Type::with_data(
            grease_type_uuid(b"abi_stable::std_types::RVec"),
            TypeParameters(vec![T::grease_type()]).into(),
        )
    }
}

impl GreaseType for RString {
    fn grease_type() -> Type {
        Type::named(b"abi_stable::std_types::RString")
    }
}

impl GreaseType for PathBuf {
    fn grease_type() -> Type {
        Type::named(b"grease::path::PathBuf")
    }
}
