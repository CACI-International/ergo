//! Grease types, which have a UUID identifier and optional additional data.

use crate::type_erase::ErasedTrivial;
use crate::uuid::*;
use abi_stable::StableAbi;
use lazy_static::lazy_static;
use std::convert::TryInto;

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

impl From<Type> for ErasedTrivial {
    fn from(tp: Type) -> Self {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.extend(tp.id.as_bytes());
        // TODO improve this API
        bytes.extend(tp.data.into_raw_bytes().into_iter());
        ErasedTrivial::from_slice(bytes.into_boxed_slice())
    }
}

impl From<ErasedTrivial> for Type {
    fn from(e: ErasedTrivial) -> Self {
        let bytes = unsafe { e.to_boxed_slice::<u8>() };
        let id = Uuid::from_bytes(bytes[..16].try_into().expect("invalid erased type"));
        let data =
            unsafe { ErasedTrivial::from_raw_bytes(bytes[16..].to_vec().into_boxed_slice()) };
        Type { id, data }
    }
}
