//! Runtime types, which have a UUID identifier and optional additional data.

use crate::abi_stable::{std_types::RVec, type_erase::ErasedTrivial, uuid::Uuid, StableAbi};
use std::convert::TryInto;

/// A runtime type.
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

/// Match expression for Types.
///
/// Matching is based on types, not patterns, and each type must implement ErgoType. The else case
/// is required.
///
/// The evaluation context is similar to regular match expressions, in that flow-control statements
/// will pertain to the calling code.
#[macro_export]
macro_rules! match_type {
    ( $type:expr => { $( $t:ty => $e:expr $(,)? )+ => $else:expr } ) => {
        {
            let __ergo_match_type_tp: $crate::type_system::Type = $type;
            $( if __ergo_match_type_tp == <$t as $crate::type_system::ErgoType>::ergo_type() { $e } else )+ { $else }
        }
    };
}

impl Type {
    /// Create a new Type.
    pub fn new(id: Uuid) -> Self {
        Self::with_data(id, Default::default())
    }

    /// Create a new Type with the given name.
    ///
    /// Uses `nsid!` to generate a type id from the given name.
    pub fn named(name: &[u8]) -> Self {
        Self::new(crate::nsid!(type, name))
    }

    /// Create a new Type with the given id and additional data.
    pub fn with_data(id: Uuid, data: ErasedTrivial) -> Self {
        Type { id, data }
    }
}

/// A trait for rust types that have associated ergo `Type`s.
pub trait ErgoType {
    /// Return the associated ergo `Type`.
    fn ergo_type() -> Type;

    /// Return whether the Self type matches the given ergo type.
    ///
    /// The default implementation tests for total equality with `Self::ergo_type()`.
    fn matches_ergo_type(a: &Type) -> bool {
        &Self::ergo_type() == a
    }
}

#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct TypeParameters(pub RVec<Type>);

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
        let bytes = unsafe { e.as_slice::<u8>() };
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
        let bytes = unsafe { e.as_slice::<u8>() };
        let id = Uuid::from_bytes(bytes[..16].try_into().expect("invalid erased type"));
        let mut slice = &bytes[16..];
        let data = ErasedTrivial::deserialize(&mut slice).expect("invalid serialized type data");
        Type { id, data }
    }
}
