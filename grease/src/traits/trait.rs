//! Grease trait descriptors, which have a UUID identifier and optional additional data.

use crate::type_erase::{Eraseable, ErasedTrivial};
use crate::uuid::*;
use abi_stable::StableAbi;
use lazy_static::lazy_static;

lazy_static! {
    /// The trait type namespace UUID.
    pub static ref NAMESPACE_TRAIT: Uuid = grease_uuid(b"trait");
}

/// Create a new trait Uuid with the given string digest.
pub fn grease_trait_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*NAMESPACE_TRAIT, name)
}

/// A descriptor for grease traits.
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
pub struct Trait {
    /// The identifier for the trait.
    pub id: Uuid,
    /// Optional trait data.
    pub data: ErasedTrivial,
}

impl Trait {
    /// Create a new Trait.
    pub fn new(id: Uuid) -> Self {
        Self::with_data(id, Default::default())
    }

    /// Create a new Trait with the given name.
    ///
    /// Uses `grease_trait_uuid` to generate a id from the given name.
    pub fn named(name: &[u8]) -> Self {
        Self::new(grease_trait_uuid(name))
    }

    /// Create a new Trait with the given id and additional data.
    pub fn with_data(id: Uuid, data: ErasedTrivial) -> Self {
        Trait { id, data }
    }
}

/// A trait for rust types which represent implementations of grease traits.
pub trait GreaseTrait: Eraseable + StableAbi {
    /// Get the trait descriptor.
    fn grease_trait() -> Trait;
}
