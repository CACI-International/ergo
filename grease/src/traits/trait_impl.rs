//! Trait implementation storage.

use super::trait_::Trait;
use crate::type_erase::Erased;

/// The storage for trait implementations.
pub struct TraitImpl {
    trait_id: Trait,
    data: Erased,
}
