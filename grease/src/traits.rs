//! Runtime trait implementation.

use crate::{ValueData, ValueType};
use std::fmt;
use std::sync::Arc;
use uuid::Uuid;

/// A type-description of a trait.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TraitType {
    pub id: Uuid,
    pub data: Vec<u8>,
}

/// An implementation of a trait.
pub struct TraitImpl {
    tt: TraitType,
    data: ValueData,
}

pub struct TraitImplRef<T> {
    v: Arc<TraitImpl>,
    _phantom: std::marker::PhantomData<T>,
}

/// A runtime trait storage class.
pub trait Trait {
    fn trait_type() -> TraitType;
}

/// A runtime trait interface, created from a storage class.
pub trait CreateTrait: Trait {
    type Storage: Sync;

    fn create(
        traits: super::runtime::Traits,
        value_type: Arc<ValueType>,
        storage: TraitImplRef<Self::Storage>,
    ) -> Self;
}

/// A trait reference.
///
/// This creates a simple struct which implements CreateTrait. It requires the storage type to
/// implement `Trait`.
///
/// The struct has the following (private) members:
/// `traits`: `Traits`,
/// `value_type`: `Arc<ValueType>`,
/// `storage`: `TraitImplRef<T>`.
///
/// The macro is preferred to make it convenient for implementors to add their own `impl` for the
/// struct. The alternative would be a `struct TraitRef<T>`, but this requires implementors to make
/// a trait and implement it for the `TraitRef<T>`, so the macro is a bit more straitforward.
#[macro_export]
macro_rules! TraitRef {
    ( $(#[$attr:meta])* $vis:vis struct $name:ident $(<$( $ts:ty ),+>)? ( $t:ty ); ) => {
        $(#[$attr])*
        $vis struct $name $(<$($ts),+>)? {
            #[allow(dead_code)]
            traits: $crate::Traits,
            #[allow(dead_code)]
            value_type: std::sync::Arc<$crate::ValueType>,
            storage: $crate::TraitImplRef<$t>
        }

        impl $(<$($ts),+>)? $crate::Trait for $name $(<$($ts),+>)? {
            fn trait_type() -> $crate::TraitType {
                <$t as $crate::Trait>::trait_type()
            }
        }

        impl $(<$($ts),+>)? $crate::CreateTrait for $name $(<$($ts),+>)? {
            type Storage = $t;

            fn create(
                traits: $crate::Traits,
                value_type: std::sync::Arc<$crate::ValueType>,
                storage: $crate::TraitImplRef<Self::Storage>,
            ) -> Self {
                $name { traits, value_type, storage }
            }
        }
    };
}

impl TraitType {
    /// Create a new trait type with the given id.
    pub fn new(id: Uuid) -> Self {
        Self::with_data(id, Default::default())
    }

    /// Create a new trait type with the given id and data.
    pub fn with_data(id: Uuid, data: Vec<u8>) -> Self {
        TraitType { id, data }
    }
}

impl TraitImpl {
    pub fn new<T>(tt: TraitType, imp: T) -> Self {
        Self {
            tt,
            data: ValueData::new(imp),
        }
    }

    pub fn for_trait<T: CreateTrait>(v: T::Storage) -> Self {
        Self {
            tt: T::trait_type(),
            data: ValueData::new(v),
        }
    }

    pub unsafe fn as_ref<T: Sync>(&self) -> &T {
        self.data.as_ref()
    }
}

impl<T: Sync> TraitImplRef<T> {
    // Unsafe as callers must verify that TraitImpl has type T.
    pub unsafe fn new(v: Arc<TraitImpl>) -> Self {
        TraitImplRef {
            v,
            _phantom: Default::default(),
        }
    }
}

impl<T: Sync> std::ops::Deref for TraitImplRef<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { (*self.v).as_ref() }
    }
}

impl fmt::Debug for TraitImpl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TraitImpl").field("tt", &self.tt).finish()
    }
}

impl PartialEq for TraitImpl {
    fn eq(&self, other: &Self) -> bool {
        &self.tt == &other.tt
    }
}

impl Eq for TraitImpl {}

impl std::hash::Hash for TraitImpl {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.tt.hash(h)
    }
}

impl std::borrow::Borrow<TraitType> for TraitImpl {
    fn borrow(&self) -> &TraitType {
        &self.tt
    }
}

impl std::borrow::Borrow<TraitType> for std::sync::Arc<TraitImpl> {
    fn borrow(&self) -> &TraitType {
        &self.tt
    }
}
