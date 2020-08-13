//! Runtime trait implementation.

use crate::type_erase::{Eraseable, Erased, ErasedTrivial};
use crate::types::*;
use crate::uuid::*;
use abi_stable::{
    std_types::{RArc, RHashMap, ROption},
    StableAbi,
};
use lazy_static::lazy_static;

pub use grease_macro::GreaseTrait;

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

/// ABI-stable key to store (type, trait) tuple.
#[derive(Clone, Debug, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
struct TraitKey(pub Type, pub Trait);

/// A reference to a trait implementation.
pub type Ref<T> = crate::type_erase::Ref<T, RArc<Erased>>;

/// Storage for trait implementations.
///
/// Implementations are stored as reference-counted values.
/// Storage may indicate there is no trait implementation explicitly.
#[derive(Debug, Default, StableAbi)]
#[repr(C)]
pub(crate) struct Traits {
    impls: RHashMap<TraitKey, ROption<RArc<Erased>>>,
}

impl Traits {
    /// Insert a new implementation.
    ///
    /// Unsafe because the implementation must correspond to a the trait correctly
    /// on retrieval.
    pub unsafe fn insert_unchecked(&mut self, tp: Type, trt: Trait, implementation: Erased) {
        self.impls
            .insert(TraitKey(tp, trt), ROption::RSome(RArc::new(implementation)));
    }

    /// Insert the given implementation for the type.
    pub fn insert<Impl: GreaseTrait>(&mut self, tp: Type, implementation: Impl) {
        unsafe { self.insert_unchecked(tp, Impl::grease_trait(), Erased::new(implementation)) }
    }

    /// Insert the given impelementation for the rust type.
    pub fn insert_for_type<Tp, Impl>(&mut self, implementation: Impl)
    where
        Tp: GreaseType,
        Impl: GreaseTrait,
    {
        self.insert(Tp::grease_type(), implementation)
    }

    /// Insert an empty implementation, indicating explicitly that no implementation exists.
    pub fn insert_empty(&mut self, tp: Type, trt: Trait) {
        self.impls.insert(TraitKey(tp, trt), ROption::RNone);
    }

    /// Get a trait implementation.
    ///
    /// Unsafe because the implementation type must correspond to the grease trait descriptor.
    pub unsafe fn get_unchecked<Impl>(&self, tp: &Type, trt: &Trait) -> Option<Option<Ref<Impl>>> {
        self.impls
            .get(&TraitKey(tp.clone(), trt.clone()))
            .cloned()
            .map(|i| i.into_option().map(|erased| Ref::new(erased)))
    }

    /// Get a trait implementation for the given type.
    pub fn get<Impl: GreaseTrait>(&self, tp: &Type) -> Option<Option<Ref<Impl>>> {
        unsafe { self.get_unchecked::<Impl>(tp, &Impl::grease_trait()) }
    }
}
