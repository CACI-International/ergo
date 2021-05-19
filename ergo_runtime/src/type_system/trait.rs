//! Runtime trait implementation.

use super::ergo_type::*;
use crate::abi_stable::{
    std_types::{RArc, RHashMap, ROption},
    type_erase::{self, Eraseable, Erased, ErasedTrivial},
    uuid::Uuid,
    StableAbi,
};

/// A descriptor for ergo traits.
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
    /// Uses `nsid!` to generate a id from the given name.
    pub fn named(name: &[u8]) -> Self {
        Self::new(crate::nsid!(trait, name))
    }

    /// Create a new Trait with the given id and additional data.
    pub fn with_data(id: Uuid, data: ErasedTrivial) -> Self {
        Trait { id, data }
    }
}

/// A trait for rust types which represent implementations of ergo traits.
pub trait ErgoTrait {
    type Impl: Eraseable + StableAbi + Sized;
    /// Get the trait descriptor.
    fn ergo_trait() -> Trait;

    /// Create the trait from the implementation and context.
    fn create(trt: Ref<Self::Impl>, ctx: &crate::Context) -> Self;
}

/// ABI-stable key to store (type, trait) tuple.
#[derive(Clone, Debug, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
struct TraitKey(pub Type, pub Trait);

/// A reference to a trait implementation.
pub type Ref<T> = type_erase::Ref<T, RArc<Erased>>;

/// Storage for trait implementations.
///
/// Implementations are stored as reference-counted values.
/// Storage of `None` explicitly indicates there is no trait implementation.
#[derive(Debug, Default, StableAbi)]
#[repr(C)]
pub(crate) struct TraitRegistry {
    impls: RHashMap<TraitKey, ROption<RArc<Erased>>>,
}

impl TraitRegistry {
    /// Insert a new implementation.
    ///
    /// Unsafe because the implementation must correspond to a the trait correctly
    /// on retrieval.
    pub unsafe fn insert_unchecked(&mut self, tp: Type, trt: Trait, implementation: Erased) {
        self.impls
            .insert(TraitKey(tp, trt), ROption::RSome(RArc::new(implementation)));
    }

    /// Insert the given implementation for the type.
    pub fn insert<Trt: ErgoTrait>(&mut self, tp: Type, implementation: Trt::Impl) {
        unsafe { self.insert_unchecked(tp, Trt::ergo_trait(), Erased::new(implementation)) }
    }

    /// Insert the given impelementation for the rust type.
    pub fn insert_for_type<Tp, Trt>(&mut self, implementation: Trt::Impl)
    where
        Tp: ErgoType,
        Trt: ErgoTrait,
    {
        self.insert::<Trt>(Tp::ergo_type(), implementation)
    }

    /// Insert an empty implementation, indicating explicitly that no implementation exists.
    pub fn insert_empty(&mut self, tp: Type, trt: Trait) {
        self.impls.insert(TraitKey(tp, trt), ROption::RNone);
    }

    /// Get the raw trait implementation.
    pub fn get_impl(&self, tp: &Type, trt: &Trait) -> Option<Option<RArc<Erased>>> {
        self.impls
            .get(&TraitKey(tp.clone(), trt.clone()))
            .cloned()
            .map(|i| i.into_option())
    }

    /// Get a trait implementation.
    ///
    /// Unsafe because the implementation type must correspond to the ergo trait descriptor.
    #[allow(dead_code)]
    pub unsafe fn get_unchecked<Impl>(&self, tp: &Type, trt: &Trait) -> Option<Option<Ref<Impl>>> {
        self.get_impl(tp, trt)
            .map(|i| i.map(|erased| Ref::new(erased)))
    }

    /// Get a trait implementation for the given type.
    #[allow(dead_code)]
    pub fn get<Trt: ErgoTrait>(&self, tp: &Type) -> Option<Option<Ref<Trt::Impl>>> {
        unsafe { self.get_unchecked::<Trt::Impl>(tp, &Trt::ergo_trait()) }
    }
}
