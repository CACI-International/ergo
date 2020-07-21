//! Runtime trait implementation.

use crate::type_erase::Erased;
use crate::types::*;
use abi_stable::{
    std_types::{RArc, RHashMap, ROption},
    StableAbi,
};

#[path = "traits/trait.rs"]
pub mod trait_;

pub use trait_::*;

/// ABI-stable key to store (type, trait) tuple.
#[derive(Clone, Debug, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
pub struct TraitKey(pub Type, pub Trait);

/// A reference to a trait implementation.
pub type Ref<T> = crate::type_erase::Ref<T, RArc<Erased>>;

/// Storage for trait implementations.
///
/// Implementations are stored as reference-counted values.
/// Storage may indicate there is no trait implementation explicitly.
#[derive(Debug, Default, StableAbi)]
#[repr(C)]
pub struct Traits {
    impls: RHashMap<TraitKey, ROption<RArc<Erased>>>,
}

impl Traits {
    /// Create a new, empty set of trait implementations.
    pub fn new() -> Self {
        Self::default()
    }

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

    /// Get a trait implementation for the given rust type.
    pub fn get_for_type<Tp, Impl>(&self) -> Option<Option<Ref<Impl>>>
    where
        Tp: GreaseType,
        Impl: GreaseTrait,
    {
        self.get::<Impl>(&Tp::grease_type())
    }
}
