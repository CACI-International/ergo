//! Runtime bindings with dynamic scope.

use crate::abi_stable::{
    bst::BstMap,
    std_types::RArc,
    type_erase::{Eraseable, Erased, Ref},
    u128::U128,
    StableAbi,
};
use crate::{IdentifiedValue, Source, Value};

/// Types which can be used as a key in a dynamic scope.
pub trait DynamicScopeKey {
    /// The type of value stored in the dynamic scope.
    type Value: Eraseable;

    /// Get the scope entry identifier for this key.
    fn id(&self) -> u128;
}

impl DynamicScopeKey for IdentifiedValue {
    type Value = Value;

    fn id(&self) -> u128 {
        *self.id()
    }
}

/// Runtime dynamic-scoped bindings.
#[derive(Debug, Default, Clone, StableAbi)]
#[repr(C)]
pub struct DynamicScope {
    scope: BstMap<U128, RArc<DynamicValue>>,
}

#[derive(Debug, StableAbi)]
#[repr(C)]
struct DynamicValue {
    key_source: Source<()>,
    value: Erased,
}

/// A pointer to a dynamic entry value.
pub struct EntryPointer(RArc<DynamicValue>);

impl std::ops::Deref for EntryPointer {
    type Target = Erased;

    fn deref(&self) -> &Self::Target {
        &self.0.value
    }
}

/// A reference to a dynamic scoped value.
pub type DynamicScopeRef<T> = Ref<T, EntryPointer>;

impl DynamicScope {
    /// Get a value from the dynamic scope.
    pub fn get<T: DynamicScopeKey>(&self, key: &T) -> Option<DynamicScopeRef<T::Value>> {
        self.scope
            .get(&key.id())
            .map(|v| unsafe { DynamicScopeRef::new(EntryPointer(v.clone())) })
    }

    /// Get the source location for a key.
    pub fn get_key_source<T: DynamicScopeKey>(&self, key: &T) -> Option<Source<()>> {
        self.scope.get(&key.id()).map(|v| v.key_source.clone())
    }

    /// Set a value in the dynamic scope.
    pub fn set<T: DynamicScopeKey>(&mut self, key: &Source<T>, value: T::Value) {
        let (key_source, k) = key.as_ref().map(|k| k.id()).take();
        self.scope.insert(
            k.into(),
            RArc::new(DynamicValue {
                key_source,
                value: Erased::new(value),
            }),
        );
    }

    /// Remove a value in the dynamic scope with the given key.
    pub fn remove<T: DynamicScopeKey>(&mut self, key: &T) {
        self.scope.remove(&key.id());
    }
}
