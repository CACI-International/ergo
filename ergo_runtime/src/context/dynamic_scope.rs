//! Runtime bindings with dynamic scope.

use crate::abi_stable::{
    bst::BstMap,
    std_types::RArc,
    type_erase::{Eraseable, Erased, Ref},
    u128::U128,
    StableAbi,
};
use crate::{IdentifiedValue, Source};
use std::sync::atomic::{AtomicBool, Ordering};

/// Types which can be used as a key in a dynamic scope.
pub trait DynamicScopeKey {
    /// The type of value stored in the dynamic scope.
    type Value: Eraseable;

    /// Get the scope entry identifier for this key.
    fn id(&self) -> u128;

    /// Get an identifier for the stored value of this key.
    ///
    /// This is only required if `affects_identity` returns true (the default).
    fn value_id(value: &Self::Value) -> u128;

    /// Return whether the scoped value affects consumer identity.
    fn affects_identity(&self) -> bool {
        true
    }
}

impl DynamicScopeKey for IdentifiedValue {
    type Value = IdentifiedValue;

    fn id(&self) -> u128 {
        *self.id()
    }

    fn value_id(value: &Self::Value) -> u128 {
        *value.id()
    }
}

/// Runtime dynamic-scoped bindings.
#[derive(Debug, Default, StableAbi)]
#[repr(C)]
pub struct DynamicScope {
    scope: BstMap<U128, Entry>,
}

#[derive(Debug, StableAbi)]
#[repr(C)]
struct DynamicValue {
    key_source: Source<()>,
    value: Erased,
    id: U128,
}

/// A pointer to a dynamic scope entry.
///
/// This type is used to implement Deref such that it can be used with type_erase::Ref.
#[derive(Debug, StableAbi)]
#[repr(C)]
struct Entry {
    value: RArc<DynamicValue>,
    // Store whether the entry has been accessed, for use in tracking when dynamic bindings are
    // accessed while evaluating Values.
    accessed: AtomicBool,
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

impl Entry {
    fn new(source: Source<()>, value: Erased, id: U128) -> Self {
        Entry {
            value: RArc::new(DynamicValue {
                key_source: source,
                value,
                id,
            }),
            accessed: AtomicBool::new(false),
        }
    }
}

impl DynamicScope {
    /// Get a value from the dynamic scope.
    pub fn get<T: DynamicScopeKey>(&self, key: &T) -> Option<DynamicScopeRef<T::Value>> {
        self.scope.get(&key.id()).map(|v| {
            if key.affects_identity() {
                v.accessed.store(true, Ordering::Relaxed);
            }
            unsafe { DynamicScopeRef::new(EntryPointer(v.value.clone())) }
        })
    }

    /// Get the source location for a key.
    pub fn get_key_source<T: DynamicScopeKey>(&self, key: &T) -> Option<Source<()>> {
        self.scope
            .get(&key.id())
            .map(|v| v.value.key_source.clone())
    }

    /// Set a value in the dynamic scope.
    pub fn set<T: DynamicScopeKey>(&mut self, key: &Source<T>, value: T::Value) {
        let (src, k) = key.as_ref().map(|k| k.id()).take();
        let entry_id = T::value_id(&value);
        self.scope.insert(
            k.into(),
            Entry::new(src, Erased::new(value), entry_id.into()),
        );
    }

    /// Remove a value in the dynamic scope with the given key.
    pub fn remove<T: DynamicScopeKey>(&mut self, key: &T) {
        self.scope.remove(&key.id());
    }

    /// Return the keys in the dynamic scope that were accessed, and their value ids.
    pub fn accessed(&self) -> Vec<(u128, u128)> {
        self.scope
            .iter()
            .filter_map(|(k, v)| {
                if v.accessed.load(Ordering::Relaxed) {
                    Some((k.value(), v.value.id.value()))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Return the keys in the dynamic scope and their value ids.
    pub fn ids(&self) -> Vec<(u128, u128)> {
        self.scope
            .iter()
            .map(|(k, v)| (k.value(), v.value.id.value()))
            .collect()
    }
}

impl super::Fork for DynamicScope {
    fn fork(&self) -> Self {
        DynamicScope {
            scope: self
                .scope
                .iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        Entry {
                            value: v.value.clone(),
                            accessed: AtomicBool::new(false),
                        },
                    )
                })
                .collect(),
        }
    }

    fn join(&self, forked: Self) {
        for (k, v) in self.scope.iter() {
            if let Some(entry) = forked.scope.get(k) {
                if std::ptr::eq(entry.value.as_ref(), v.value.as_ref()) {
                    v.accessed
                        .fetch_or(entry.accessed.load(Ordering::Relaxed), Ordering::Relaxed);
                }
            }
        }
    }
}
