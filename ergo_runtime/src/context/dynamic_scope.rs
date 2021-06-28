//! Runtime bindings with dynamic scope.

use crate::abi_stable::{
    bst::BstMap,
    std_types::RArc,
    type_erase::{Eraseable, Erased, Ref},
    u128::U128,
    StableAbi,
};
use crate::{Source, Value};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Types which can be used as a key in a dynamic scope.
pub trait DynamicScopeKey {
    /// The type of value stored in the dynamic scope.
    type Value: Eraseable;

    /// Get the scope entry identifier for this key.
    fn id(&self) -> u128;
}

impl DynamicScopeKey for Value {
    type Value = Source<Value>;

    fn id(&self) -> u128 {
        self.id()
    }
}

/// Runtime dynamic-scoped bindings.
#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct DynamicScope {
    scope: BstMap<U128, EntryPointer>,
}

#[derive(Debug, StableAbi)]
#[repr(C)]
struct Entry {
    source: Source<()>,
    value: Erased,
    // Store the number of times the entry has been accessed, for use in tracking when dynamic
    // bindings are accessed while evaluating Values.
    access_count: AtomicUsize,
}

/// A pointer to a dynamic scope entry.
///
/// This type is used to implement Deref such that it can be used with type_erase::Ref.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct EntryPointer(RArc<Entry>);

impl std::ops::Deref for EntryPointer {
    type Target = Erased;

    fn deref(&self) -> &Self::Target {
        &self.0.value
    }
}

/// A reference to a dynamic scoped value.
pub type DynamicScopeRef<T> = Ref<T, EntryPointer>;

impl EntryPointer {
    fn new(source: Source<()>, value: Erased) -> EntryPointer {
        EntryPointer(RArc::new(Entry {
            source,
            value,
            access_count: Default::default(),
        }))
    }
}

/// A snapshot of the state of dynamically scoped bindings.
///
/// Snapshots from the _same_ dynamic scope may be used to determine whether the scope has been
/// accessed between the snapshots.
pub struct Snapshot<'a>(usize, std::marker::PhantomData<&'a ()>);

impl<'a> Snapshot<'a> {
    /// Return whether any bindings within the dynamic scope were accessed between two snapshots.
    pub fn scope_accessed_between(&self, other: &Self) -> bool {
        self.0 != other.0
    }
}

impl DynamicScope {
    /// Get a value from the dynamic scope.
    pub fn get<T: DynamicScopeKey>(&self, key: &T) -> Option<DynamicScopeRef<T::Value>> {
        self.scope.get(&key.id()).map(|v| {
            v.0.access_count.fetch_add(1, Ordering::Relaxed);
            unsafe { DynamicScopeRef::new(v.clone()) }
        })
    }

    /// Get the source location for a key.
    pub fn get_key_source<T: DynamicScopeKey>(&self, key: &T) -> Option<Source<()>> {
        self.scope.get(&key.id()).map(|v| v.0.source.clone())
    }

    /// Set a value in the dynamic scope.
    pub fn set<T: DynamicScopeKey>(&mut self, key: &Source<T>, value: T::Value) {
        let (src, k) = key.as_ref().map(|k| k.id()).take();
        self.scope
            .insert(k.into(), EntryPointer::new(src, Erased::new(value)));
    }

    /// Remove a value in the dynamic scope with the given key.
    pub fn remove<T: DynamicScopeKey>(&mut self, key: &T) {
        self.scope.remove(&key.id());
    }

    /// Create a snapshot of the dynamic scope.
    pub fn snapshot(&self) -> Snapshot {
        let mut total: usize = 0;
        for (_, v) in self.scope.iter() {
            total += v.0.access_count.load(Ordering::Relaxed);
        }
        Snapshot(total, std::marker::PhantomData)
    }

    /// Return whether the scope has been accessed since the given snapshot.
    pub fn scope_accessed_since(&self, snapshot: &Snapshot<'_>) -> bool {
        let current = self.snapshot();
        snapshot.scope_accessed_between(&current)
    }
}
