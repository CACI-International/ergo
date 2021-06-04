//! Runtime bindings with dynamic scope.

use crate::abi_stable::{
    bst::BstMap,
    rtuple,
    std_types::{RArc, Tuple2},
    type_erase::{Eraseable, Erased, Ref},
    u128::U128,
    StableAbi,
};
use crate::{Source, Value};

pub type DynamicScopeRef<T> = Ref<T, RArc<Erased>>;

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
    scope: BstMap<U128, Tuple2<Source<()>, RArc<Erased>>>,
}

impl DynamicScope {
    /// Get a value from the dynamic scope.
    pub fn get<T: DynamicScopeKey>(&self, key: &T) -> Option<DynamicScopeRef<T::Value>> {
        self.scope
            .get(&key.id())
            .map(|v| unsafe { Ref::new(v.1.clone()) })
    }

    /// Get the source location for a key.
    pub fn get_key_source<T: DynamicScopeKey>(&self, key: &T) -> Option<Source<()>> {
        self.scope.get(&key.id()).map(|v| v.0.clone())
    }

    /// Set a value in the dynamic scope.
    pub fn set<T: DynamicScopeKey>(&mut self, key: &Source<T>, value: T::Value) {
        let (src, k) = key.as_ref().map(|k| k.id()).take();
        self.scope
            .insert(k.into(), rtuple!(src, RArc::new(Erased::new(value))));
    }
}
