//! Runtime bindings with dynamic scope.

use crate::abi_stable::{bst::BstMap, StableAbi};
use crate::{Source, Value};

/// Runtime dynamic-scoped bindings.
#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct DynamicScope {
    scope: BstMap<Source<Value>, Source<Value>>,
}

impl DynamicScope {
    /// Get a value from the dynamic scope.
    pub fn get<Q>(&self, key: &Q) -> Option<&Source<Value>>
    where
        Source<Value>: std::borrow::Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.scope.get(key)
    }

    /// Get a key-value pair from the dynamic scope.
    pub fn get_key_value<Q>(&self, key: &Q) -> Option<(&Source<Value>, &Source<Value>)>
    where
        Source<Value>: std::borrow::Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.scope.get_key_value(key)
    }

    /// Set a value in the dynamic scope.
    pub fn set(&mut self, key: Source<Value>, value: Source<Value>) {
        self.scope.insert(key, value);
    }
}
