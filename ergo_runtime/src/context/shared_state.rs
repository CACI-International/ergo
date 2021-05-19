//! Runtime shared state.

use crate::abi_stable::{
    external_types::RMutex,
    std_types::{RArc, RHashMap},
    type_erase::{Eraseable, Erased, Ref},
    StableAbi,
};
use crate::type_system::{ErgoType, Type};
use crate::Result;

#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct SharedState {
    keyed_lifetime: RArc<RMutex<RHashMap<Type, RArc<Erased>>>>,
}

impl std::fmt::Debug for SharedState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("SharedState")
            .field("keyed_lifetime", &self.keyed_lifetime.lock())
            .finish()
    }
}

impl SharedState {
    pub(crate) fn new() -> Self {
        SharedState {
            keyed_lifetime: RArc::new(RMutex::new(Default::default())),
        }
    }

    /// Load or create and load a value in the runtime.
    ///
    /// The passed closure is called when the value has not yet been created.
    pub fn get<T: Eraseable + ErgoType, F>(&self, missing: F) -> Result<Ref<T, RArc<Erased>>>
    where
        F: FnOnce() -> Result<T>,
    {
        let mut guard = self.keyed_lifetime.lock();
        if !guard.contains_key(&T::ergo_type()) {
            guard.insert(T::ergo_type(), RArc::new(Erased::new(missing()?)));
        }
        Ok(unsafe {
            Ref::new(
                guard
                    .get(&T::ergo_type())
                    .expect("type must have been inserted")
                    .clone(),
            )
        })
    }
}
