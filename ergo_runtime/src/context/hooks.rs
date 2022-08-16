//! Runtime state hooks.

use crate as ergo_runtime;
use crate::abi_stable::{closure::ClosureOnce, external_types::RMutex, std_types::RVec, StableAbi};
use crate::type_system::ErgoType;

#[derive(StableAbi, ErgoType)]
#[repr(C)]
pub struct Hooks {
    shutdown: RMutex<RVec<ClosureOnce<(), ()>>>,
}

impl std::fmt::Debug for Hooks {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Hooks")
            .field("shutdown", &self.shutdown.lock().len())
            .finish()
    }
}

impl Default for Hooks {
    fn default() -> Self {
        Hooks {
            shutdown: RMutex::new(Default::default()),
        }
    }
}

impl Hooks {
    /// Add a shutdown hook.
    pub fn add_shutdown<F: FnOnce() + Send + Sync + 'static>(&self, f: F) {
        self.shutdown.lock().push(f.into());
    }

    /// Call the shutdown hooks.
    pub fn shutdown(&self) {
        for f in std::mem::take(&mut *self.shutdown.lock()) {
            f.call();
        }
    }
}
