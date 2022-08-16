//! The set of owned paths managed by the runtime.
//!
//! These paths will be removed when the runtime is shutdown.

use crate as ergo_runtime;
use crate::abi_stable::{
    bst::BstSet, external_types::RMutex, path::PathBuf, std_types::RArc, StableAbi,
};
use crate::type_system::ErgoType;

#[derive(Clone, StableAbi, ErgoType)]
#[repr(C)]
pub struct OwnedPaths {
    paths: RArc<RMutex<BstSet<PathBuf>>>,
}

impl Default for OwnedPaths {
    fn default() -> Self {
        OwnedPaths {
            paths: RArc::new(RMutex::new(Default::default())),
        }
    }
}

impl OwnedPaths {
    /// Add a path to the set of owned paths.
    pub fn add(&self, path: &PathBuf) {
        self.paths.lock().insert(path.clone());
    }

    /// Remove a path from the set of owned paths.
    ///
    /// Returns whether the path was previously owned.
    pub fn remove(&self, path: &PathBuf) -> bool {
        self.paths.lock().remove(path)
    }

    /// Delete all owned paths.
    pub fn delete_all(&self) {
        for p in std::mem::take(&mut *self.paths.lock()) {
            let p = p.as_ref();
            if p.is_symlink() || p.is_file() {
                if let Err(e) = std::fs::remove_file(p) {
                    log::warn!("error while removing owned file: {}", e);
                }
            } else if p.is_dir() {
                if let Err(e) = std::fs::remove_dir_all(p) {
                    log::warn!("error while removing owned directory: {}", e);
                }
            }
        }
    }
}
