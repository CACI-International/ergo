//! Environmental values.

use crate::abi_stable::{path::PathBuf, StableAbi};

#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct Environment {
    project_directory: PathBuf,
}

impl Environment {
    pub fn new(project_dir: std::path::PathBuf) -> Self {
        Environment {
            project_directory: project_dir.into(),
        }
    }

    /// Get the configured project directory.
    pub fn project_directory(&self) -> std::borrow::Cow<std::path::Path> {
        self.project_directory.as_ref()
    }
}
