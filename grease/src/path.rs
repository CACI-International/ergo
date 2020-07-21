//! ABI-stable PathBuf.
//!
//! The implementation only supports converting to/from `std::path::PathBuf`.

use crate::ffi::OsString;
use abi_stable::StableAbi;

#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct PathBuf {
    path: OsString,
}

impl PathBuf {
    pub fn into_pathbuf(self) -> std::path::PathBuf {
        self.into()
    }
}

impl From<std::path::PathBuf> for PathBuf {
    fn from(p: std::path::PathBuf) -> Self {
        let osstr: std::ffi::OsString = p.into();
        PathBuf { path: osstr.into() }
    }
}

impl From<PathBuf> for std::path::PathBuf {
    fn from(p: PathBuf) -> Self {
        std::ffi::OsString::from(p.path).into()
    }
}
