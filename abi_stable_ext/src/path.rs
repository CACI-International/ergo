//! ABI-stable PathBuf.
//!
//! The implementation only supports converting to/from `std::path::PathBuf`.

use crate::ffi::OsString;
use abi_stable::StableAbi;
use std::borrow::Cow;

#[derive(Clone, Debug, Default, PartialOrd, Ord, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
pub struct PathBuf {
    path: OsString,
}

#[derive(Debug)]
pub struct Display<'a>(&'a PathBuf);

impl<'a> std::fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.as_ref().display().fmt(f)
    }
}

impl PathBuf {
    pub fn into_pathbuf(self) -> std::path::PathBuf {
        self.into()
    }

    pub fn as_ref<'a>(&'a self) -> Cow<'a, std::path::Path> {
        match self.path.as_ref() {
            Cow::Borrowed(b) => Cow::Borrowed(std::path::Path::new(b)),
            Cow::Owned(o) => Cow::Owned(o.into()),
        }
    }

    pub fn display(&self) -> Display {
        Display(self)
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

impl From<PathBuf> for OsString {
    fn from(p: PathBuf) -> Self {
        p.path
    }
}

impl From<OsString> for PathBuf {
    fn from(path: OsString) -> Self {
        PathBuf { path }
    }
}

impl PartialEq<std::path::Path> for PathBuf {
    fn eq(&self, other: &std::path::Path) -> bool {
        self.as_ref() == other
    }
}

impl PartialEq<PathBuf> for std::path::Path {
    fn eq(&self, other: &PathBuf) -> bool {
        self == other.as_ref()
    }
}

impl PartialEq<std::path::PathBuf> for PathBuf {
    fn eq(&self, other: &std::path::PathBuf) -> bool {
        self.as_ref() == other.as_path()
    }
}

impl PartialEq<PathBuf> for std::path::PathBuf {
    fn eq(&self, other: &PathBuf) -> bool {
        self.as_path() == other.as_ref()
    }
}
