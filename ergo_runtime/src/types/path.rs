//! The Path type.

use crate as ergo_runtime;
use crate::abi_stable::{path::PathBuf, type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, DependenciesConstant, GetDependenciesConstant, TypedValue};
use bincode;

/// Script string type.
#[derive(Debug, Default, ErgoType, StableAbi)]
#[repr(C)]
pub struct Path {
    pub path: PathBuf,
    owned: std::sync::atomic::AtomicBool,
}

impl Path {
    /// Create a new Path.
    pub fn new<P: Into<PathBuf>>(p: P) -> Self {
        Path {
            path: p.into(),
            owned: false.into(),
        }
    }

    /// Create an owned Path.
    pub fn owned<P: Into<PathBuf>>(p: P) -> Self {
        Path {
            path: p.into(),
            owned: true.into(),
        }
    }

    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }

    pub fn into_owned(self) -> Self {
        self.take_ownership();
        Path {
            path: self.path.clone(),
            owned: true.into(),
        }
    }

    /// Take ownership of the Path, returning whether the path was owned.
    pub fn take_ownership(&self) -> bool {
        self.owned.swap(false, std::sync::atomic::Ordering::Relaxed)
    }
}

impl Drop for Path {
    fn drop(&mut self) {
        if self.owned.load(std::sync::atomic::Ordering::Relaxed) {
            let p = self.path.as_ref();
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

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.path.eq(&other.path)
    }
}

impl Eq for Path {}

impl Clone for Path {
    fn clone(&self) -> Self {
        Self::new(self.path.clone())
    }
}

impl std::hash::Hash for Path {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.path.hash(h);
    }
}

crate::ConstantDependency!(Path);

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.path.as_ref().display().fmt(f)
    }
}

impl<'a> From<std::borrow::Cow<'a, std::path::Path>> for Path {
    fn from(p: std::borrow::Cow<'a, std::path::Path>) -> Self {
        Path::new(p.into_owned())
    }
}

impl<'a> From<&'a std::path::Path> for Path {
    fn from(p: &'a std::path::Path) -> Self {
        Path::new(p.to_owned())
    }
}

impl From<std::path::PathBuf> for Path {
    fn from(p: std::path::PathBuf) -> Self {
        Path::new(p)
    }
}

impl std::ops::Deref for Path {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl std::ops::DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.path
    }
}

impl GetDependenciesConstant for Path {
    fn get_depends(&self) -> DependenciesConstant {
        depends![Path::ergo_type(), self]
    }
}

impl From<Path> for TypedValue<Path> {
    fn from(v: Path) -> Self {
        Self::constant(v)
    }
}

impl Path {
    /// Convert this value into a std::path::PathBuf.
    pub fn into_pathbuf(self) -> std::path::PathBuf {
        self.path.clone().into_pathbuf()
    }
}

impl From<Path> for super::String {
    fn from(p: Path) -> Self {
        Self::from(p.path.as_ref().as_ref().to_string_lossy())
    }
}

impl From<super::String> for Path {
    fn from(s: super::String) -> Self {
        Path::new(std::path::PathBuf::from(s.0.into_string()))
    }
}

ergo_traits_fn! {
    crate::ergo_display_basic!(traits, Path);

    impl traits::Stored for Path {
        async fn put(&self, data: &mut traits::PutData<'_>) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                { bincode::serialize_into(data, self.path.as_ref().as_ref()) }
            ).into()
        }

        async fn get(data: &mut traits::GetData<'_>) -> crate::RResult<Erased> {
            crate::error_info!(
                { bincode::deserialize_from(data).map(|p: std::path::PathBuf| Erased::new(Path::from(p))) }
            ).into()
        }
    }

    traits::IntoTyped::<super::String>::add_impl::<Path>(traits);
    traits::IntoTyped::<Path>::add_impl::<super::String>(traits);
    crate::ergo_type_name!(traits, Path);
}
