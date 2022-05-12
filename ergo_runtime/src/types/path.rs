//! The Path type.

use crate as ergo_runtime;
use crate::abi_stable::{path::PathBuf, type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, DependenciesConstant, GetDependenciesConstant, TypedValue};
use bincode;

/// Script string type.
#[derive(Clone, Debug, Default, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Path(pub PathBuf);

crate::ConstantDependency!(Path);

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.as_ref().display().fmt(f)
    }
}

impl<'a> From<std::borrow::Cow<'a, std::path::Path>> for Path {
    fn from(p: std::borrow::Cow<'a, std::path::Path>) -> Self {
        Path(p.into_owned().into())
    }
}

impl<'a> From<&'a std::path::Path> for Path {
    fn from(p: &'a std::path::Path) -> Self {
        Path(p.to_owned().into())
    }
}

impl From<std::path::PathBuf> for Path {
    fn from(p: std::path::PathBuf) -> Self {
        Path(p.into())
    }
}

impl std::ops::Deref for Path {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
        self.0.into_pathbuf()
    }
}

impl From<Path> for super::String {
    fn from(p: Path) -> Self {
        Self::from(p.0.as_ref().as_ref().to_string_lossy())
    }
}

impl From<super::String> for Path {
    fn from(s: super::String) -> Self {
        Path(std::path::PathBuf::from(s.0.into_string()).into())
    }
}

ergo_traits_fn! {
    crate::ergo_display_basic!(traits, Path);

    impl traits::Stored for Path {
        async fn put(&self, data: &mut traits::PutData<'_>) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                { bincode::serialize_into(data, self.0.as_ref().as_ref()) }
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
