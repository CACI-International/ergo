//! The Path type.

use crate as ergo_runtime;
use crate::abi_stable::{path::PathBuf, type_erase::Erased, StableAbi};
use crate::metadata::Doc;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue};
use bincode;

/// Script string type.
#[derive(Clone, Debug, Default, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Path(pub PathBuf);

crate::HashAsDependency!(Path);

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.as_ref().display().fmt(f)
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

impl From<Path> for TypedValue<Path> {
    fn from(v: Path) -> Self {
        let doc = v.to_string();
        let mut v = Self::constant(v);
        Doc::set_string(&mut v, doc);
        v
    }
}

impl Path {
    /// Convert this value into a std::path::PathBuf.
    pub fn into_pathbuf(self) -> std::path::PathBuf {
        self.0.into_pathbuf()
    }
}

impl From<&'_ Path> for Dependencies {
    fn from(p: &'_ Path) -> Self {
        depends![Path::ergo_type(), p]
    }
}

impl From<Path> for super::String {
    fn from(p: Path) -> Self {
        Self::from(p.0.as_ref().as_ref().to_string_lossy())
    }
}

ergo_traits_fn! {
    crate::ergo_display_basic!(traits, Path);

    impl traits::Stored for Path {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            bincode::serialize_into(item, self.0.as_ref().as_ref()).map_err(|e| e.into()).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            bincode::deserialize_from(item).map(|p: std::path::PathBuf| Erased::new(Path::from(p))).map_err(|e| e.into()).into()
        }
    }

    traits::IntoTyped::<super::String>::add_impl::<Path>(traits);
    crate::ergo_type_name!(traits, Path);
    traits::ValueByContent::add_impl::<Path>(traits);
}
