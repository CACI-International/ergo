//! The String type.

use crate as ergo_runtime;
use crate::abi_stable::{std_types::RString, type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, DependenciesConstant, GetDependenciesConstant, TypedValue};
use bincode;

/// Script string type.
#[derive(Clone, Debug, Default, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct String(pub RString);

crate::ConstantDependency!(String);

impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a> From<&'a str> for String {
    fn from(s: &'a str) -> Self {
        String(s.into())
    }
}

impl<'a> From<std::borrow::Cow<'a, str>> for String {
    fn from(s: std::borrow::Cow<'a, str>) -> Self {
        String(s.into())
    }
}

impl From<std::string::String> for String {
    fn from(s: std::string::String) -> Self {
        String(s.into())
    }
}

impl std::ops::Deref for String {
    type Target = RString;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for String {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<String> for TypedValue<String> {
    fn from(v: String) -> Self {
        Self::constant(v)
    }
}

impl GetDependenciesConstant for String {
    fn get_depends(&self) -> DependenciesConstant {
        depends![String::ergo_type(), self]
    }
}

impl String {
    /// Create a new (empty) string.
    pub fn new() -> Self {
        Default::default()
    }

    /// Convert this value into a std::string::String.
    pub fn into_string(self) -> std::string::String {
        self.0.into_string()
    }
}

ergo_traits_fn! {
    crate::ergo_display_basic!(traits, String);

    impl traits::Stored for String {
        async fn put(&self, _stored_ctx: &traits::StoredContext, data: &mut traits::PutData<'_>) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                { bincode::serialize_into(data, &self.0) }
            ).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, data: &mut traits::GetData<'_>) -> crate::RResult<Erased> {
            crate::error_info!(
                { bincode::deserialize_from(data).map(|s| Erased::new(String(s))) }
            ).into()
        }
    }

    crate::ergo_type_name!(traits, String);
}
