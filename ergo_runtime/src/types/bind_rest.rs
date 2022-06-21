//! The BindRest type.

use crate as ergo_runtime;
use crate::abi_stable::StableAbi;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{TypedValue, Value};

/// A type used as a key in maps to indicate to what to bind any remaining values.
///
/// This type is used when merging unbound values into maps.
#[derive(Clone, Hash, ErgoType, StableAbi)]
#[repr(C)]
pub struct BindRestKey;

/// A type indicating to what to bind any remaining values.
///
/// This type is used when merging unbound values into arrays.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct BindRest(pub Value);

impl From<BindRestKey> for TypedValue<BindRestKey> {
    fn from(v: BindRestKey) -> Self {
        Self::constant(v)
    }
}

unsafe impl crate::value::InnerValues for BindRest {
    fn visit<'a, F: FnMut(&'a Value)>(&'a self, mut f: F) {
        f(&self.0);
    }
}

impl From<BindRest> for TypedValue<BindRest> {
    fn from(v: BindRest) -> Self {
        Self::new(v)
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, BindRest);
    crate::ergo_type_name!(traits, BindRestKey);
}
