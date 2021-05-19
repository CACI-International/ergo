//! The BindRest type.

use crate as ergo_runtime;
use crate::abi_stable::StableAbi;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, Source, TypedValue, Value};

/// A type used as a key in maps to indicate to what to bind any remaining values.
///
/// This type is used when merging unbound values into maps.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct BindRestKey;

/// A type indicating to what to bind any remaining values.
///
/// This type is used when merging unbound values into arrays.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct BindRest(pub Source<Value>);

impl From<&'_ BindRestKey> for Dependencies {
    fn from(_: &'_ BindRestKey) -> Self {
        depends![BindRestKey::ergo_type()]
    }
}

impl From<BindRestKey> for TypedValue<BindRestKey> {
    fn from(v: BindRestKey) -> Self {
        Self::constant(v)
    }
}

impl From<&'_ BindRest> for Dependencies {
    fn from(b: &'_ BindRest) -> Self {
        depends![BindRest::ergo_type(), *b.0]
    }
}

impl From<BindRest> for TypedValue<BindRest> {
    fn from(v: BindRest) -> Self {
        Self::constant(v)
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, BindRest);
    crate::ergo_type_name!(traits, BindRestKey);
}
