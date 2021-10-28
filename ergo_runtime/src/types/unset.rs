//! The Unset type.

use crate as ergo_runtime;
use crate::abi_stable::StableAbi;
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue};

/// The type indicating a value is unset.
#[derive(Clone, Copy, Debug, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Unset;

impl From<&'_ Unset> for Dependencies {
    fn from(_: &'_ Unset) -> Self {
        depends![Unset::ergo_type()]
    }
}

impl From<Unset> for TypedValue<Unset> {
    fn from(v: Unset) -> Self {
        Self::constant(v)
    }
}

impl From<Unset> for super::Bool {
    fn from(_: Unset) -> Self {
        super::Bool(false)
    }
}

ergo_traits_fn! {
    impl traits::Display for Unset {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::RResult<()> {
            write!(f, "<unset>").map_err(|e| crate::error!{
                labels: [ primary(Source::get(SELF_VALUE).with("while displaying this value")) ],
                error: e
            }).into()
        }
    }

    traits::IntoTyped::<super::Bool>::add_impl::<Unset>(traits);

    crate::ergo_type_name!(traits, Unset);
    traits::ValueByContent::add_impl::<Unset>(traits);
}
