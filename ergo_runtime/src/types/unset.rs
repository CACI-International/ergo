//! The Unset type.

use crate as ergo_runtime;
use crate::abi_stable::{type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, DependenciesConstant, GetDependenciesConstant, TypedValue};

/// The type indicating a value is unset.
#[derive(Clone, Copy, Debug, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Unset;

impl GetDependenciesConstant for Unset {
    fn get_depends(&self) -> DependenciesConstant {
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

    impl traits::Stored for Unset {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                { bincode::serialize_into(item, &()) }
            ).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            crate::error_info!(
                { bincode::deserialize_from(item).map(|()| Erased::new(Unset)) }
            ).into()
        }
    }

    traits::IntoTyped::<super::Bool>::add_impl::<Unset>(traits);

    crate::ergo_type_name!(traits, Unset);
}
