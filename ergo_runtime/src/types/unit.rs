//! The Unit type.

use crate as ergo_runtime;
use crate::abi_stable::{type_erase::Erased, StableAbi};
use crate::metadata::Doc;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue};
use bincode;

/// Script unit type.
#[derive(Clone, Copy, Debug, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Unit;

impl From<&'_ Unit> for Dependencies {
    fn from(_: &'_ Unit) -> Self {
        depends![Unit::ergo_type()]
    }
}

impl From<Unit> for TypedValue<Unit> {
    fn from(v: Unit) -> Self {
        let mut v = Self::constant(v);
        Doc::set_string(&mut v, "()");
        v
    }
}

ergo_traits_fn! {
    impl traits::Display for Unit {
        async fn fmt(&self, _f: &mut traits::Formatter) -> crate::RResult<()> {
            crate::RResult::ROk(())
        }
    }

    impl traits::Stored for Unit {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            bincode::serialize_into(item, &()).map_err(|e| e.into()).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            bincode::deserialize_from(item).map(|()| Erased::new(Unit)).map_err(|e| e.into()).into()
        }
    }

    crate::ergo_type_name!(traits, Unit);
    traits::ValueByContent::add_impl::<Unit>(traits);
}
