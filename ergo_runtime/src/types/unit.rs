//! The Unit type.

use crate as ergo_runtime;
use crate::abi_stable::{type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, DependenciesConstant, GetDependenciesConstant, TypedValue};
use bincode;

/// Script unit type.
#[derive(Clone, Copy, Debug, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Unit;

impl GetDependenciesConstant for Unit {
    fn get_depends(&self) -> DependenciesConstant {
        depends![Unit::ergo_type()]
    }
}

impl From<Unit> for TypedValue<Unit> {
    fn from(v: Unit) -> Self {
        Self::constant(v)
    }
}

ergo_traits_fn! {
    impl traits::Display for Unit {
        async fn fmt(&self, _f: &mut traits::Formatter) -> crate::RResult<()> {
            crate::RResult::ROk(())
        }
    }

    impl traits::Stored for Unit {
        async fn put(&self, data: &mut traits::PutData<'_>) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                { bincode::serialize_into(data, &()) }
            ).into()
        }

        async fn get(data: &mut traits::GetData<'_>) -> crate::RResult<Erased> {
            crate::error_info!(
                { bincode::deserialize_from(data).map(|()| Erased::new(Unit)) }
            ).into()
        }
    }

    crate::ergo_type_name!(traits, Unit);
}
