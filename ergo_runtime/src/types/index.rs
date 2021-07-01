//! The Index type.

use crate as ergo_runtime;
use crate::abi_stable::StableAbi;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue, Value};

/// The value in an index operation.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct Index(pub Value);

impl From<&'_ Index> for Dependencies {
    fn from(i: &'_ Index) -> Self {
        depends![Index::ergo_type(), i.0]
    }
}

impl From<Index> for TypedValue<Index> {
    fn from(v: Index) -> Self {
        Self::constant(v)
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, Index);

    impl traits::Bind for Index {
        async fn bind(&self, arg: Value) -> Value {
            let ind = crate::try_result!(CONTEXT.eval_as::<Index>(arg).await).to_owned().0;
            crate::try_result!(traits::bind_no_error(CONTEXT, self.0.clone(), ind).await);
            super::Unit.into()
        }
    }
}
