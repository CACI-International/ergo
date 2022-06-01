//! The Index type.

use crate as ergo_runtime;
use crate::abi_stable::StableAbi;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, GetDependencies, TypedValue, Value};

/// The value in an index operation.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct Index(pub Value);

impl GetDependencies for Index {
    fn get_depends(&self) -> Dependencies {
        depends![Index::ergo_type(), self.0]
    }
}

impl From<Index> for TypedValue<Index> {
    fn from(v: Index) -> Self {
        Self::new(v)
    }
}

impl traits::NestedValues for Index {
    fn nested_values(&self) -> Vec<&Value> {
        vec![&self.0]
    }

    fn nested_values_mut(&mut self) -> Vec<&mut Value> {
        vec![&mut self.0]
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, Index);

    traits::Nested::add_impl::<Index>(traits);
    traits::Functor::add_nested_impl::<Index>(traits);

    impl traits::Bind for Index {
        async fn bind(&self, arg: Value) -> Value {
            let ind = crate::try_result!(crate::Context::eval_as::<Index>(arg).await).to_owned().0;
            crate::try_result!(traits::bind_no_error(self.0.clone(), ind).await);
            super::Unit.into()
        }
    }
}
