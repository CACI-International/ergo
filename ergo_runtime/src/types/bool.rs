//! The Bool type.

use crate as ergo_runtime;
use crate::abi_stable::{
    closure::FnPtr, future::BoxFuture, std_types::ROption, type_erase::Erased, StableAbi,
};
use crate::metadata::{Doc, Source};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue};
use bincode;

/// Script bool type.
#[derive(Clone, Copy, Debug, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Bool(pub bool);

crate::HashAsDependency!(Bool);

impl From<&'_ Bool> for Dependencies {
    fn from(b: &'_ Bool) -> Self {
        depends![Bool::ergo_type(), b]
    }
}

impl From<Bool> for TypedValue<Bool> {
    fn from(b: Bool) -> Self {
        let mut v = Self::constant(b);
        Doc::set_string(&mut v, if b.0 { "<true>" } else { "<false>" });
        v
    }
}

impl std::fmt::Display for Bool {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

ergo_traits_fn! {
    crate::ergo_display_basic!(traits, Bool);

    // Anything: Into<Bool> (true)
    {
        extern "C" fn to_bool<'a>(_data: &'a Erased, _v: crate::Value) ->
            BoxFuture<'a, crate::Value> {
            BoxFuture::new(async move {
                // TODO eval `v` first?
                Bool(true).into()
            })
        }
        traits.add_generator_by_trait_for_trait::<traits::IntoTyped<Bool>>(|_traits, _type| {
            ROption::RSome(traits::IntoTypedImpl::<Bool> {
                into_typed: unsafe { FnPtr::new(to_bool) },
                ergo_trait_data: Default::default(),
                _phantom0: Default::default(),
            })
        });
    }

    impl traits::Stored for Bool {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while storing this value"))
                ],
                { bincode::serialize_into(item, &self.0) }
            ).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            crate::error_info!({
                bincode::deserialize_from(item).map(|b| Erased::new(Bool(b)))
            }).into()
        }
    }

    crate::ergo_type_name!(traits, Bool);
    traits::ValueByContent::add_impl::<Bool>(traits);
}
