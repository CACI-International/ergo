//! The MapEntry type.

use crate as ergo_runtime;
use crate::abi_stable::{type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue, Value};
use bincode;

/// Type for an individual Map entry.
#[derive(Clone, Debug, ErgoType, PartialEq, StableAbi)]
#[repr(C)]
pub struct MapEntry {
    pub key: Value,
    pub value: Value,
}

impl From<&'_ MapEntry> for Dependencies {
    fn from(m: &'_ MapEntry) -> Self {
        depends![MapEntry::ergo_type(), m.key, m.value]
    }
}

impl From<MapEntry> for TypedValue<MapEntry> {
    fn from(v: MapEntry) -> Self {
        Self::constant(v)
    }
}

impl traits::NestedValues for MapEntry {
    fn nested_values(&self) -> Vec<&Value> {
        vec![&self.key, &self.value]
    }

    fn nested_values_mut(&mut self) -> Vec<&mut Value> {
        vec![&mut self.key, &mut self.value]
    }
}

ergo_traits_fn! {
    impl traits::Display for MapEntry {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::error::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while displaying this value")) ],
                async {
                    traits::display(self.key.clone(), f).await?;
                    write!(f, " = ")?;
                    traits::display(self.value.clone(), f).await?;
                    crate::Result::Ok(())
                }
            ).into()
        }
    }

    traits::Nested::add_impl::<MapEntry>(traits);

    impl traits::Stored for MapEntry {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                async {
                    let ids = (self.key.id(), self.value.id());
                    stored_ctx.write_to_store(self.key.clone()).await?;
                    stored_ctx.write_to_store(self.value.clone()).await?;
                    bincode::serialize_into(item, &ids)
                }
            ).into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            crate::error_info!(
                async {
                    let ids: (u128, u128) = bincode::deserialize_from(item)?;
                    let key = stored_ctx.read_from_store(ids.0).await?;
                    let value = stored_ctx.read_from_store(ids.1).await?;
                    crate::Result::Ok(Erased::new(MapEntry { key, value }))
                }
            ).into()
        }
    }

    impl traits::Bind for MapEntry {
        async fn bind(&self, arg: Value) -> Value {
            let ind = crate::try_result!(crate::Context::eval_as::<super::Index>(arg).await).to_owned().0;
            let ind = crate::try_result!(crate::Context::eval_as::<super::String>(ind).await);
            let s = ind.as_ref().as_str();
            if s == "key" {
                self.key.clone()
            } else if s == "value" {
                self.value.clone()
            } else {
                crate::error!(
                    labels: [ primary(Source::get(&ind).with("")) ],
                    notes: [ "only `key` and `value` indices supported" ],
                    error: "unrecognized MapEntry index"
                ).into()
            }
        }
    }

    crate::ergo_type_name!(traits, MapEntry);
    traits::ValueByContent::add_nested_impl::<MapEntry>(traits);
}
