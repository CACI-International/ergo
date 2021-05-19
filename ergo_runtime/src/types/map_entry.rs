//! The MapEntry type.

use crate as ergo_runtime;
use crate::abi_stable::{type_erase::Erased, StableAbi};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, Source, TypedValue, Value};
use bincode;

/// Type for an individual Map entry.
#[derive(Clone, Debug, ErgoType, PartialEq, StableAbi)]
#[repr(C)]
pub struct MapEntry {
    pub key: Source<Value>,
    pub value: Source<Value>,
}

impl From<&'_ MapEntry> for Dependencies {
    fn from(m: &'_ MapEntry) -> Self {
        depends![MapEntry::ergo_type(), *m.key, *m.value]
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
            async move {
                traits::display(CONTEXT, self.key.as_ref().unwrap().clone(), f).await?;
                write!(f, " = ")?;
                traits::display(CONTEXT, self.value.as_ref().unwrap().clone(), f).await?;
                Ok(())
            }.await.into()

        }
    }

    traits::Nested::add_impl::<MapEntry>(traits);

    impl traits::Stored for MapEntry {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            async move {
                let ids = (self.key.id(), self.value.id());
                stored_ctx.write_to_store(CONTEXT, self.key.as_ref().unwrap().clone()).await?;
                stored_ctx.write_to_store(CONTEXT, self.value.as_ref().unwrap().clone()).await?;
                Ok(bincode::serialize_into(item, &ids)?)
            }.await.into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            async move {
                let ids: (u128, u128) = bincode::deserialize_from(item)?;
                let key = stored_ctx.read_from_store(CONTEXT, ids.0).await?;
                let value = stored_ctx.read_from_store(CONTEXT, ids.1).await?;
                Ok(Erased::new(MapEntry { key: Source::stored(key), value: Source::stored(value) }))
            }.await.into()
        }
    }

    impl traits::Bind for MapEntry {
        async fn bind(&self, arg: Source<Value>) -> Value {
            // TODO pattern errors?
            let ind = crate::err_return_value!(CONTEXT.eval_as::<super::Index>(arg).await).unwrap().to_owned().0;
            let (src, ind) = crate::err_return_value!(CONTEXT.eval_as::<super::String>(ind).await).take();
            let s = ind.as_ref().as_str();
            if s == "key" {
                self.key.as_ref().unwrap().clone()
            } else if s == "value" {
                self.value.as_ref().unwrap().clone()
            } else {
                src.with("unknown index").into_error().into()
            }
        }
    }

    crate::ergo_type_name!(traits, MapEntry);
    traits::ValueByContent::add_nested_impl::<MapEntry>(traits);
}
