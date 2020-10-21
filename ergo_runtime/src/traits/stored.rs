//! Stored grease trait, allowing values to be written to and read from the store.

use super::type_name;
use crate::{types, Result};
use abi_stable::{std_types::RVec, StableAbi};
use bincode;
use grease::{
    bst::BstMap,
    grease_trait, grease_traits_fn,
    path::PathBuf,
    runtime::{Context, Item, ItemContent},
    type_erase::{Erased, ErasedTrivial},
    types::Type,
    value::Value,
};
use std::collections::BTreeMap;

/// Context passed to Stored traits.
#[derive(StableAbi)]
#[repr(C)]
pub struct StoredContext {
    store_item: Item,
}

/// Stored trait information.
#[grease_trait]
pub trait Stored {
    async fn put(&self, stored_ctx: &StoredContext, item: ItemContent);
    async fn get(stored_ctx: &StoredContext, item: ItemContent) -> Erased;
}

impl StoredContext {
    fn new(store_item: Item) -> Self {
        StoredContext { store_item }
    }

    /// Read the given value from the store.
    pub async fn read_from_store(&self, ctx: &Context, id: u128) -> Result<Value> {
        read_from_store(ctx, &self.store_item, id).await
    }

    /// Write the given value to the store.
    pub async fn write_to_store(&self, ctx: &Context, v: Value) -> Result<()> {
        write_to_store(ctx, &self.store_item, v).await
    }
}

grease_traits_fn! {
    impl Stored for types::Unit {
        async fn put(&self, _stored_ctx: &StoredContext, item: ItemContent) {
            bincode::serialize_into(item, self)?
        }

        async fn get(_stored_ctx: &StoredContext, item: ItemContent) -> Erased {
            let t: types::Unit = bincode::deserialize_from(item)?;
            Erased::new(t)
        }
    }

    impl Stored for types::String {
        async fn put(&self, _stored_ctx: &StoredContext, item: ItemContent) {
            bincode::serialize_into(item, self)?
        }

        async fn get(_stored_ctx: &StoredContext, item: ItemContent) -> Erased {
            let t: types::String = bincode::deserialize_from(item)?;
            Erased::new(t)
        }
    }

    impl Stored for PathBuf {
        async fn put(&self, _stored_ctx: &StoredContext, item: ItemContent) {
            bincode::serialize_into(item, self.as_ref().as_ref())?
        }

        async fn get(_stored_ctx: &StoredContext, item: ItemContent) -> Erased {
            let t: std::path::PathBuf = bincode::deserialize_from(item)?;
            Erased::new(PathBuf::from(t))
        }
    }

    impl Stored for types::Array {
        async fn put(&self, stored_ctx: &StoredContext, item: ItemContent) {
            let mut ids: Vec<u128> = Vec::new();
            for v in self.0.iter().cloned() {
                ids.push(v.id());
                stored_ctx.write_to_store(CONTEXT, v).await?;
            }
            bincode::serialize_into(item, &ids)?
        }

        async fn get(stored_ctx: &StoredContext, item: ItemContent) -> Erased {
            let ids: Vec<u128> = bincode::deserialize_from(item)?;
            let mut vals = RVec::new();
            for id in ids {
                vals.push(stored_ctx.read_from_store(CONTEXT, id).await?);
            }
            Erased::new(types::Array(vals))
        }
    }

    impl Stored for types::Map {
        async fn put(&self, stored_ctx: &StoredContext, item: ItemContent) {
            let mut ids: BTreeMap<String, u128> = BTreeMap::new();
            for (k, v) in self.0.iter() {
                let v = v.clone();
                ids.insert(k.clone().into(), v.id());
                stored_ctx.write_to_store(CONTEXT, v).await?;
            }
            bincode::serialize_into(item, &ids)?
        }

        async fn get(stored_ctx: &StoredContext, item: ItemContent) -> Erased {
            let ids: BTreeMap<String, u128> = bincode::deserialize_from(item)?;
            let mut vals = BstMap::new();
            for (k, id) in ids {
                vals.insert(k.into(), stored_ctx.read_from_store(CONTEXT, id).await?);
            }
            Erased::new(types::Map(vals))
        }
    }
}

/// Read a Value from the store by id.
pub async fn read_from_store(ctx: &Context, store_item: &Item, id: u128) -> Result<Value> {
    let item = store_item.value_id(id);
    let mut content = item.read_existing()?;
    let tp: Type = ErasedTrivial::deserialize(&mut content)?.into();
    if let Some(mut s) = ctx.get_trait_for_type::<Stored>(&tp) {
        let stored_ctx = StoredContext::new(store_item.clone());
        let data = s.get(&stored_ctx, content).await?;
        // TODO revisit metadata
        Ok(Value::from_raw(
            tp.into(),
            std::sync::Arc::new(data),
            Default::default(),
            id,
        ))
    } else {
        Err(format!("no stored trait for {}", type_name(ctx, &tp).await?).into())
    }
}

/// Write a value to the store.
///
/// The value must already be forced (using `force_value_nested`).
pub async fn write_to_store(ctx: &Context, store_item: &Item, mut v: Value) -> Result<()> {
    if let Some(mut s) = ctx.get_trait::<Stored>(&v) {
        let item = store_item.value(&v);
        let mut content = item.write()?;

        let tp: ErasedTrivial = v.grease_type().await?.clone().into();
        tp.serialize(&mut content)?;

        let stored_ctx = StoredContext::new(store_item.clone());
        s.put(v, &stored_ctx, content).await
    } else {
        Err(format!(
            "no stored trait for {}",
            type_name(ctx, v.grease_type().await?).await?
        )
        .into())
    }
}
