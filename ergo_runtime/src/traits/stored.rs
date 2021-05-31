//! Stored ergo trait, allowing values to be written to and read from the store.

use super::{type_name, type_name_for};
use crate as ergo_runtime;
use crate::abi_stable::{
    std_types::RArc,
    type_erase::{Erased, ErasedTrivial},
    StableAbi,
};
use crate::context::{Context, Item, ItemContent};
use crate::type_system::{ergo_trait, Type};
use crate::{Result, Value};

/// Context passed to Stored traits.
#[derive(StableAbi)]
#[repr(C)]
pub struct StoredContext {
    store_item: Item,
}

/// Stored trait information.
#[ergo_trait]
pub trait Stored {
    async fn put(&self, stored_ctx: &StoredContext, item: ItemContent) -> crate::RResult<()>;
    async fn get(stored_ctx: &StoredContext, item: ItemContent) -> crate::RResult<Erased>;
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

/// Read a Value from the store by id.
pub async fn read_from_store(ctx: &Context, store_item: &Item, id: u128) -> Result<Value> {
    let item = store_item.value_id(id);
    let mut content = item.read_existing()?;
    let tp: Type = ErasedTrivial::deserialize(&mut content)?.into();
    if let Some(s) = ctx.get_trait_for_type::<Stored>(&tp) {
        let stored_ctx = StoredContext::new(store_item.clone());
        let data = s.get(&stored_ctx, content).await.into_result()?;
        // TODO revisit metadata
        Ok(unsafe { Value::with_id(RArc::new(tp), RArc::new(data), id) })
    } else {
        Err(format!("no stored trait for {}", type_name_for(ctx, &tp)).into())
    }
}

/// Check whether a Value is in the store by id.
pub fn present_in_store(_ctx: &Context, store_item: &Item, id: u128) -> bool {
    store_item.value_id(id).exists()
}

/// Write a value to the store.
pub async fn write_to_store(ctx: &Context, store_item: &Item, mut v: Value) -> Result<()> {
    let item = store_item.value(&v);
    // TODO should this not eval (relying on the caller to eval)?
    ctx.eval(&mut v).await?;
    let t = ctx
        .get_trait::<Stored>(&v)
        .ok_or_else(|| format!("no stored trait for {}", type_name(ctx, &v)))?;

    let mut content = item.write()?;

    let tp: ErasedTrivial = v.ergo_type().unwrap().clone().into();
    tp.serialize(&mut content)?;

    let stored_ctx = StoredContext::new(store_item.clone());
    t.put(v, &stored_ctx, content).await.into_result()
}
