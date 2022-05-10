//! Stored ergo trait, allowing values to be written to and read from the store.

use crate as ergo_runtime;
use crate::abi_stable::{
    future::BoxFuture, sabi_trait, sabi_trait::prelude::*, sabi_types::RMut, std_types::RBox,
    type_erase::Erased, u128::U128, DynTrait, StableAbi,
};
use crate::type_system::ergo_trait;
use crate::{Result, Value};

/// Context passed to Stored traits.
#[derive(StableAbi)]
#[repr(C)]
pub struct StoredContext(StoredContextInterface_TO<'static, RBox<()>>);

/// The storage context interface.
#[sabi_trait]
pub trait StoredContextInterface: Sync + Send {
    fn write_value(&self, v: Value) -> BoxFuture<'_, crate::RResult<()>>;
    fn read_value(&self, id: &U128) -> BoxFuture<'_, crate::RResult<Value>>;
    fn has_value(&self, id: &U128) -> BoxFuture<'_, crate::RResult<bool>>;
}

#[derive(StableAbi)]
#[repr(C)]
#[sabi(impl_InterfaceType(IoWrite, Send, Sync))]
struct PutInterface;

#[derive(StableAbi)]
#[repr(C)]
pub struct PutData<'a>(DynTrait<'static, RMut<'a, ()>, PutInterface>, bool);

impl<'a> PutData<'a> {
    pub fn new<T: std::io::Write + Send + Sync + 'static>(
        target: &'a mut T,
        may_block: bool,
    ) -> Self {
        PutData(
            DynTrait::from_any_ptr(RMut::new(target), PutInterface),
            may_block,
        )
    }

    /// Return whether writing to this value may block.
    pub fn may_block(&self) -> bool {
        self.1
    }
}

impl<'a> std::io::Write for PutData<'a> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

#[derive(StableAbi)]
#[repr(C)]
#[sabi(impl_InterfaceType(IoRead, Send, Sync))]
struct GetInterface;

#[derive(StableAbi)]
#[repr(C)]
pub struct GetData<'a>(DynTrait<'static, RMut<'a, ()>, GetInterface>, bool);

impl<'a> GetData<'a> {
    pub fn new<T: std::io::Read + Send + Sync + 'static>(
        target: &'a mut T,
        may_block: bool,
    ) -> Self {
        GetData(
            DynTrait::from_any_ptr(RMut::new(target), GetInterface),
            may_block,
        )
    }

    /// Return whether reading from this value may block.
    pub fn may_block(&self) -> bool {
        self.1
    }
}

impl<'a> std::io::Read for GetData<'a> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
}

/// Stored trait information.
#[ergo_trait]
pub trait Stored {
    async fn put(&self, stored_ctx: &StoredContext, data: &mut PutData<'_>) -> crate::RResult<()>;
    async fn get(stored_ctx: &StoredContext, data: &mut GetData<'_>) -> crate::RResult<Erased>;
}

impl StoredContext {
    /// Create a new StoredContext from the given interface.
    pub fn new<T: StoredContextInterface + 'static>(context: T) -> Self {
        StoredContext(StoredContextInterface_TO::from_value(context, TD_Opaque))
    }

    /// Write the given value.
    pub async fn write_value(&self, v: Value) -> Result<()> {
        self.0.write_value(v).await.into()
    }

    /// Read the value with the given identity.
    pub async fn read_value(&self, id: u128) -> Result<Value> {
        self.0.read_value(&id.into()).await.into()
    }

    /// Check whether the value with the given identity is available.
    pub async fn has_value(&self, id: u128) -> Result<bool> {
        self.0.has_value(&id.into()).await.into()
    }
}

/*
/// Read a Value from the store by id.
pub async fn read_from_store(store_item: &Item, id: u128) -> Result<Value> {
    crate::error_info! {
        notes: [
            format!("reading value id {}", id)
        ],
        async {
            let item = store_item.value_id(id);
            let mut content = item.read_existing().await?;
            let mut id_bytes = [0; 16];
            content.read_exact(&mut id_bytes)?;
            let id = u128::from_be_bytes(id_bytes);
            let tp: Type = ErasedTrivial::deserialize(&mut content)?.into();
            if let Some(s) = Context::get_trait_for_type::<Stored>(&tp) {
                let stored_ctx = StoredContext::new(store_item.clone());
                let data = s.get(&stored_ctx, content).await.into_result()?;
                // TODO revisit metadata
                Ok(unsafe { Value::new(RArc::new(tp), RArc::new(data), id) })
            } else {
                Err(format!("no stored trait for {}", type_name_for(&tp)))
            }
        }
    }
}

/// Check whether a Value is in the store by id.
pub fn present_in_store(store_item: &Item, id: u128) -> bool {
    store_item.value_id(id).exists()
}

/// Write a value to the store.
pub async fn write_to_store(store_item: &Item, mut v: Value) -> Result<()> {
    let source = Source::get(&v);
    crate::error_info! {
        labels: [
            primary(source.with("while writing this value"))
        ],
        async {
            let item = store_item.value_id(v.id().await);
            // TODO should this not eval (relying on the caller to eval)?
            drop(Context::eval(&mut v).await);
            let t = Context::get_trait::<Stored>(&v)
                .ok_or_else(|| format!("no stored trait for {}", type_name(&v)))?;

            let mut content = item.write().await?;

            content.write_all(&v.id().await.to_be_bytes())?;

            let tp: ErasedTrivial = v.ergo_type().unwrap().clone().into();
            tp.serialize(&mut content)?;

            let stored_ctx = StoredContext::new(store_item.clone());
            t.put(v, &stored_ctx, content).await.into_result()
        }
    }
}
*/
