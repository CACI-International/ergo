//! Stored ergo trait, allowing values to be written to and read from the store.

use crate as ergo_runtime;
use crate::abi_stable::{
    future::BoxFuture,
    sabi_trait,
    sabi_trait::prelude::*,
    sabi_types::RMut,
    std_types::{RIoError, RResult, RSlice, RSliceMut},
    type_erase::Erased,
    u128::U128,
    StableAbi,
};
use crate::type_system::ergo_trait;
use crate::{Result, Value};

/// Stored trait information.
#[ergo_trait]
pub trait Stored {
    async fn put(&self, data: &mut PutData<'_>) -> crate::RResult<()>;
    async fn get(data: &mut GetData<'_>) -> crate::RResult<Erased>;
}

/// Data interface passed to the put method.
#[derive(StableAbi)]
#[repr(C)]
pub struct PutData<'a>(PutDataInterface_TO<'a, RMut<'a, ()>>);

/// The put data interface.
#[sabi_trait]
pub trait PutDataInterface: Sync + Send {
    fn write(&mut self, buf: RSlice<'_, u8>) -> RResult<usize, RIoError>;
    fn flush(&mut self) -> RResult<(), RIoError>;

    fn write_value(&self, v: Value) -> BoxFuture<'_, crate::RResult<()>>;
    fn may_block(&self) -> bool;
}

impl<'a> PutData<'a> {
    pub fn new<T: PutDataInterface + 'a>(iface: &'a mut T) -> Self {
        PutData(PutDataInterface_TO::from_ptr(RMut::new(iface), TD_Opaque))
    }

    /// Return whether writing to this value may block.
    pub fn may_block(&self) -> bool {
        self.0.may_block()
    }

    /// Write the given value.
    pub async fn write_value(&self, v: Value) -> Result<()> {
        self.0.write_value(v).await.into()
    }
}

impl<'a> std::io::Write for PutData<'a> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf.into()).map_err(|e| e.into()).into()
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush().map_err(|e| e.into()).into()
    }
}

/// The get data interface.
#[sabi_trait]
pub trait GetDataInterface: Sync + Send {
    fn read(&mut self, buf: RSliceMut<'_, u8>) -> RResult<usize, RIoError>;

    fn read_value(&self, id: &U128) -> BoxFuture<'_, crate::RResult<Value>>;
    fn has_value(&self, id: &U128) -> BoxFuture<'_, crate::RResult<bool>>;
    fn may_block(&self) -> bool;
}

#[derive(StableAbi)]
#[repr(C)]
pub struct GetData<'a>(GetDataInterface_TO<'a, RMut<'a, ()>>);

impl<'a> GetData<'a> {
    pub fn new<T: GetDataInterface + 'a>(iface: &'a mut T) -> Self {
        GetData(GetDataInterface_TO::from_ptr(RMut::new(iface), TD_Opaque))
    }

    /// Return whether reading from this value may block.
    pub fn may_block(&self) -> bool {
        self.0.may_block()
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

impl<'a> std::io::Read for GetData<'a> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf.into()).map_err(|e| e.into()).into()
    }
}
