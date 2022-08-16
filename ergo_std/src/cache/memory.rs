//! A memory-backed cache.

use ergo_runtime::{Result, Value};
use futures::future::FutureExt;
use futures::lock::Mutex as FMutex;
use parking_lot::Mutex;
use std::collections::HashMap as CacheMap;
use std::sync::Arc;

#[derive(Default)]
pub struct MemCache {
    stored: Mutex<CacheMap<u128, Arc<FMutex<Option<(u128, Value)>>>>>,
}

impl MemCache {
    async fn cache_value(
        &self,
        key: u128,
        value: Value,
        _error_handling: super::ErrorHandling,
    ) -> Result<Value> {
        self.get_entry(key, value, |v| async move { Ok(v) }).await
    }

    /// Get an entry by id, without separating keys from value identities.
    ///
    /// This is a simpler caching scheme than get_entry.
    pub async fn get_basic_entry<F>(&self, id: u128, missing: F) -> Result<Value>
    where
        F: std::future::Future<Output = Result<Value>>,
    {
        let entry = self
            .stored
            .lock()
            .entry(id)
            .or_insert_with(|| Arc::new(FMutex::new(None)))
            .clone();

        let mut guard = entry.lock().await;
        match *guard {
            None => {
                let v = missing.await?;
                *guard = Some((id, v));
            }
            Some((vid, _)) => {
                debug_assert!(vid == id);
            }
        }
        Ok(guard.as_ref().unwrap().1.clone())
    }

    /// Return whether the given entry exists.
    pub async fn has_entry(&self, id: u128) -> Option<bool> {
        let entry = self.stored.lock().get(&id)?.clone();
        let guard = entry.lock().await;
        Some(guard.is_some())
    }

    /// Get the entry for the given key/value pair.
    ///
    /// `missing` is called if the entry is not present.
    pub async fn get_entry<F, R>(&self, key: u128, value: Value, missing: F) -> Result<Value>
    where
        F: FnOnce(Value) -> R,
        R: std::future::Future<Output = Result<Value>>,
    {
        let id = value.id().await;

        let entry = self
            .stored
            .lock()
            .entry(key)
            .or_insert_with(|| Arc::new(FMutex::new(None)))
            .clone();

        let mut guard = entry.lock().await;
        match *guard {
            None => {
                let v = missing(value).await?;
                *guard = Some((id, v));
            }
            Some((vid, _)) if vid != id => {
                let v = missing(value).await?;
                *guard = Some((id, v));
            }
            _ => (),
        }
        Ok(guard.as_ref().unwrap().1.clone())
    }
}

impl super::CacheInterface for MemCache {
    fn cache_value(
        &self,
        key: super::U128,
        value: Value,
        error_handling: super::ErrorHandling,
    ) -> super::BoxFuture<'_, super::RResult<Value>> {
        super::BoxFuture::new(
            self.cache_value(key.into(), value, error_handling)
                .map(|r| r.into()),
        )
    }
}
