//! Allow synchronized access to a value that is bound to a particular scope.
//!
//! When the scope exits, the value is no longer accessible.

use std::sync::{Arc, RwLock, RwLockReadGuard};

pub struct Scoped<T> {
    value: Arc<RwLock<Option<T>>>,
}

#[derive(Clone)]
pub struct ScopedRef<T> {
    value: Arc<RwLock<Option<T>>>,
}

pub struct ScopedRefGuard<'lock, T>(RwLockReadGuard<'lock, Option<T>>);

impl<T> Scoped<T> {
    pub fn new_pair(value: T) -> (Self, ScopedRef<T>) {
        let lock = Arc::new(RwLock::new(Some(value)));
        (
            Scoped {
                value: lock.clone(),
            },
            ScopedRef { value: lock },
        )
    }
}

impl<T> ScopedRef<T> {
    pub fn try_get<'a>(&'a self) -> Option<ScopedRefGuard<'a, T>> {
        self.value.read().ok().and_then(|v| {
            if v.is_some() {
                Some(ScopedRefGuard(v))
            } else {
                None
            }
        })
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
        R: Default,
    {
        if let Some(s) = self.try_get() {
            f(&*s)
        } else {
            Default::default()
        }
    }
}

impl<'a, T> std::ops::Deref for ScopedRefGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl<T> Drop for Scoped<T> {
    fn drop(&mut self) {
        let mut lock = self.value.write().expect("scoped lock poisoned");
        *lock = None;
    }
}
