//! Runtime error scope.
//!
//! The error scope is used to give feedback about errors while other (concurrent) computations may
//! be occurring.

use crate::abi_stable::{
    external_types::RMutex,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox},
    StableAbi,
};
use crate::Error;

#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct ErrorScope {
    on_error: RArc<RMutex<ErrorScopeInnerInterface_TO<'static, RBox<()>>>>,
}

impl std::fmt::Debug for ErrorScope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("ErrorScope").finish()
    }
}

#[sabi_trait]
trait ErrorScopeInnerInterface: Send + Sync {
    fn len(&self) -> usize;

    #[sabi(last_prefix_field)]
    fn error(&mut self, e: &Error);
}

struct ErrorScopeInner<F> {
    seen: crate::error::Diagnostics,
    on_error: F,
}

impl<F> ErrorScopeInner<F> {
    pub fn new(on_error: F) -> Self {
        ErrorScopeInner {
            seen: Default::default(),
            on_error,
        }
    }
}

impl<F> ErrorScopeInnerInterface for ErrorScopeInner<F>
where
    F: Fn(Error) + Send + Sync + 'static,
{
    fn len(&self) -> usize {
        self.seen.len()
    }

    fn error(&mut self, e: &Error) {
        if self.seen.insert(e) {
            (self.on_error)(e.clone())
        }
    }
}

impl Default for ErrorScope {
    fn default() -> Self {
        Self::new(|_| ())
    }
}

impl ErrorScope {
    pub fn new<F>(on_error: F) -> Self
    where
        F: Fn(Error) + Send + Sync + 'static,
    {
        ErrorScope {
            on_error: RArc::new(RMutex::new(ErrorScopeInnerInterface_TO::from_value(
                ErrorScopeInner::new(on_error),
                TD_Opaque,
            ))),
        }
    }

    pub fn error(&self, e: &Error) {
        self.on_error.lock().error(e);
    }

    pub fn count(&self) -> usize {
        self.on_error.lock().len()
    }
}
