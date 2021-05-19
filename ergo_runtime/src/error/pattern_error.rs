//! Errors from pattern matching (which may be recoverable).

use super::{BoxErgoError, ErgoError, Error};
use crate::abi_stable::{uuid::Uuid, StableAbi};

/// A marker indicating an error is a pattern binding error.
///
/// This is intended to wrap errors that are meant to be recoverable.
///
/// For instance, an error from evaluating a function _after_ binding its arguments should _not_ be
/// recoverable (since functions themselves are `crate::types::Unbound`).
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct PatternError {
    error: BoxErgoError,
}

impl std::fmt::Display for PatternError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl ErgoError for PatternError {
    fn ergo_error_id() -> Uuid {
        crate::nsid!(error::pattern)
    }

    fn source(&self) -> Vec<&BoxErgoError> {
        vec![&self.error]
    }
}

/// A marker to indicate that inner PatternErrors should not be considered.
#[derive(Debug, StableAbi)]
#[repr(C)]
struct IgnorePatternError {
    error: BoxErgoError,
}

impl std::fmt::Display for IgnorePatternError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl ErgoError for IgnorePatternError {
    fn ergo_error_id() -> Uuid {
        crate::nsid!(error::pattern::ignore)
    }

    fn source(&self) -> Vec<&BoxErgoError> {
        vec![&self.error]
    }
}

impl PatternError {
    /// Wrap the given error as a pattern error.
    pub fn wrap(error: Error) -> Error {
        Error::new(PatternError {
            error: BoxErgoError::new(error),
        })
    }

    /// Consider inner pattern errors as normal errors when `only_pattern_errors` is called.
    pub fn unwrap(error: Error) -> Error {
        Error::new(IgnorePatternError {
            error: BoxErgoError::new(error),
        })
    }

    /// Check whether the given error is composed only of pattern errors.
    pub fn only_pattern_errors(error: &Error) -> bool {
        error.all(|e| {
            if e.downcast_ref::<PatternError>().is_some() {
                Some(true)
            } else if e.downcast_ref::<IgnorePatternError>().is_some() {
                Some(false)
            } else {
                None
            }
        })
    }
}
