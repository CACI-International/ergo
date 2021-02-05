//! Errors returned as part of the runtime.

use abi_stable::StableAbi;
use grease::error::{grease_error_uuid, BoxGreaseError, GreaseError};

/// The error returned when a function does not accept a specific non-positional argument.
#[derive(Debug)]
pub struct UnexpectedNonPositionalArgument;

impl std::fmt::Display for UnexpectedNonPositionalArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "extraneous non-positional argument")
    }
}

impl std::error::Error for UnexpectedNonPositionalArgument {}

/// The error returned when a function does not accept one or more positional arguments.
#[derive(Debug)]
pub struct UnexpectedPositionalArguments;

impl std::fmt::Display for UnexpectedPositionalArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "extraneous positional arguments")
    }
}

impl std::error::Error for UnexpectedPositionalArguments {}

/// A marker indicating an error is a pattern binding error.
///
/// This is intended to wrap errors that are meant to be recoverable.
///
/// For instance, an error from evaluating a function _after_ binding its arguments should _not_ be
/// recoverable (since functions themselves are `crate::types::Unbound`).
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct PatternError {
    error: BoxGreaseError,
}

impl std::fmt::Display for PatternError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl GreaseError for PatternError {
    fn grease_error_id() -> grease::uuid::Uuid {
        grease_error_uuid(b"ergo::pattern_error")
    }

    fn source(&self) -> Vec<&BoxGreaseError> {
        vec![&self.error]
    }
}

/// A marker to indicate that inner PatternErrors should not be considered.
#[derive(Debug, StableAbi)]
#[repr(C)]
struct IgnorePatternError {
    error: BoxGreaseError,
}

impl std::fmt::Display for IgnorePatternError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl GreaseError for IgnorePatternError {
    fn grease_error_id() -> grease::uuid::Uuid {
        grease_error_uuid(b"ergo::pattern_error::ignore")
    }

    fn source(&self) -> Vec<&BoxGreaseError> {
        vec![&self.error]
    }
}

impl PatternError {
    /// Wrap the given error as a pattern error.
    pub fn wrap(error: grease::Error) -> grease::Error {
        grease::Error::new(PatternError {
            error: BoxGreaseError::new(error),
        })
    }

    /// Consider inner pattern errors as normal errors when `only_pattern_errors` is called.
    pub fn unwrap(error: grease::Error) -> grease::Error {
        grease::Error::new(IgnorePatternError {
            error: BoxGreaseError::new(error),
        })
    }

    /// Check whether the given error is composed only of pattern errors.
    pub fn only_pattern_errors(error: &grease::Error) -> bool {
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
