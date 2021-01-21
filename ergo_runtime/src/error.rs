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

/// A marker indicating an error is a binding error.
///
/// This is intended to wrap errors that are meant to be recoverable.
///
/// For instance, an error from evaluating a function after binding its arguments should not be
/// recoverable (since functions themselves are `crate::types::Unbound`).
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct BindError {
    error: BoxGreaseError,
}

impl std::fmt::Display for BindError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl GreaseError for BindError {
    fn grease_error_id() -> grease::uuid::Uuid {
        grease_error_uuid(b"ergo::bind_error")
    }

    fn source(&self) -> Vec<&BoxGreaseError> {
        vec![&self.error]
    }
}

/// A marker to indicate that inner BindErrors should not be considered.
#[derive(Debug, StableAbi)]
#[repr(C)]
struct IgnoreBindError {
    error: BoxGreaseError,
}

impl std::fmt::Display for IgnoreBindError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl GreaseError for IgnoreBindError {
    fn grease_error_id() -> grease::uuid::Uuid {
        grease_error_uuid(b"ergo::bind_error::ignore")
    }

    fn source(&self) -> Vec<&BoxGreaseError> {
        vec![&self.error]
    }
}

impl BindError {
    /// Wrap the given error as a bind error.
    pub fn wrap(error: grease::Error) -> grease::Error {
        grease::Error::new(BindError {
            error: BoxGreaseError::new(error),
        })
    }

    /// Consider inner bind errors as normal errors when `only_bind_errors` is called.
    pub fn unwrap(error: grease::Error) -> grease::Error {
        grease::Error::new(IgnoreBindError {
            error: BoxGreaseError::new(error),
        })
    }

    /// Check whether the given error is composed only of bind errors.
    pub fn only_bind_errors(error: &grease::Error) -> bool {
        error.all(|e| {
            if e.downcast_ref::<BindError>().is_some() {
                Some(true)
            } else if e.downcast_ref::<IgnoreBindError>().is_some() {
                Some(false)
            } else {
                None
            }
        })
    }
}
