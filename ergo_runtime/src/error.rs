//! Errors returned as part of the runtime.

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
#[derive(Debug)]
pub struct BindError {
    pub error: grease::Error,
}

impl std::fmt::Display for BindError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl std::error::Error for BindError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.error.error_ref())
    }
}

impl BindError {
    /// Wrap the given error as a bind error.
    pub fn wrap(error: grease::Error) -> grease::Error {
        BindError { error }.into()
    }

    /// Check whether the given error is composed only of BindErrors.
    pub fn only_within(error: &grease::Error) -> bool {
        grease::error::all(error.error_ref(), |e| {
            grease::error::downcast_ref::<Self>(e).is_some()
        })
    }
}
