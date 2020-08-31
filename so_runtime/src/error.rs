//! Errors returned as part of the runtime.

use grease::value::Value;

/// The error returned when a function does not accept a specific non-positional argument.
#[derive(Debug)]
pub struct UnexpectedNonPositionalArgument(pub String);

impl std::fmt::Display for UnexpectedNonPositionalArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "the function does not accept a non-positional argument with key '{}'",
            self.0
        )
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

/// No binding with the given name is available in the current environment.
#[derive(Debug)]
pub struct MissingBinding(pub String);

impl std::fmt::Display for MissingBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "'{}' is not available in the current environment",
            &self.0
        )
    }
}

impl std::error::Error for MissingBinding {}

/// An integer index (for arrays) was expected.
#[derive(Debug)]
pub struct NonIntegerIndex;

impl std::fmt::Display for NonIntegerIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "positive integer index expected")
    }
}

impl std::error::Error for NonIntegerIndex {}

/// An expression is in call-position (had arguments) but is not callable.
#[derive(Debug)]
pub struct NonCallableExpression(pub Value);

impl std::fmt::Display for NonCallableExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "cannot pass arguments to non-callable value")
    }
}

impl std::error::Error for NonCallableExpression {}
