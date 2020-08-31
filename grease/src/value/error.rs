//! Value errors.
//!
//! The `Error` type can be created from any type supporting `std::error::Error`, which is
//! convenient with `std::ops::Try`.
//!
//! `Error` also supports aggregate errors, and thus supports `FromIterator<Error>`.
//!
//! These errors support a thread-local error signal within the grease runtime. When a new error is
//! created, it will call this signal function (if set in the runtime).

use crate::runtime::call_on_error;
use abi_stable::{
    rvec,
    std_types::{RArc, RBoxError, RVec},
    StableAbi,
};

/// An external error.
pub type ExternalError = dyn std::error::Error + Send + Sync;

/// Value error type.
///
/// The type does not implement `Error` itself (so that `From<T: Error>` can be implemented), but
/// you can get such an error with the `Error::error()` function.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Error {
    inner: InnerError,
}

/// A wrapped `Error` which implements `std::error::Error`.
#[derive(Clone, Debug)]
pub struct WrappedError {
    inner: InnerError,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl std::fmt::Display for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

/// Value errors may be aborted, new errors, or nested sets of errors.
#[derive(Clone, Debug, StableAbi)]
#[repr(u8)]
enum InnerError {
    Aborted,
    New(RArc<RBoxError>),
    Nested(RVec<RArc<RBoxError>>),
}

impl std::fmt::Display for InnerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InnerError::*;
        match self {
            Aborted => write!(f, "aborted"),
            New(e) => write!(f, "{}", e),
            Nested(es) => {
                for e in es {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for InnerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            InnerError::New(e) => Some(e.as_ref()),
            _ => None,
        }
    }
}

impl std::error::Error for WrappedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

impl Error {
    /// Change the error into a type supporting `std::error::Error`.
    pub fn error(self) -> WrappedError {
        WrappedError { inner: self.inner }
    }

    /// Get a reference to the error as an external error.
    pub fn error_ref(&self) -> &ExternalError {
        self.as_ref()
    }

    /// Aggregate multiple errors into a single error.
    pub fn aggregate<I: IntoIterator<Item = Self>>(i: I) -> Self {
        let errs: Vec<Error> = i.into_iter().collect();

        if errs.len() == 1 {
            errs.into_iter().next().unwrap()
        } else {
            Error {
                inner: InnerError::Nested(
                    errs.into_iter()
                        .map(|e| match e.inner {
                            InnerError::Aborted => rvec![],
                            InnerError::New(e) => rvec![e],
                            InnerError::Nested(es) => es,
                        })
                        .flatten()
                        .collect(),
                ),
            }
        }
    }

    /// Add context information to the error.
    pub fn with_context<T: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static>(
        self,
        context: T,
    ) -> Self {
        ErrorContext::new(self, context).into()
    }

    /// Create an aborted error.
    pub(crate) fn aborted() -> Self {
        Error {
            inner: InnerError::Aborted,
        }
    }
}

impl AsRef<ExternalError> for Error {
    fn as_ref(&self) -> &ExternalError {
        &self.inner
    }
}

fn has_inner_error(v: &(dyn std::error::Error + 'static)) -> bool {
    if v.is::<InnerError>() {
        true
    } else if let Some(s) = v.source() {
        has_inner_error(s)
    } else {
        false
    }
}

impl<T> From<T> for Error
where
    T: Into<Box<ExternalError>>,
{
    fn from(v: T) -> Self {
        let ext: Box<ExternalError> = v.into();
        Error {
            inner: match ext.downcast::<WrappedError>() {
                Ok(v) => v.inner,
                // TODO: traverse source() to find WrappedError?
                Err(e) => {
                    if !has_inner_error(e.as_ref()) {
                        call_on_error();
                    }
                    InnerError::New(RArc::new(RBoxError::from_box(e.into())))
                }
            },
        }
    }
}

impl From<&'_ Self> for Error {
    fn from(v: &Self) -> Self {
        v.clone()
    }
}

impl std::iter::FromIterator<Error> for Error {
    fn from_iter<I: IntoIterator<Item = Error>>(iter: I) -> Self {
        Self::aggregate(iter)
    }
}

#[derive(Debug)]
struct ErrorContext<T> {
    err: Error,
    context: T,
}

impl<T> ErrorContext<T> {
    pub fn new(err: Error, context: T) -> Self {
        ErrorContext { err, context }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for ErrorContext<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.err)?;
        write!(f, "note: {}", self.context)?;
        Ok(())
    }
}

impl<T: std::fmt::Display + std::fmt::Debug> std::error::Error for ErrorContext<T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.err.error_ref())
    }
}
