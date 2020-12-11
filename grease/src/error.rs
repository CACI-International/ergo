//! Grease errors.
//!
//! The `Error` type can be created from any type supporting `std::error::Error`, which is
//! convenient with `std::ops::Try`.
//!
//! `Error` also supports aggregate errors, and thus supports `FromIterator<Error>`.

use abi_stable::{
    rvec,
    std_types::{RArc, RBoxError, RVec},
    StableAbi,
};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

/// An external error.
pub type ExternalError = dyn std::error::Error + Send + Sync;

/// Grease result type, with a grease Error.
pub type Result<T> = std::result::Result<T, Error>;

/// Grease abi-stable result type, with a grease Error.
pub type RResult<T> = abi_stable::std_types::RResult<T, Error>;

/// Grease error type.
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

/// Downcast an error that is potentially an `Error`, accounting for inner type erasure.
pub fn downcast_ref<'a, T: std::error::Error + 'static>(
    error: &'a (dyn std::error::Error + 'static),
) -> Option<&'a T> {
    error.downcast_ref::<T>().or_else(|| {
        error
            .downcast_ref::<RBoxError>()
            .and_then(|e| e.downcast_ref::<T>())
    })
}

/// Grease errors may be aborted, new errors, or nested sets of errors.
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
                        .map(|mut e| match &mut e.inner {
                            InnerError::Aborted => rvec![],
                            InnerError::New(_) => {
                                rvec![RArc::new(RBoxError::from_box(e.error().into()))]
                            }
                            InnerError::Nested(es) => std::mem::take(es),
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
        if self.is_aborted() {
            self
        } else {
            ErrorContext::new(self, context).into()
        }
    }

    /// Create an aborted error.
    ///
    /// Abborted errors will be dropped/removed when aggregating errors.
    pub fn aborted() -> Self {
        Error {
            inner: InnerError::Aborted,
        }
    }

    /// Returns the root source of this error.
    ///
    /// Unlike `source()`, this may return this error (if no source is available).
    ///
    /// This is the root error traversing known error types, namely nested `Error`s.
    pub fn root_error(&self) -> Self {
        let mut inner = &self.inner;
        let mut src: &dyn std::error::Error = self.error_ref();
        // Traverse all sources, tracking the last source that was an InnerError.
        while let Some(nsrc) = src.source() {
            src = nsrc;
            if let Some(e) = downcast_ref::<WrappedError>(src) {
                inner = &e.inner;
            } else if let Some(e) = downcast_ref::<InnerError>(src) {
                inner = e;
            }
        }
        Error {
            inner: inner.clone(),
        }
    }

    /// Whether this error is an aggregate error.
    pub fn is_aggregate(&self) -> bool {
        if let InnerError::Nested(_) = &self.inner {
            true
        } else {
            false
        }
    }

    /// Whether this error is an aborted error.
    pub fn is_aborted(&self) -> bool {
        if let InnerError::Aborted = &self.inner {
            true
        } else {
            false
        }
    }
}

impl AsRef<ExternalError> for Error {
    fn as_ref(&self) -> &ExternalError {
        &self.inner
    }
}

impl<T> From<T> for Error
where
    T: Into<Box<ExternalError>>,
{
    fn from(v: T) -> Self {
        let ext: Box<ExternalError> = v.into();
        match ext.downcast::<WrappedError>() {
            Ok(v) => Error { inner: v.inner },
            Err(e) => Error {
                inner: InnerError::New(RArc::new(RBoxError::from_box(e))),
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

#[derive(Debug)]
struct HashBySource(Error);

impl HashBySource {
    fn as_ptr(&self) -> *const RBoxError {
        match &self.0.inner {
            InnerError::New(a) => a.as_ref() as *const RBoxError,
            _ => panic!("invalid entry in UniqueErrorSources"),
        }
    }
}

impl Hash for HashBySource {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.as_ptr().hash(h)
    }
}

impl PartialEq for HashBySource {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl Eq for HashBySource {}

/// A set to track errors with referentially-unique sources.
#[derive(Default, Debug)]
pub struct UniqueErrorSources(HashSet<HashBySource>);

impl UniqueErrorSources {
    /// Create an empty set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert an error into the set.
    pub fn insert(&mut self, err: Error) -> bool {
        let err = err.root_error();
        match &err.inner {
            InnerError::New(_) => (),
            _ => return false,
        }
        self.0.insert(HashBySource(err))
    }

    /// Return the number of unique errors in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Iterate over the errors in the set.
    pub fn iter(&self) -> impl Iterator<Item = &Error> {
        self.0.iter().map(|h| &h.0)
    }
}
