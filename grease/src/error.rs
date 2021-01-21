//! Grease errors.
//!
//! The `Error` type can be created from any type supporting `std::error::Error`, which is
//! convenient with `std::ops::Try`.
//!
//! `Error` also supports aggregate errors, and thus supports `FromIterator<Error>`.

use crate::type_erase::Eraseable;
use crate::uuid::*;
use abi_stable::{
    rvec, sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, RBoxError, RVec},
    DynTrait, StableAbi,
};
use lazy_static::lazy_static;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

/// An external error.
pub type ExternalError = dyn std::error::Error + Send + Sync;

/// Grease result type, with a grease Error.
pub type Result<T> = std::result::Result<T, Error>;

/// Grease abi-stable result type, with a grease Error.
pub type RResult<T> = abi_stable::std_types::RResult<T, Error>;

lazy_static! {
    /// The error type namespace UUID.
    pub static ref NAMESPACE_ERROR: Uuid = grease_uuid(b"error");
}

/// Create a new error Uuid with the given string digest.
pub fn grease_error_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*NAMESPACE_ERROR, name)
}

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

/// A type that can be a grease error.
pub trait GreaseError: std::fmt::Display + std::fmt::Debug + Eraseable + StableAbi {
    /// Get the unique identifier for this error type.
    fn grease_error_id() -> Uuid;

    /// Get the source(s) of this error.
    fn source(&self) -> Vec<&BoxGreaseError> {
        vec![]
    }
}

#[sabi_trait]
trait BoxErrorInterface: Debug + Display + Send + Sync {
    #[sabi(last_prefix_field)]
    fn source(&self) -> RVec<&BoxGreaseError>;
}

/// A boxed grease error.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct BoxGreaseError {
    id: Uuid,
    error: BoxErrorInterface_TO<'static, RBox<()>>,
}

impl BoxGreaseError {
    /// Create a new boxed grease error from the given grease error type.
    pub fn new<E: GreaseError>(err: E) -> Self {
        BoxGreaseError {
            id: E::grease_error_id(),
            error: BoxErrorInterface_TO::from_value(err, TU_Opaque),
        }
    }

    /// Create an error from an external error.
    pub fn from_external<E: std::error::Error + Send + Sync + Sized + 'static>(err: E) -> Self {
        Self::new(RBoxError::new(err))
    }

    /// Create an error from a boxed external error.
    pub fn from_external_box(err: Box<ExternalError>) -> Self {
        Self::new(RBoxError::from_box(err))
    }

    /// Get the source(s) of this error.
    pub fn source(&self) -> Vec<&BoxGreaseError> {
        self.error.source().into_vec()
    }

    /// Downcast the boxed error to the given grease error type.
    pub fn downcast_ref<E: GreaseError>(&self) -> Option<&E> {
        if self.id == E::grease_error_id() {
            Some(unsafe { self.error.obj.unchecked_as_unerased::<E>() })
        } else {
            None
        }
    }

    /// Downcast the boxed error as an external error.
    pub fn downcast_ref_external<E: std::error::Error + 'static>(&self) -> Option<&E> {
        self.downcast_ref::<RBoxError>()
            .and_then(|r| r.downcast_ref::<E>())
    }

    /// Return whether all errors within this error satisfy the predicate.
    ///
    /// If the predicate returns None, checks whether all sources satisfy the predicate.
    pub fn all<F>(&self, f: F) -> bool
    where
        F: Fn(&Self) -> Option<bool> + Clone,
    {
        match f(self) {
            None => {
                let sources = self.source();
                if sources.is_empty() {
                    return false;
                }
                for src in sources {
                    if !src.all(f.clone()) {
                        return false;
                    }
                }
                true
            }
            Some(v) => v,
        }
    }
}

impl<E: GreaseError> BoxErrorInterface for E {
    fn source(&self) -> RVec<&BoxGreaseError> {
        self.source().into()
    }
}

impl GreaseError for RBoxError {
    fn grease_error_id() -> Uuid {
        grease_error_uuid(b"grease_external_error")
    }
}

impl std::fmt::Display for BoxGreaseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl std::error::Error for BoxGreaseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.source().first() {
            None => None,
            Some(e) => Some(*e),
        }
    }
}

/// Grease errors may be aborted, new errors, or nested sets of errors.
#[derive(Clone, Debug, StableAbi)]
#[repr(u8)]
enum InnerError {
    Aborted,
    New(RArc<BoxGreaseError>),
    Nested(RVec<RArc<BoxGreaseError>>),
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
    /// Create a new error.
    pub fn new<E: GreaseError>(e: E) -> Self {
        Self::new_boxed(BoxGreaseError::new(e))
    }

    /// Create a new error from a BoxGreaseError.
    pub fn new_boxed(v: BoxGreaseError) -> Self {
        Error {
            inner: InnerError::New(RArc::new(v)),
        }
    }

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
                            InnerError::New(_) => rvec![RArc::new(BoxGreaseError::new(e))],
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
            Self::new(ErrorContext::new(self, context))
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

    /// Returns the root sources of this error.
    ///
    /// Unlike `source()`, this may return this error (if no source is available).
    ///
    /// This only traverses GreaseErrors.
    pub fn root_errors(&self) -> Vec<Self> {
        let sources = GreaseError::source(self);
        if sources.is_empty() {
            debug_assert!(self.is_aborted());
            vec![]
        } else {
            let mut to_visit = sources;
            let mut result = Vec::new();
            while let Some(e) = to_visit.pop() {
                match e.downcast_ref::<Error>() {
                    Some(e) => result.extend(e.root_errors()),
                    None => to_visit.extend(e.source()),
                }
            }
            if result.is_empty() {
                vec![self.clone()]
            } else {
                result
            }
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

    /// Return whether all errors within this error satisfy the predicate.
    ///
    /// If there are no sources (this is an aborted error), returns false.
    pub fn all<F>(&self, f: F) -> bool
    where
        F: Fn(&BoxGreaseError) -> Option<bool> + Clone,
    {
        let sources = GreaseError::source(self);
        if sources.is_empty() {
            false
        } else {
            for src in sources {
                if !src.all(f.clone()) {
                    return false;
                }
            }
            true
        }
    }
}

impl GreaseError for Error {
    fn grease_error_id() -> Uuid {
        grease_error_uuid(b"grease_error")
    }

    fn source(&self) -> Vec<&BoxGreaseError> {
        match &self.inner {
            InnerError::Aborted => vec![],
            InnerError::New(v) => vec![v.as_ref()],
            InnerError::Nested(v) => v.iter().map(|v| v.as_ref()).collect(),
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
            Err(e) => Error::new_boxed(BoxGreaseError::from_external_box(e)),
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

#[derive(StableAbi)]
#[repr(C)]
#[sabi(impl_InterfaceType(Debug, Display, Send, Sync))]
struct ContextInterface;

#[derive(Debug, StableAbi)]
#[repr(C)]
struct ErrorContext {
    err: BoxGreaseError,
    context: DynTrait<'static, RBox<()>, ContextInterface>,
}

impl ErrorContext {
    pub fn new<T: std::fmt::Debug + std::fmt::Display + Send + Sync + 'static>(
        err: Error,
        context: T,
    ) -> Self {
        ErrorContext {
            err: BoxGreaseError::new(err),
            context: DynTrait::from_any_value(context, ContextInterface),
        }
    }
}

impl std::fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}", self.err)?;
        write!(f, "note: {}", self.context)?;
        Ok(())
    }
}

impl GreaseError for ErrorContext {
    fn grease_error_id() -> Uuid {
        grease_error_uuid(b"error_context")
    }

    fn source(&self) -> Vec<&BoxGreaseError> {
        vec![&self.err]
    }
}

#[derive(Debug)]
struct HashBySource(Error);

impl HashBySource {
    fn as_ptr(&self) -> *const BoxGreaseError {
        match &self.0.inner {
            InnerError::New(a) => a.as_ref() as *const BoxGreaseError,
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
        let mut added = false;
        for err in err.root_errors() {
            match &err.inner {
                InnerError::New(_) => {
                    added |= self.0.insert(HashBySource(err));
                }
                _ => (),
            }
        }
        added
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
