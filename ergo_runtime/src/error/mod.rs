//! Runtime errors.
//!
//! The `Error` type can be created from any type supporting `std::error::Error`, which is
//! convenient with `std::ops::Try`.
//!
//! `Error` also supports aggregate errors, and thus supports `FromIterator<Error>`.

use crate as ergo_runtime;
use crate::abi_stable::{
    rvec, sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, RBoxError, RVec},
    type_erase::Eraseable,
    uuid::*,
    DynTrait, StableAbi,
};
use crate::type_system::ErgoType;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

mod pattern_error;

pub use pattern_error::PatternError;

/// An external error.
pub type ExternalError = dyn std::error::Error + Send + Sync;

/// Ergo result type, with an Error.
pub type Result<T> = std::result::Result<T, Error>;

/// Ergo abi-stable result type, with an Error.
pub type RResult<T> = crate::abi_stable::std_types::RResult<T, Error>;

/// Ergo error type.
///
/// The type does not implement `Error` itself (so that `From<T: Error>` can be implemented), but
/// you can get such an error with the `Error::error()` function.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Error {
    inner: InnerError,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

/// A type that can be an ergo error.
pub trait ErgoError: std::fmt::Display + std::fmt::Debug + Eraseable + StableAbi {
    /// Get the unique identifier for this error type.
    fn ergo_error_id() -> Uuid;

    /// Get the source(s) of this error.
    fn source(&self) -> Vec<&BoxErgoError> {
        vec![]
    }
}

#[sabi_trait]
trait BoxErrorInterface: Debug + Display + Send + Sync {
    #[sabi(last_prefix_field)]
    fn source(&self) -> RVec<&BoxErgoError>;
}

/// A boxed ergo error.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct BoxErgoError {
    id: Uuid,
    error: BoxErrorInterface_TO<'static, RBox<()>>,
}

impl BoxErgoError {
    /// Create a new boxed ergo error from the given ergo error type.
    pub fn new<E: ErgoError>(err: E) -> Self {
        BoxErgoError {
            id: E::ergo_error_id(),
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
    pub fn source(&self) -> Vec<&BoxErgoError> {
        self.error.source().into_vec()
    }

    /// Downcast the boxed error to the given ergo error type.
    pub fn downcast_ref<E: ErgoError>(&self) -> Option<&E> {
        if self.id == E::ergo_error_id() {
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

impl<E: ErgoError> BoxErrorInterface for E {
    fn source(&self) -> RVec<&BoxErgoError> {
        self.source().into()
    }
}

impl ErgoError for RBoxError {
    fn ergo_error_id() -> Uuid {
        crate::nsid!(error::external)
    }
}

impl std::fmt::Display for BoxErgoError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl std::error::Error for BoxErgoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.source().first() {
            None => None,
            Some(e) => Some(*e),
        }
    }
}

/// Ergo errors may be aborted or some set of errors.
#[derive(Clone, Debug, StableAbi)]
#[repr(u8)]
enum InnerError {
    Aborted,
    Errors(RVec<RArc<BoxErgoError>>),
}

impl std::fmt::Display for InnerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InnerError::*;
        match self {
            Aborted => write!(f, "aborted"),
            Errors(es) => {
                let mut es = es.iter();
                if let Some(e) = es.next() {
                    write!(f, "{}", e)?;
                }
                while let Some(e) = es.next() {
                    write!(f, "\n{}", e)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for InnerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            InnerError::Errors(es) if es.len() == 1 => Some(es.first().unwrap().as_ref()),
            _ => None,
        }
    }
}

impl Error {
    /// Create a new error.
    pub fn new<E: ErgoError>(e: E) -> Self {
        Self::new_boxed(BoxErgoError::new(e))
    }

    /// Create a new error from a BoxErgoError.
    pub fn new_boxed(v: BoxErgoError) -> Self {
        Error {
            inner: InnerError::Errors(rvec![RArc::new(v)]),
        }
    }

    /// Change the error into a type supporting `std::error::Error`.
    pub fn error(self) -> BoxErgoError {
        BoxErgoError::new(self)
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
            let errs: RVec<_> = errs
                .into_iter()
                .map(|mut e| match &mut e.inner {
                    InnerError::Aborted => rvec![],
                    InnerError::Errors(es) => std::mem::take(es),
                })
                .flatten()
                .collect();
            Error {
                inner: if errs.is_empty() {
                    InnerError::Aborted
                } else {
                    InnerError::Errors(errs)
                },
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
    /// This only traverses ErgoErrors.
    pub fn root_errors(&self) -> Vec<Self> {
        let sources = ErgoError::source(self);
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
        F: Fn(&BoxErgoError) -> Option<bool> + Clone,
    {
        let sources = ErgoError::source(self);
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

impl ErgoError for Error {
    fn ergo_error_id() -> Uuid {
        crate::nsid!(error)
    }

    fn source(&self) -> Vec<&BoxErgoError> {
        match &self.inner {
            InnerError::Aborted => vec![],
            InnerError::Errors(es) => es.iter().map(|e| e.as_ref()).collect(),
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
        match ext.downcast::<BoxErgoError>() {
            Ok(v) => match v.downcast_ref::<Self>() {
                Some(e) => e.clone(),
                None => Error::new_boxed(*v),
            },
            Err(e) => Error::new_boxed(BoxErgoError::from_external_box(e)),
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

/// An error with context.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct ErrorContext {
    err: BoxErgoError,
    context: Context,
}

/// The context of an error.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct Context(DynTrait<'static, RBox<()>, ContextInterface>);

impl std::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl ErrorContext {
    pub fn new<T: std::fmt::Debug + std::fmt::Display + Send + Sync + 'static>(
        err: Error,
        context: T,
    ) -> Self {
        ErrorContext {
            err: BoxErgoError::new(err),
            context: Context(DynTrait::from_any_value(context, ContextInterface)),
        }
    }

    pub fn context(&self) -> &Context {
        &self.context
    }
}

impl std::fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}\n{}", self.err, self.context)
    }
}

impl ErgoError for ErrorContext {
    fn ergo_error_id() -> Uuid {
        crate::nsid!(error::context)
    }

    fn source(&self) -> Vec<&BoxErgoError> {
        vec![&self.err]
    }
}

#[derive(Debug)]
struct HashBySource(Error);

impl HashBySource {
    fn as_ptr(&self) -> *const BoxErgoError {
        match &self.0.inner {
            InnerError::Errors(es) => es.first().unwrap().as_ref() as *const BoxErgoError,
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
                InnerError::Errors(es) if es.len() == 1 => {
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
