//! Runtime errors.
//!
//! The `Error` type can be created from any type supporting `std::error::Error`, which is
//! convenient with `std::ops::Try`.
//!
//! `Error` also supports aggregate errors, and thus supports `FromIterator<Error>`.

use crate as ergo_runtime;
use crate::abi_stable::{
    rvec,
    std_types::{RArc, RString, RVec},
    StableAbi,
};
use crate::type_system::ErgoType;
use codespan_reporting::{diagnostic as csd, term as cst};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::hash::Hash;

pub use codespan_reporting::term::termcolor;

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
#[derive(Clone, Debug, ErgoType, StableAbi, Serialize, Deserialize, Hash)]
#[repr(C)]
pub struct Error {
    inner: InnerError,
}

/// The severity of a diagnostic.
#[derive(Clone, Copy, Debug, PartialEq, Eq, StableAbi, Serialize, Deserialize, Hash)]
#[repr(u8)]
pub enum Severity {
    Bug,
    Error,
    Warning,
    Note,
    Help,
}

/// A label (with source information) for a diagnostic.
#[derive(Clone, Debug, StableAbi, Serialize, Deserialize, Hash, Eq)]
#[repr(C)]
pub struct Label {
    pub label: crate::Source<RString>,
    pub secondary: bool,
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        crate::Source::total_eq(&self.label, &other.label) && self.secondary == other.secondary
    }
}

impl Label {
    /// Create a label relating to the primary cause of a diagnostic.
    pub fn primary<T: ToString>(label: crate::Source<T>) -> Self {
        Label {
            label: label.map(|s| s.to_string().into()),
            secondary: false,
        }
    }

    /// Create a label relating to secondary causes of a diagnostic.
    pub fn secondary<T: ToString>(label: crate::Source<T>) -> Self {
        Label {
            label: label.map(|s| s.to_string().into()),
            secondary: true,
        }
    }
}

/// A diagnostic to display to the user.
#[derive(Clone, Debug, StableAbi, Serialize, Deserialize, Hash, PartialEq, Eq)]
#[repr(C)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: RString,
    pub labels: RVec<Label>,
    pub notes: RVec<RString>,
}

impl Severity {
    pub fn into_codespan_severity(self) -> csd::Severity {
        match self {
            Severity::Bug => csd::Severity::Bug,
            Severity::Error => csd::Severity::Error,
            Severity::Warning => csd::Severity::Warning,
            Severity::Note => csd::Severity::Note,
            Severity::Help => csd::Severity::Help,
        }
    }
}

impl Label {
    pub fn into_codespan_label(self) -> csd::Label<crate::context::SourceId> {
        let (source, message) = self.label.take();
        csd::Label::new(
            if self.secondary {
                csd::LabelStyle::Secondary
            } else {
                csd::LabelStyle::Primary
            },
            source.source_id,
            source.location,
        )
        .with_message(message)
    }
}

impl Diagnostic {
    pub fn into_codespan_diagnostic(self) -> csd::Diagnostic<crate::context::SourceId> {
        csd::Diagnostic::new(self.severity.into_codespan_severity())
            .with_message(self.message)
            .with_labels(
                self.labels
                    .into_iter()
                    .map(|l| l.into_codespan_label())
                    .collect(),
            )
            .with_notes(self.notes.into_iter().map(|l| l.into()).collect())
    }

    pub fn into_error(self) -> Error {
        self.into()
    }
}

/// A trait for types supporting the addition of diagnostic info.
pub trait DiagnosticInfo {
    /// The output type incorporating diagnostic info.
    type Output;

    /// Convert the type into one supporting diagnostic info.
    fn into_diagnostic(self) -> Self::Output;

    /// Set the severity level of diagnostic info.
    fn set_severity(self, severity: Severity) -> Self::Output;

    /// Set the message in diagnostic info.
    fn set_message<S: ToString>(self, message: S) -> Self::Output;

    /// Add a label to the diagnostic info.
    fn add_label(self, label: Label) -> Self::Output;

    /// Add a note regarding the primary cause to the diagnostic info.
    fn add_note<S: ToString>(self, note: S) -> Self::Output;

    /// Add a label regarding the primary cause to the diagnostic info.
    fn add_primary_label<S: ToString>(self, label: crate::Source<S>) -> Self::Output
    where
        Self: Sized,
    {
        self.add_label(Label::primary(label))
    }

    /// Add a label regarding a secondary cause to the diagnostic info.
    fn add_secondary_label<S: ToString>(self, label: crate::Source<S>) -> Self::Output
    where
        Self: Sized,
    {
        self.add_label(Label::secondary(label))
    }

    fn add_value_sources<R>(self, name: &str, v: &crate::Value) -> R
    where
        Self: Sized + DiagnosticInfo<Output = R>,
        R: DiagnosticInfo<Output = R>,
    {
        use crate::metadata::Source;
        let sources = match v.get_metadata(&Source) {
            None => return self.into_diagnostic(),
            Some(sources) => sources,
        };
        let mut sources = sources.iter().enumerate().peekable();
        let mut me = self.into_diagnostic();
        while let Some((i, source)) = sources.next() {
            if sources.peek().is_none() {
                me = me.add_primary_label(source.clone().with(format_args!("'{}'", name)));
            } else if i == 0 {
                me = me.add_primary_label(
                    source.clone().with(format_args!("'{}' created here", name)),
                );
            } else {
                me = me.add_secondary_label(
                    source
                        .clone()
                        .with(format_args!("'{}' passed here ({})", name, i)),
                );
            }
        }
        me
    }

    fn add_value_content<'a>(
        self,
        name: &'a str,
        v: crate::Value,
    ) -> futures::future::BoxFuture<'a, Self::Output>
    where
        Self: Sized + Send + 'a,
        Self::Output: Send + 'a,
    {
        use futures::future::FutureExt;
        const TO_STRING_LENGTH_LIMIT: usize = 80;
        let tp = crate::traits::type_name(&v);
        async move {
            self.add_note(match crate::traits::to_string(v).await {
                Ok(s) => {
                    if s.len() > TO_STRING_LENGTH_LIMIT {
                        format!(
                            "'{}' is {}: `{}...`",
                            name,
                            tp,
                            &s[..TO_STRING_LENGTH_LIMIT]
                        )
                    } else {
                        format!("'{}' is {}: `{}`", name, tp, s)
                    }
                }
                Err(_) => format!("'{}' is {}", name, tp),
            })
        }
        .boxed()
    }

    fn add_value_info<'a, R>(
        self,
        name: &'a str,
        v: &crate::Value,
    ) -> futures::future::BoxFuture<'a, R>
    where
        Self: Sized + DiagnosticInfo<Output = R>,
        R: DiagnosticInfo<Output = R> + Send + 'a,
    {
        self.add_value_sources(name, v)
            .add_value_content(name, v.clone())
    }
}

impl DiagnosticInfo for Diagnostic {
    type Output = Self;

    fn into_diagnostic(self) -> Self::Output {
        self
    }

    fn set_severity(mut self, severity: Severity) -> Self::Output {
        self.severity = severity;
        self
    }

    fn set_message<S: ToString>(mut self, message: S) -> Self::Output {
        self.message = message.to_string().into();
        self
    }

    fn add_label(mut self, label: Label) -> Self::Output {
        self.labels.push(label);
        self
    }

    fn add_note<S: ToString>(mut self, note: S) -> Self::Output {
        self.notes.push(note.to_string().into());
        self
    }
}

impl DiagnosticInfo for &'_ mut Diagnostic {
    type Output = ();

    fn into_diagnostic(self) -> Self::Output {}

    fn set_severity(self, severity: Severity) -> Self::Output {
        self.severity = severity;
    }

    fn set_message<S: ToString>(self, message: S) -> Self::Output {
        self.message = message.to_string().into();
    }

    fn add_label(self, label: Label) -> Self::Output {
        self.labels.push(label);
    }

    fn add_note<S: ToString>(self, note: S) -> Self::Output {
        self.notes.push(note.to_string().into());
    }
}

impl<T, E: Into<Diagnostic>> DiagnosticInfo for std::result::Result<T, E> {
    type Output = std::result::Result<T, Diagnostic>;

    fn into_diagnostic(self) -> Self::Output {
        self.map_err(|e| e.into())
    }

    fn set_severity(self, severity: Severity) -> Self::Output {
        self.map_err(|e| e.into().set_severity(severity))
    }

    fn set_message<S: ToString>(self, message: S) -> Self::Output {
        self.map_err(|e| e.into().set_message(message))
    }

    fn add_label(self, label: Label) -> Self::Output {
        self.map_err(|e| e.into().add_label(label))
    }

    fn add_note<S: ToString>(self, note: S) -> Self::Output {
        self.map_err(|e| e.into().add_note(note))
    }
}

impl<T> DiagnosticInfo for Option<T> {
    type Output = std::result::Result<T, Diagnostic>;

    fn into_diagnostic(self) -> Self::Output {
        self.ok_or("none".into())
    }

    fn set_severity(self, severity: Severity) -> Self::Output {
        self.into_diagnostic().set_severity(severity)
    }

    fn set_message<S: ToString>(self, message: S) -> Self::Output {
        self.into_diagnostic().set_message(message)
    }

    fn add_label(self, label: Label) -> Self::Output {
        self.into_diagnostic().add_label(label)
    }

    fn add_note<S: ToString>(self, note: S) -> Self::Output {
        self.into_diagnostic().add_note(note)
    }
}

/// Emit diagnostics to the given writer.
pub fn emit_diagnostics<'a, I: IntoIterator<Item = &'a Diagnostic> + 'a>(
    diagnostics: I,
    sources: &crate::context::Sources,
    writer: &mut dyn codespan_reporting::term::termcolor::WriteColor,
) -> std::io::Result<()> {
    let cfg = cst::Config::default();
    for d in diagnostics.into_iter() {
        let cs_diag = d.clone().into_codespan_diagnostic();
        if let Err(e) = cst::emit(writer, &cfg, sources, &cs_diag) {
            use codespan_reporting::files::Error as E;
            match e {
                E::Io(e) => return Err(e),
                other => write!(writer, "{}\n    source location error: {}\n", d, other)?,
            }
        }
    }
    Ok(())
}

/// Write a string with the given diagnostics.
pub fn diagnostics_to_string<'a, I: IntoIterator<Item = &'a Diagnostic> + 'a>(
    diagnostics: I,
    sources: &crate::context::Sources,
    use_ansi_colors: bool,
) -> String {
    let mut s: Vec<u8> = Default::default();
    let result = if use_ansi_colors {
        emit_diagnostics(
            diagnostics,
            sources,
            &mut codespan_reporting::term::termcolor::Ansi::new(&mut s),
        )
    } else {
        emit_diagnostics(
            diagnostics,
            sources,
            &mut codespan_reporting::term::termcolor::NoColor::new(&mut s),
        )
    };
    result
        .and_then(|()| {
            String::from_utf8(s)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
        })
        .unwrap_or_else(|err| format!("error writing diagnostics: {}", err))
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let level = match self.severity {
            Severity::Bug => "bug",
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
            Severity::Help => "help",
        };
        let source = if let Some(l) = self.labels.first() {
            format!(" ({}:{})", l.label.source_id, l.label.location)
        } else {
            Default::default()
        };
        write!(f, "{}: {}{}", level, self.message, source)
    }
}

impl<T> From<T> for Diagnostic
where
    T: Into<Box<ExternalError>>,
{
    fn from(v: T) -> Self {
        Diagnostic {
            severity: Severity::Error,
            message: v.into().to_string().into(),
            labels: Default::default(),
            notes: Default::default(),
        }
    }
}

/// Either an Error or a Diagnostic.
///
/// This is mainly used as a convenient way for the Try operator (`?`) to be used with error return
/// values.
pub enum ErrorOrDiagnostic {
    Error(Error),
    Diagnostic(Diagnostic),
}

impl<T> From<T> for ErrorOrDiagnostic
where
    T: Into<Diagnostic>,
{
    fn from(v: T) -> Self {
        ErrorOrDiagnostic::Diagnostic(v.into())
    }
}

impl From<Error> for ErrorOrDiagnostic {
    fn from(e: Error) -> Self {
        ErrorOrDiagnostic::Error(e)
    }
}

impl ErrorOrDiagnostic {
    pub fn into_error(self) -> Error {
        match self {
            ErrorOrDiagnostic::Error(e) => e,
            ErrorOrDiagnostic::Diagnostic(d) => d.into(),
        }
    }

    fn modify_diagnostic<F: FnOnce(&mut Diagnostic)>(&mut self, f: F) {
        match self {
            ErrorOrDiagnostic::Error(_) => (),
            ErrorOrDiagnostic::Diagnostic(d) => f(d),
        }
    }
}

impl From<ErrorOrDiagnostic> for Error {
    fn from(e: ErrorOrDiagnostic) -> Self {
        e.into_error()
    }
}

impl DiagnosticInfo for ErrorOrDiagnostic {
    type Output = Self;

    fn into_diagnostic(self) -> Self::Output {
        self
    }

    fn set_severity(mut self, severity: Severity) -> Self::Output {
        self.modify_diagnostic(|d| d.set_severity(severity));
        self
    }

    fn set_message<S: ToString>(mut self, message: S) -> Self::Output {
        self.modify_diagnostic(|d| d.set_message(message));
        self
    }

    fn add_label(mut self, label: Label) -> Self::Output {
        self.modify_diagnostic(|d| d.add_label(label));
        self
    }

    fn add_note<S: ToString>(mut self, note: S) -> Self::Output {
        self.modify_diagnostic(|d| d.add_note(note));
        self
    }
}

impl<T> DiagnosticInfo for std::result::Result<T, ErrorOrDiagnostic> {
    type Output = Self;

    fn into_diagnostic(self) -> Self::Output {
        self
    }

    fn set_severity(mut self, severity: Severity) -> Self::Output {
        if let Err(e) = &mut self {
            e.modify_diagnostic(|d| d.set_severity(severity));
        }
        self
    }

    fn set_message<S: ToString>(mut self, message: S) -> Self::Output {
        if let Err(e) = &mut self {
            e.modify_diagnostic(|d| d.set_message(message));
        }
        self
    }

    fn add_label(mut self, label: Label) -> Self::Output {
        if let Err(e) = &mut self {
            e.modify_diagnostic(|d| d.add_label(label));
        }
        self
    }

    fn add_note<S: ToString>(mut self, note: S) -> Self::Output {
        if let Err(e) = &mut self {
            e.modify_diagnostic(|d| d.add_note(note));
        }
        self
    }
}

/// Create a diagnostic.
///
/// ```ignore
/// diagnostic!{
///     severity: Bug,
///     labels: [
///         primary(...),
///         secondary(...)
///     ],
///     notes: [
///         "something interesting"
///     ],
///     EXPRESSION
/// }
/// ```
///
/// `severity`, `labels`, and `notes` are all optional, but must
/// be provided in the above order.
#[macro_export]
macro_rules! diagnostic {
    ( $(severity : $severity:ident ,)? $(labels : [$($labelfn:ident ( $($labelarg:expr),* )),+] ,)?
      $(notes : [$($note:expr),+] ,)? message: $message:expr ) => {
        {
            #[allow(unused_mut)]
            let mut d: $crate::error::Diagnostic = $message.into();
            $(d.severity = $crate::error::Severity::$severity;)?
            $(d.labels = $crate::abi_stable::rvec![$($crate::error::Label::$labelfn ($($labelarg),*)),+];)?
            $(d.notes = $crate::abi_stable::rvec![$($note.to_string().into()),+];)?
            d
        }
    };
}

/// Create an error.
///
/// ```ignore
/// error!{
///     severity: Bug,
///     labels: [
///         primary(...),
///         secondary(...)
///     ],
///     notes: [
///         "something interesting"
///     ],
///     error: EXPRESSION
/// }
/// ```
///
/// `severity`, `labels`, and `notes` are all optional, but must
/// be provided in the above order.
#[macro_export]
macro_rules! error {
    ( $(severity : $severity:ident ,)? $(labels : [$($labelfn:ident ( $($labelarg:expr),* )),+] ,)?
      $(notes : [$($note:expr),+] ,)? error : $error:expr ) => {
        {
            $crate::error::Error::new($crate::diagnostic! {
                $(severity: $severity,)?
                $(labels: [$($labelfn ( $($labelarg),* )),+],)?
                $(notes: [$($note),+] ,)?
                message: $error
            })
        }
    };
}

/// A macro to add diagnostic information to any errors returned by the given (Result) expression.
///
/// ```ignore
/// error_info!{
///     severity: Bug,
///     message: |old_message| ...,
///     labels: [
///         primary(...),
///         secondary(...)
///     ],
///     notes: [
///         "something interesting"
///     ],
///     <async> BLOCK_EXPRESSION
/// }
/// ```
///
/// The expression can return (possibly early with `return` or `?`) an Error, a Diagnostic, or a
/// type implementing `std::error::Error`. If a Diagnostic or `std::error::Error` is returned, the
/// given details are set on the resulting Diagnostic. Note that `severity`, `message`, `labels`,
/// and `notes` are all optional, but must be provided in the above order.
///
/// The macro evaluates to a Result type with the same Ok type as the expression, and an Error Err
/// type.
#[macro_export]
macro_rules! error_info {
    ( $(severity : $severity:ident ,)? $(message : ($old_message:pat) $message:expr ,)?
      $(labels : [$($labelfn:ident ( $($labelarg:expr),* )),+] ,)?
      $(notes : [$($note:expr),+] ,)? { $($es:tt)+ } ) => {
        {
            (|| -> std::result::Result<_,$crate::error::ErrorOrDiagnostic> { { $($es)+ }.map_err(|e| e.into()) })().map_err(|e| match e {
                $crate::error::ErrorOrDiagnostic::Error(e) => e,
                #[allow(unused_mut)]
                $crate::error::ErrorOrDiagnostic::Diagnostic(mut d) => {
                    $(d.severity = $crate::error::Severity::$severity;)?
                    $(d.message = { let $old_message = d.message; $message };)?
                    $(d.labels.extend($crate::abi_stable::rvec![$($crate::error::Label::$labelfn ($($labelarg),*)),+]);)?
                    $(d.notes.extend($crate::abi_stable::rvec![$($note.to_string().into()),+]);)?
                    d.into()
                }
            })
        }
    };
    ( $(severity : $severity:ident ,)? $(message : ($old_message:pat) $message:expr ,)?
      $(labels : [$($labelfn:ident ( $($labelarg:expr),* )),+] ,)?
      $(notes : [$($note:expr),+] ,)? async { $($es:tt)+ } ) => {
        {
            let result: std::result::Result<_, $crate::error::ErrorOrDiagnostic> = async {
                { $($es)+ }.map_err(|e| e.into())
            }.await;
            result.map_err(|e| match e {
                $crate::error::ErrorOrDiagnostic::Error(e) => e,
                #[allow(unused_mut)]
                $crate::error::ErrorOrDiagnostic::Diagnostic(mut d) => {
                    $(d.severity = $crate::error::Severity::$severity;)?
                    $(d.message = { let $old_message = d.message; $message };)?
                    $(d.labels.extend($crate::abi_stable::rvec![$($crate::error::Label::$labelfn ($($labelarg),*)),+]);)?
                    $(d.notes.extend($crate::abi_stable::rvec![$($note.to_string().into()),+]);)?
                    d.into()
                }
            })
        }
    };
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

/// Ergo errors may be an abort or some set of diagnostics.
#[derive(Clone, Debug, StableAbi, Serialize, Deserialize, Hash)]
#[repr(u8)]
enum InnerError {
    Aborted,
    Errors(RVec<RArc<Diagnostic>>),
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

impl std::error::Error for InnerError {}

/// An Error which supports `std::error::Error`.
#[derive(Clone)]
pub struct WrappedError {
    inner: InnerError,
}

impl WrappedError {
    /// Convert back to an Error.
    pub fn unwrap(self) -> Error {
        Error { inner: self.inner }
    }
}

impl std::fmt::Debug for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl std::fmt::Display for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl std::error::Error for WrappedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

impl Error {
    /// Create a new error with the given diagnostic.
    pub fn new(d: Diagnostic) -> Self {
        Error {
            inner: InnerError::Errors(rvec![RArc::new(d)]),
        }
    }

    /// Create an aborted error.
    ///
    /// Aborted errors will be dropped/removed when aggregating errors.
    pub fn aborted() -> Self {
        Error {
            inner: InnerError::Aborted,
        }
    }

    /// Get a value with implements `std::error::Error`.
    pub fn error(self) -> WrappedError {
        WrappedError { inner: self.inner }
    }

    /// Get a reference to a `std::error::Error`.
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

    /// Whether this error is an aborted error.
    pub fn is_aborted(&self) -> bool {
        if let InnerError::Aborted = &self.inner {
            true
        } else {
            false
        }
    }

    /// Call a function on all diagnostics that can be modified.
    pub fn modify_diagnostics<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Diagnostic),
    {
        match &mut self.inner {
            InnerError::Errors(errs) => {
                for e in errs {
                    if let Some(d) = RArc::get_mut(e) {
                        f(d);
                    }
                }
            }
            _ => (),
        }
    }

    /// Call a function on all diagnostics in this Error.
    pub fn visit_diagnostics<F>(&self, mut f: F)
    where
        F: FnMut(&Diagnostic),
    {
        match &self.inner {
            InnerError::Errors(errs) => {
                for e in errs {
                    f(e);
                }
            }
            _ => (),
        }
    }
}

impl AsRef<ExternalError> for Error {
    fn as_ref(&self) -> &ExternalError {
        &self.inner
    }
}

impl From<Diagnostic> for Error {
    fn from(d: Diagnostic) -> Self {
        Error {
            inner: InnerError::Errors(rvec![RArc::new(d)]),
        }
    }
}

impl From<&'_ Self> for Error {
    fn from(v: &Self) -> Self {
        v.clone()
    }
}

impl From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        std::io::Error::new(std::io::ErrorKind::Other, e.error())
    }
}

impl std::iter::FromIterator<Error> for Error {
    fn from_iter<I: IntoIterator<Item = Error>>(iter: I) -> Self {
        Self::aggregate(iter)
    }
}

pub mod diagnostics {
    use super::*;

    /// A set of unique diagnostics.
    #[derive(Clone, Default, Debug)]
    pub struct Diagnostics {
        seen: HashSet<RArc<Diagnostic>>,
        ordered: RVec<RArc<Diagnostic>>,
    }

    impl Diagnostics {
        /// Create an empty set.
        pub fn new() -> Self {
            Self::default()
        }

        /// Insert all diagnostics from the given error into the set.
        ///
        /// Returns whether any new diagnostics were inserted.
        pub fn insert(&mut self, err: &Error) -> bool {
            self.insert_new(err, |_| ())
        }

        /// Insert all diagnostics from the given error into the set.
        ///
        /// Calls the `new` function on each diagnostic that hasn't yet been seen.
        ///
        /// Returns whether any new diagnostics were inserted.
        pub fn insert_new<F>(&mut self, err: &Error, mut new: F) -> bool
        where
            F: FnMut(&Diagnostic),
        {
            if err.is_aborted() {
                return false;
            }
            let mut added = false;
            match &err.inner {
                InnerError::Errors(diagnostics) => {
                    for d in diagnostics {
                        let is_new = self.seen.insert(d.clone());
                        if is_new {
                            self.ordered.push(d.clone());
                            new(d.as_ref());
                        }
                        added |= is_new;
                    }
                }
                _ => (),
            }
            added
        }

        /// Extend this set with the contents of another set.
        pub fn extend(&mut self, other: Self) {
            for d in other.ordered {
                if self.seen.insert(d.clone()) {
                    self.ordered.push(d);
                }
            }
        }

        /// Return the number of unique diagnostics in the set.
        pub fn len(&self) -> usize {
            self.seen.len()
        }

        /// Iterate over the diagnostics in the set.
        pub fn iter(&self) -> Iter {
            Iter(self.ordered.iter())
        }
    }

    /// Diagnostics reference iterator.
    pub struct Iter<'a>(<&'a RVec<RArc<Diagnostic>> as IntoIterator>::IntoIter);

    impl<'a> Iterator for Iter<'a> {
        type Item = &'a Diagnostic;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next().map(|h| h.as_ref())
        }
    }

    impl<'a> IntoIterator for &'a Diagnostics {
        type Item = &'a Diagnostic;
        type IntoIter = Iter<'a>;

        fn into_iter(self) -> Self::IntoIter {
            self.iter()
        }
    }

    impl<'a> From<&'a super::Error> for Diagnostics {
        fn from(e: &'a super::Error) -> Self {
            let mut ret = Self::new();
            ret.insert(e);
            ret
        }
    }
}

pub use diagnostics::Diagnostics;
