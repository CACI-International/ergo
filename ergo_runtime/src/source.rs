//! Runtime source information.

use abi_stable::{
    erased_types::DynTrait,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RResult, RString},
    StableAbi,
};
use grease::value::Dependency;
use std::fmt;
use std::future::Future;
use std::io::{BufRead, BufReader, Read};

use ROption::*;

/// A type which adds source location to a value.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Source<T> {
    value: T,
    pub location: Location,
    source: SourceFactoryRef,
}

/// A location in the original (character) input stream.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, StableAbi)]
#[repr(C)]
pub struct Location {
    /// The start index of the first character.
    pub start: usize,
    /// The number of characters represented by the Location.
    pub length: usize,
}

#[derive(StableAbi)]
#[sabi(impl_InterfaceType(IoRead))]
#[repr(C)]
pub struct ReadInterface;

/// A factory that provides source names and data.
pub trait SourceFactory {
    /// The name of the source.
    fn name(&self) -> String;

    /// Read from the source.
    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String>;
}

#[sabi_trait]
trait SourceFactoryAbi: Send + Sync {
    fn name(&self) -> RString;

    #[sabi(last_prefix_field)]
    fn read<'a>(&'a self) -> RResult<DynTrait<'a, RBox<()>, ReadInterface>, RString>;
}

impl<T: SourceFactory + Sync + Send> SourceFactoryAbi for T {
    fn name(&self) -> RString {
        SourceFactory::name(self).into()
    }

    fn read<'a>(&'a self) -> RResult<DynTrait<'a, RBox<()>, ReadInterface>, RString> {
        SourceFactory::read(self)
            .map(|v| DynTrait::from_borrowing_value(v, ReadInterface))
            .map_err(|e| e.into())
            .into()
    }
}

/// Types which can be converted into a Source<T>.
pub trait IntoSource {
    /// The Output type, which will be present in Source<Output>.
    type Output;

    /// Convert the type into a source.
    fn into_source(self) -> Source<Self::Output>;
}

/// A string-based source.
pub struct StringSource {
    name: String,
    src: String,
}

/// A file-based source.
pub struct FileSource(pub std::path::PathBuf);

/// No source.
pub struct NoSource;

/// A reference to a SourceFactory.
#[derive(Clone, Default, StableAbi)]
#[repr(C)]
struct SourceFactoryRef(ROption<RArc<SourceFactoryAbi_TO<'static, RBox<()>>>>);

impl Location {
    /// Create a Location with the given fields.
    pub fn new(start: usize, length: usize) -> Self {
        Location { start, length }
    }

    /// Get the end index of the location.
    pub fn end(&self) -> usize {
        self.start + self.length
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.start + self.length - 1)
    }
}

impl std::ops::Add for Location {
    type Output = Location;

    fn add(self, other: Location) -> Self::Output {
        &self + &other
    }
}

impl std::ops::Add for &'_ Location {
    type Output = Location;

    fn add(self, other: &Location) -> Self::Output {
        if self.length == 0 {
            other.clone()
        } else if other.length == 0 {
            self.clone()
        } else {
            let start = std::cmp::min(self.start, other.start);
            let end = std::cmp::max(self.end(), other.end());
            Location {
                start,
                length: end - start,
            }
        }
    }
}

impl std::iter::Sum for Location {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        iter.next()
            .map(|first| iter.fold(first, |a, b| a + b))
            .unwrap_or_default()
    }
}

impl fmt::Debug for SourceFactoryRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            RNone => write!(f, "no SourceFactory"),
            RSome(s) => write!(f, "SourceFactory({})", s.name()),
        }
    }
}

impl std::ops::Add for SourceFactoryRef {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (&self.0, &other.0) {
            (RSome(ref a), RSome(ref b)) => {
                if a.as_ref() as *const _ == b.as_ref() as *const _ {
                    self
                } else {
                    SourceFactoryRef(RNone)
                }
            }
            (RSome(_), _) => self,
            (_, RSome(_)) => other,
            _ => SourceFactoryRef(RNone),
        }
    }
}

impl std::iter::Sum for SourceFactoryRef {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        iter.next()
            .map(|first| iter.fold(first, |a, b| a + b))
            .unwrap_or_default()
    }
}

impl PartialEq for SourceFactoryRef {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (RSome(ref a), RSome(ref b)) => a.as_ref() as *const _ == b.as_ref() as *const _,
            (RNone, RNone) => true,
            _ => false,
        }
    }
}

impl std::hash::Hash for SourceFactoryRef {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        match &self.0 {
            RNone => (233498237u128).hash(h),
            RSome(v) => std::ptr::hash(v.as_ref(), h),
        }
    }
}

impl StringSource {
    pub fn new<N: Into<String>>(name: N, src: String) -> Self {
        StringSource {
            name: name.into(),
            src,
        }
    }
}

impl SourceFactory for StringSource {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String> {
        let r: &'a [u8] = self.src.as_ref();
        Ok(Box::new(r))
    }
}

impl SourceFactory for FileSource {
    fn name(&self) -> String {
        format!("{}", self.0.display())
    }

    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String> {
        Ok(Box::new(
            std::fs::File::open(self.0.clone()).map_err(|e| e.to_string())?,
        ))
    }
}

impl SourceFactory for NoSource {
    fn name(&self) -> String {
        "none".into()
    }

    fn read<'a>(&'a self) -> Result<Box<dyn Read + 'a>, String> {
        Err("no source".into())
    }
}

impl Source<()> {
    /// Create a source with the given factory.
    pub fn new(source: impl SourceFactory + Send + Sync + 'static) -> Self {
        Source {
            value: (),
            location: Location::default(),
            source: SourceFactoryRef(RSome(RArc::new(SourceFactoryAbi_TO::from_value(
                source, TU_Opaque,
            )))),
        }
    }

    /// Open a source, returning a Source around the iterator over the source's characters.
    ///
    /// This should only be called when it is known that the source has a source factory.
    pub fn open(self) -> std::io::Result<Source<impl IntoIterator<Item = char>>> {
        let src = self.source.0.clone().expect("no source factory");
        let mut r = src
            .read()
            .into_result()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.into_string()))?;
        // TODO: inefficient
        let mut s = String::new();
        r.read_to_string(&mut s)?;
        let s: Vec<_> = s.chars().collect();

        Ok(Source {
            value: s,
            location: self.location,
            source: self.source,
        })
    }
}

impl<T> Source<T> {
    /// Create a value that has internal source.
    pub fn builtin(v: T) -> Self {
        Source::new(NoSource).map(|_| v)
    }

    /// Get the inner value.
    pub fn unwrap(self) -> T {
        self.value
    }

    /// Get a copy of the source.
    pub fn source(&self) -> Source<()> {
        Source {
            value: (),
            location: self.location.clone(),
            source: self.source.clone(),
        }
    }

    /// Map the inner value of the source.
    pub fn map<U, F>(self, f: F) -> Source<U>
    where
        F: FnOnce(T) -> U,
    {
        Source {
            value: f(self.value),
            location: self.location,
            source: self.source,
        }
    }

    /// Map the inner value of the source with an async function.
    pub async fn map_async<U, F, Fut>(self, f: F) -> Source<U>
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = U>,
    {
        Source {
            value: f(self.value).await,
            location: self.location,
            source: self.source,
        }
    }

    /// Replace the contents of the source.
    pub fn with<U>(self, u: U) -> Source<U> {
        self.map(|_| u)
    }

    /// Remove a value from a source.
    pub fn take(self) -> (Source<()>, T) {
        (
            Source {
                value: (),
                location: self.location,
                source: self.source,
            },
            self.value,
        )
    }

    /// Convert a &Source<T> to a Source<&T>.
    pub fn as_ref(&self) -> Source<&T> {
        let r: &T = AsRef::<T>::as_ref(self);
        self.source().map(move |()| r)
    }

    /// Convert a &mut Source<T> to a Source<&mut T>.
    pub fn as_mut(&mut self) -> Source<&mut T> {
        let src = self.source();
        let r: &mut T = AsMut::<T>::as_mut(self);
        src.map(move |()| r)
    }

    /// Convert a Source<T> to a Source<U>.
    pub fn value_into<U: From<T>>(self) -> Source<U> {
        self.map(|v| v.into())
    }
}

impl<T: ToString> Source<T> {
    /// Add extra context to an error.
    pub fn context_for_error(&self, e: grease::value::Error) -> grease::value::Error {
        e.with_context(self.source().with(self.value.to_string()))
    }

    /// Add extra context to the error returned by the given value.
    ///
    /// The returned value will have this extra information in the error output (if an error occurs
    /// at all).
    pub fn imbue_error_context(&self, v: grease::value::Value) -> grease::value::Value {
        let src = self.source().with(self.value.to_string());
        v.map_err(move |e| e.with_context(src))
    }
}

impl<T, E> Source<Result<T, E>> {
    /// Move the source into the Ok/Err result.
    pub fn transpose(self) -> Result<Source<T>, Source<E>> {
        let (source, v) = self.take();
        match v {
            Ok(t) => Ok(source.with(t)),
            Err(e) => Err(source.with(e)),
        }
    }

    /// Move the source into the Ok result.
    pub fn transpose_ok(self) -> Result<Source<T>, E> {
        let (source, v) = self.take();
        v.map(move |t| source.with(t))
    }

    /// Move the source into the Err result.
    pub fn transpose_err(self) -> Result<T, Source<E>> {
        let (source, v) = self.take();
        v.map_err(move |e| source.with(e))
    }
}

impl<T> Source<Option<T>> {
    /// Move the source into the Some value.
    pub fn transpose(self) -> Option<Source<T>> {
        let (source, v) = self.take();
        match v {
            Some(v) => Some(source.with(v)),
            None => None,
        }
    }
}

impl<T: PartialEq> Source<T> {
    pub fn total_eq(this: &Self, other: &Self) -> bool {
        this.value == other.value && this.location == other.location && this.source == other.source
    }
}

impl<E> Source<E>
where
    E: Into<grease::value::Error>,
{
    /// Convert a sourced error into a grease::value::Error.
    pub fn into_grease_error(self) -> grease::value::Error {
        self.map(|v| v.into().error()).into()
    }
}

impl<T: Eq> Eq for Source<T> {}

impl<T: PartialEq> PartialEq for Source<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Source<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<T: PartialOrd> PartialOrd for Source<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T: Ord> Ord for Source<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T> std::ops::Deref for Source<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Source<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> AsRef<T> for Source<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Source<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> std::error::Error for Source<T>
where
    T: std::error::Error + 'static,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.value)
    }
}

impl<T: Into<Dependency>> From<Source<T>> for Dependency {
    fn from(v: Source<T>) -> Dependency {
        v.unwrap().into()
    }
}

impl<T: Future> Future for Source<T> {
    type Output = Source<T::Output>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        let source = self.source();
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.value) }, cx)
            .map(|v| source.with(v))
    }
}

impl<T> IntoSource for Source<T> {
    type Output = T;

    fn into_source(self) -> Source<T> {
        self
    }
}

impl<'a, T> IntoSource for &'a Source<T> {
    type Output = &'a T;

    fn into_source(self) -> Source<Self::Output> {
        self.as_ref()
    }
}

impl<T: IntoSource, U: IntoSource> IntoSource for (T, U) {
    type Output = (Source<T::Output>, Source<U::Output>);

    fn into_source(self) -> Source<Self::Output> {
        (self.0.into_source(), self.1.into_source()).into()
    }
}

impl<T: IntoSource> IntoSource for Vec<T> {
    type Output = Vec<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|t| {
                let s = t.into_source();
                let source = s.source.0.clone();
                let loc = s.location.clone();
                (s, (loc, SourceFactoryRef(source)))
            })
            .unzip();

        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value,
            location,
            source,
        }
    }
}

impl<'a, T> IntoSource for &'a [T]
where
    &'a T: IntoSource,
{
    type Output = Vec<Source<<&'a T as IntoSource>::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|t| {
                let s = t.into_source();
                let source = s.source.0.clone();
                let loc = s.location.clone();
                (s, (loc, SourceFactoryRef(source)))
            })
            .unzip();

        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value,
            location,
            source,
        }
    }
}

impl<'a, T, U> IntoSource for std::collections::BTreeMap<T, U>
where
    T: Ord,
    U: IntoSource,
{
    type Output = std::collections::BTreeMap<T, Source<U::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|(k, v)| {
                let v = v.into_source();
                let source = v.source.0.clone();
                let loc = v.location.clone();
                ((k, v), (loc, SourceFactoryRef(source)))
            })
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value: value.into_iter().collect(),
            location,
            source,
        }
    }
}

impl<'a, T, U> IntoSource for grease::bst::BstMap<T, U>
where
    T: Ord,
    U: IntoSource,
{
    type Output = grease::bst::BstMap<T, Source<U::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|(k, v)| {
                let v = v.into_source();
                let source = v.source.0.clone();
                let loc = v.location.clone();
                ((k, v), (loc, SourceFactoryRef(source)))
            })
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value: value.into_iter().collect(),
            location,
            source,
        }
    }
}

impl<T: IntoSource> IntoSource for Option<T> {
    type Output = Option<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        match self {
            None => Source {
                value: None,
                location: Default::default(),
                source: Default::default(),
            },
            Some(s) => {
                let s = s.into_source();
                let location = s.location.clone();
                let source = s.source.clone();
                Source {
                    value: Some(s),
                    location,
                    source,
                }
            }
        }
    }
}

impl<T, U> std::iter::FromIterator<Source<U>> for Source<T>
where
    T: std::iter::FromIterator<U>,
{
    fn from_iter<I: IntoIterator<Item = Source<U>>>(iter: I) -> Source<T> {
        let (vals, rest): (Vec<_>, Vec<_>) = iter
            .into_iter()
            .map(|s| (s.value, (s.location, s.source)))
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let value = vals.into_iter().collect();
        let location = locs.into_iter().sum();
        let source = srcs.into_iter().sum();

        Source {
            value,
            location,
            source,
        }
    }
}

impl<T, U> From<(Source<T>, Source<U>)> for Source<(Source<T>, Source<U>)> {
    fn from((t, u): (Source<T>, Source<U>)) -> Self {
        let location = t.location.clone() + u.location.clone();
        let source = t.source.clone() + u.source.clone();
        Source {
            value: (t, u),
            location,
            source,
        }
    }
}

const TAB_WIDTH: usize = 4;

impl<T: fmt::Display> fmt::Display for Source<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut start = None;
        let mut end = None;
        let mut startline = None;

        if let RSome(source) = &self.source.0 {
            write!(f, "{}", source.name())?;
            match source.read() {
                RResult::ROk(reader) => {
                    let mut src = BufReader::new(reader);

                    let mut remaining = self.location.start;
                    let mut linecount = 1;
                    let error = loop {
                        let mut line = String::new();
                        let read = match src.read_line(&mut line) {
                            Ok(s) => s,
                            Err(e) => break Some(e.to_string()),
                        };
                        if read == 0 {
                            break Some("invalid location".to_string());
                        }

                        let chars = line.chars().count();
                        if remaining < chars {
                            if start.is_none() {
                                start = Some((linecount, remaining + 1));
                                startline = Some(line.trim_end().to_owned());
                                remaining += self.location.length - 1;
                                if remaining <= chars {
                                    end = Some((linecount, remaining + 1));
                                    break None;
                                }
                            } else {
                                end = Some((linecount, remaining + 1));
                                break None;
                            }
                        }
                        remaining -= chars;
                        linecount += 1;
                    };

                    match error {
                        Some(e) => write!(f, ": {}\n[error: {}]", &self.value, e),
                        None => {
                            let start = start.unwrap();
                            let end = end.unwrap();
                            let startline = startline.unwrap();
                            let mut linechars = startline.chars();
                            let mut underline = String::new();
                            let mut put_underline = |c, alt_c| {
                                if let Some('\t') = linechars.next() {
                                    underline.push(c);
                                    for _ in 1..TAB_WIDTH {
                                        underline.push(alt_c);
                                    }
                                } else {
                                    underline.push(c);
                                }
                            };
                            for _ in 1..start.1 {
                                put_underline(' ', ' ');
                            }
                            put_underline('^', '-');
                            let endchar = if start.0 == end.0 {
                                end.1
                            } else {
                                startline.chars().count()
                            };
                            for _ in start.1..endchar {
                                put_underline('-', '-');
                            }

                            let mut display_startline = String::new();
                            for c in startline.chars() {
                                if c == '\t' {
                                    display_startline.extend(" ".repeat(TAB_WIDTH).chars());
                                } else {
                                    display_startline.push(c);
                                }
                            }

                            write!(
                                f,
                                " ({}:{}-{}:{}): {}\n{}\n{}",
                                start.0,
                                start.1,
                                end.0,
                                end.1,
                                &self.value,
                                display_startline,
                                underline
                            )
                        }
                    }
                }
                RResult::RErr(e) => write!(f, ": {}\n[error reading source: {}]", &self.value, e),
            }
        } else {
            write!(f, "[no source]: {}", &self.value)
        }
    }
}

impl<T: PartialEq> PartialEq<T> for Source<T> {
    fn eq(&self, other: &T) -> bool {
        &self.value == other
    }
}
