//! Runtime source information.

use crate::abi_stable::{bst::BstMap, StableAbi};
use crate::{
    context::SourceId,
    dependency::{AsDependency, Dependency},
    error::DiagnosticInfo,
    Error,
};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::future::Future;

/// A type which adds source location to a value.
#[derive(Clone, Copy, Debug, StableAbi, Deserialize, Serialize)]
#[repr(C)]
pub struct Source<T> {
    value: T,
    pub source_id: SourceId,
    pub location: Location,
}

/// A location in the original (character) input stream.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, StableAbi, Deserialize, Serialize)]
#[repr(C)]
pub struct Location {
    /// The start index of the first character.
    pub start: usize,
    /// The number of characters represented by the Location.
    pub length: usize,
}

/// Types which can be converted into a Source<T>.
pub trait IntoSource {
    /// The Output type, which will be present in Source<Output>.
    type Output;

    /// Convert the type into a source.
    fn into_source(self) -> Source<Self::Output>;
}

impl Location {
    /// Create a Location with the given fields.
    pub fn new(start: usize, length: usize) -> Self {
        Location { start, length }
    }

    /// Get the end index of the location.
    pub fn end(&self) -> usize {
        self.start + self.length
    }

    /// Get a range representing the location.
    pub fn into_range(self) -> std::ops::Range<usize> {
        self.start..self.end()
    }

    /// Check whether this location contains another location.
    pub fn contains(&self, other: &Self) -> bool {
        self.start <= other.start && self.end() >= other.end()
    }
}

impl From<Location> for std::ops::Range<usize> {
    fn from(l: Location) -> Self {
        l.into_range()
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

impl Source<()> {
    /// Create a source from the given source id.
    pub const fn new(source_id: SourceId) -> Self {
        Source {
            value: (),
            location: Location {
                start: 0,
                length: 0,
            },
            source_id,
        }
    }
}

impl<T> Source<T> {
    /// Create a value that has a missing source.
    pub fn missing(v: T) -> Self {
        Source::new(0).with(v)
    }

    /// Get the inner value by reference.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Get the inner value.
    pub fn unwrap(self) -> T {
        self.value
    }

    /// Get a copy of the source.
    pub fn source(&self) -> Source<()> {
        Source {
            value: (),
            source_id: self.source_id.clone(),
            location: self.location.clone(),
        }
    }

    /// Map the inner value of the source.
    pub fn map<U, F>(self, f: F) -> Source<U>
    where
        F: FnOnce(T) -> U,
    {
        Source {
            value: f(self.value),
            source_id: self.source_id,
            location: self.location,
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
            source_id: self.source_id,
            location: self.location,
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
                source_id: self.source_id,
                location: self.location,
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

    /// Check whether a source contains another source.
    pub fn contains<U>(&self, other: &Source<U>) -> bool {
        self.source_id == other.source_id && self.location.contains(&other.location)
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
        this.value == other.value
            && this.location == other.location
            && this.source_id == other.source_id
    }
}

impl<E> Source<E>
where
    E: Into<crate::error::Diagnostic>,
{
    /// Convert a sourced error into a crate::Error.
    pub fn into_error(self) -> Error {
        let (src, e) = self.take();
        Error::new(e.into().add_primary_label(src.with("")))
    }
}

impl<T: Eq> Eq for Source<T> {}

impl<T: PartialEq> PartialEq for Source<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: PartialEq> PartialEq<T> for Source<T> {
    fn eq(&self, other: &T) -> bool {
        &self.value == other
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

impl<T> std::borrow::Borrow<T> for Source<T> {
    fn borrow(&self) -> &T {
        &self.value
    }
}

impl<T> std::borrow::BorrowMut<T> for Source<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.value
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

impl<T: AsDependency> AsDependency for Source<T> {
    fn as_dependency(&self) -> Dependency {
        self.as_ref().unwrap().as_dependency()
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

#[derive(Clone, Copy, PartialEq, Eq)]
struct SingleId(SourceId);

impl std::ops::Add for SingleId {
    type Output = SourceId;

    fn add(self, rhs: Self) -> Self::Output {
        if self != rhs {
            panic!("inconsistent sources");
        }
        self.0
    }
}

impl std::iter::Sum<SingleId> for SourceId {
    fn sum<I: Iterator<Item = SingleId>>(mut iter: I) -> Self {
        if let Some(i) = iter.next() {
            let id = i;
            while let Some(i) = iter.next() {
                if i != id {
                    panic!("inconsistent sources");
                }
            }
            id.0
        } else {
            0
        }
    }
}

fn into_source_iter<R, T, I>(v: T) -> Source<R>
where
    T: IntoIterator<Item = I>,
    I: IntoSource,
    R: Extend<Source<I::Output>> + Default,
{
    let (value, rest): (_, Vec<_>) = v
        .into_iter()
        .map(|t| {
            let s = t.into_source();
            let source = s.source_id;
            let loc = s.location.clone();
            (s, (loc, SingleId(source)))
        })
        .unzip();

    let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
    let location = locs.into_iter().sum();
    let source_id = srcs.into_iter().sum();

    Source {
        value,
        location,
        source_id,
    }
}

impl<T: IntoSource> IntoSource for Vec<T> {
    type Output = Vec<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        into_source_iter(self)
    }
}

impl<T: IntoSource> IntoSource for std::collections::VecDeque<T> {
    type Output = std::collections::VecDeque<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        into_source_iter(self)
    }
}

impl<'a, T> IntoSource for &'a [T]
where
    &'a T: IntoSource,
{
    type Output = Vec<Source<<&'a T as IntoSource>::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        into_source_iter(self)
    }
}

impl<'a, T, U> IntoSource for std::collections::BTreeMap<T, U>
where
    T: IntoSource,
    T::Output: Ord,
    U: IntoSource,
{
    type Output = std::collections::BTreeMap<Source<T::Output>, Source<U::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|(k, v)| {
                let k = k.into_source();
                let v = v.into_source();
                let loc = &k.location + &v.location;
                let source = SingleId(k.source_id) + SingleId(v.source_id);
                ((k, v), (loc, SingleId(source)))
            })
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source_id = srcs.into_iter().sum();

        Source {
            value: value.into_iter().collect(),
            location,
            source_id,
        }
    }
}

impl<'a, T, U> IntoSource for BstMap<T, U>
where
    T: IntoSource,
    T::Output: Ord,
    U: IntoSource,
{
    type Output = BstMap<Source<T::Output>, Source<U::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        let (value, rest): (Vec<_>, Vec<_>) = self
            .into_iter()
            .map(|(k, v)| {
                let k = k.into_source();
                let v = v.into_source();
                let loc = &k.location + &v.location;
                let source = SingleId(k.source_id) + SingleId(v.source_id);
                ((k, v), (loc, SingleId(source)))
            })
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let location = locs.into_iter().sum();
        let source_id = srcs.into_iter().sum();

        Source {
            value: value.into_iter().collect(),
            location,
            source_id,
        }
    }
}

impl<T: IntoSource> IntoSource for Option<T> {
    type Output = Option<Source<T::Output>>;

    fn into_source(self) -> Source<Self::Output> {
        match self {
            None => Source {
                value: None,
                source_id: Default::default(),
                location: Default::default(),
            },
            Some(s) => {
                let s = s.into_source();
                let location = s.location.clone();
                let source_id = s.source_id.clone();
                Source {
                    value: Some(s),
                    source_id,
                    location,
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
            .map(|s| (s.value, (s.location, SingleId(s.source_id))))
            .unzip();
        let (locs, srcs): (Vec<_>, Vec<_>) = rest.into_iter().unzip();
        let value = vals.into_iter().collect();
        let location = locs.into_iter().sum();
        let source_id = srcs.into_iter().sum();

        Source {
            value,
            source_id,
            location,
        }
    }
}

impl<T, U> From<(Source<T>, Source<U>)> for Source<(Source<T>, Source<U>)> {
    fn from((t, u): (Source<T>, Source<U>)) -> Self {
        let location = t.location.clone() + u.location.clone();
        let source_id = SingleId(t.source_id) + SingleId(u.source_id);
        Source {
            value: (t, u),
            location,
            source_id,
        }
    }
}
