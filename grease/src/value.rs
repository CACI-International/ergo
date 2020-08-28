//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use super::type_erase::Erased;
use crate::bst::{BstMap, BstSet};
use crate::future::BoxSharedFuture;
use crate::hash::HashFn;
use crate::types::*;
use crate::u128::U128;
use abi_stable::{
    std_types::{RArc, RResult},
    StableAbi,
};
use futures::future::{FusedFuture, Future, FutureExt, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::BTreeMap;
use std::hash::Hash;
use std::pin::Pin;
use std::sync::Arc;

pub mod dependency;
pub mod error;

use crate::depends;
pub use dependency::*;
pub use error::*;

/// The Result yielded by a Value's future.
pub type Result = std::result::Result<RArc<Erased>, Error>;

/// Value identifiers.
pub type Id = u128;

/// Metadata identifiers.
///
/// This identifier uniquely describes the associated metadata content.
pub type MetadataId = u128;

/// A value shared amongst tasks.
///
/// Values have a type, a future data value, metadata, and an id.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Value {
    tp: RArc<Type>,
    data: BoxSharedFuture<RResult<RArc<Erased>, Error>>,
    metadata: BstMap<U128, RArc<Erased>>,
    id: U128,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Value")
            .field("tp", &self.tp)
            .field("data", &self.data)
            .field(
                "metadata",
                &self.metadata.iter().map(|(k, _)| k).collect::<BstSet<_>>(),
            )
            .field("id", &self.id)
            .finish()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.id == other.id
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Value) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

/// Calculate the value id for a value given the value type and dependencies.
pub fn value_id(tp: &Type, deps: &Dependencies) -> Id {
    let mut h = HashFn::default();
    tp.hash(&mut h);
    deps.hash(&mut h);
    h.finish_ext().into()
}

impl Value {
    /// Create a value with the given type, future, and dependencies.
    pub fn new<F, D>(tp: RArc<Type>, value: F, deps: D) -> Self
    where
        F: Future<Output = std::result::Result<RArc<Erased>, Error>> + Send + 'static,
        D: Into<Dependencies>,
    {
        let deps = deps.into();
        let id = value_id(tp.as_ref(), &deps);
        Self::with_id(tp, value, id)
    }

    /// Create a value with the given type, future, and id.
    ///
    /// This differs from `new` which derives the id from the dependencies. Prefer `new`.
    pub fn with_id<F>(tp: RArc<Type>, value: F, id: u128) -> Self
    where
        F: Future<Output = std::result::Result<RArc<Erased>, Error>> + Send + 'static,
    {
        Value {
            tp,
            data: BoxSharedFuture::new(value.map(RResult::from)),
            metadata: Default::default(),
            id: id.into(),
        }
    }

    /// Create a Value from raw components.
    pub fn from_raw(
        tp: Arc<Type>,
        data: Arc<Erased>,
        metadata: BTreeMap<u128, Arc<Erased>>,
        id: u128,
    ) -> Self {
        Value {
            tp: tp.into(),
            data: BoxSharedFuture::new(futures::future::ok(RArc::from(data)).map(RResult::from)),
            metadata: metadata
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
            id: id.into(),
        }
    }

    /// Set dependencies of this value, producing a new value with a new identifier.
    ///
    /// The value will have the same type and evaluate to the same value as the original.
    pub fn set_dependencies<D>(self, deps: D) -> Value
    where
        D: Into<Dependencies>,
    {
        let deps = deps.into();
        let id = value_id(self.tp.as_ref(), &deps);
        Value {
            id: id.into(),
            ..self
        }
    }

    /// Cause an error to occur if this value's future is ever evaluated.
    pub fn unevaluated(self) -> Value {
        Value {
            data: BoxSharedFuture::new(
                futures::future::err("unevaluated value".into()).map(RResult::from),
            ),
            ..self
        }
    }

    /// Return a value that computes this value, discards the result (on success), and returns the
    /// given value.
    pub fn then(self, v: Value) -> Value {
        let deps = depends![self, v];
        Self::new(v.grease_type(), self.and_then(move |_| v), deps)
    }

    /// Map the error value, returning an identical value with an altered error.
    pub fn map_err<F, E>(self, f: F) -> Value
    where
        F: FnOnce(Error) -> E + Send + 'static,
        E: Into<Error>,
    {
        Value {
            data: BoxSharedFuture::new(self.data.map(|r| r.map_err(|e| f(e).into()))),
            ..self
        }
    }

    /// Try to convert this Value to a TypedValue.
    ///
    /// If the conversion fails, the Err result contains the original Value.
    pub fn typed<T: GreaseType>(self) -> std::result::Result<TypedValue<T>, Value> {
        if *self.grease_type() == T::grease_type() {
            Ok(TypedValue {
                inner: self,
                phantom: Default::default(),
            })
        } else {
            Err(self)
        }
    }

    /// Try to convert this Value by reference to a TypedValue reference.
    ///
    /// If the conversion fails, a unit Err result is returned.
    pub fn typed_ref<'a, T: GreaseType>(
        &'a mut self,
    ) -> std::result::Result<TypedValueRef<'a, T>, ()> {
        if *self.grease_type() == T::grease_type() {
            Ok(TypedValueRef::new(self))
        } else {
            Err(())
        }
    }

    /// Get the value identifier.
    ///
    /// This identifier is deterministically derived from the value type and dependencies, so is
    /// functionally pure and consistent.
    pub fn id(&self) -> u128 {
        *self.id
    }

    /// Get the type of the contained value.
    pub fn grease_type(&self) -> RArc<Type> {
        self.tp.clone()
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> Result {
        futures::executor::block_on(self)
    }

    /// Get the result of the value, if immediately available.
    pub fn peek(&self) -> Option<Result> {
        self.data.peek().map(|v| RResult::into_result(v.clone()))
    }

    /// Get the result of the value, assuming it was forced previously.
    ///
    /// This is a convenience method to unwrap the value. It will panic if the value is not
    /// immediately available.
    pub fn forced_value(&self) -> RArc<Erased> {
        self.peek()
            .expect("value should have been forced")
            .expect("error should have been handled when value was previously forced")
    }
}

impl Future for Value {
    type Output = Result;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.data) }, cx)
            .map(RResult::into_result)
    }
}

impl FusedFuture for Value {
    fn is_terminated(&self) -> bool {
        self.data.is_terminated()
    }
}

/// Match expression for ValueTypes.
///
/// Matching is based on types, not patterns, and each type must implement GreaseType. The else
/// case is required.
///
/// The evaluation contex is similar to regular match expressions, in that flow-control statements
/// will pertain to the calling code.
#[macro_export]
macro_rules! match_value_type {
    ( $value:expr => { $( $t:ty => $e:expr $(,)? )+ => $else:expr } ) => {
        {
            use $crate::types::GreaseType;
            $( if $value == <$t>::grease_type() { $e } else )+ { $else }
        }
    }
}

/// Match expression for Values.
///
/// Matching is based on types, not patterns, and each type must implement GreaseType. The else
/// case is required. All cases must be in the form `|name| body` that evaluate to a final type T,
/// and expressions must all agree on this final type. For each `body`, `name` will be bound to a
/// TypedValue according to the case type.
///
/// The evaluation context is similar to regular match expressions, in that flow-control statements
/// will pertain to the calling code.
#[macro_export]
macro_rules! match_value {
    ( $value:expr => { $( $t:ty => |$bind:pat| $e:expr $(,)? )+ => |$elsebind:pat| $else:expr } ) => {
        loop {
            let mut match_value__val = $value;
            $( match_value__val = match match_value__val.typed::<$t>() {
                Ok($bind) => break $e,
                Err(v) => v
            };)*
            {
                let $elsebind = match_value__val;
                break $else
            }
        }
    }
}

/// A reference to a TypedValue result.
pub type Ref<T> = crate::type_erase::Ref<T, RArc<Erased>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedValue<T> {
    inner: Value,
    phantom: std::marker::PhantomData<Arc<T>>,
}

impl<T: GreaseType + Send + Sync + 'static> TypedValue<T> {
    /// Create a typed value from the given future and dependencies.
    pub fn new<F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = std::result::Result<T, Error>> + Send + 'static,
        D: Into<Dependencies>,
    {
        TypedValue {
            inner: Value::new(
                RArc::new(T::grease_type()),
                value.map_ok(|r| RArc::new(Erased::new(r))),
                deps,
            ),
            phantom: Default::default(),
        }
    }

    /// Create a typed value from the given future (which cannot fail) and dependencies.
    pub fn ok<F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = T> + Send + 'static,
        D: Into<Dependencies>,
    {
        Self::new(value.map(|v| Ok(v)), deps)
    }

    /// Create a constant value.
    pub fn constant(data: T) -> Self
    where
        T: Hash,
    {
        let deps = depends![data];
        Self::constant_deps(data, deps)
    }

    /// Create a constant value with the given dependencies.
    pub fn constant_deps<D>(data: T, deps: D) -> Self
    where
        T: Send + 'static,
        D: Into<Dependencies>,
    {
        Self::ok(futures::future::ready(data), deps)
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> std::result::Result<Ref<T>, Error> {
        futures::executor::block_on(self)
    }

    /// Create a new TypedValue by consuming and mapping the result of this TypedValue.
    pub fn map<U, F>(self, f: F) -> TypedValue<U>
    where
        U: GreaseType + Send + Sync + 'static,
        F: FnOnce(Ref<T>) -> U + Send + 'static,
    {
        self.and_then(move |v| Ok(f(v)))
    }

    /// Create a new TypedValue by consuming and applying the given function (which can fail) over
    /// the result of this TypedValue.
    pub fn and_then<U, F>(self, f: F) -> TypedValue<U>
    where
        U: GreaseType + Send + Sync + 'static,
        F: FnOnce(Ref<T>) -> std::result::Result<U, Error> + Send + 'static,
    {
        let deps = depends![self];
        TypedValue::new(
            FutureExt::map(self, move |result| result.and_then(move |at| f(at))),
            deps,
        )
    }

    /// Get a by-reference typed value.
    pub fn as_ref<'a>(&'a mut self) -> TypedValueRef<'a, T> {
        TypedValueRef::new(&mut self.inner)
    }

    /// Get the result of the value, if immediately available.
    pub fn peek(&self) -> Option<std::result::Result<Ref<T>, Error>> {
        self.inner.peek().map(|v| v.map(|v| unsafe { Ref::new(v) }))
    }

    /// Get the result of the value, assuming it was forced previously.
    ///
    /// This is a convenience method to unwrap the value. It will panic if the value is not
    /// immediately available.
    pub fn forced_value(&self) -> Ref<T> {
        self.peek()
            .expect("value should have been forced")
            .expect("error should have been handled when value was previously forced")
    }
}

impl<T> From<T> for TypedValue<T>
where
    T: GreaseType + Hash + Send + Sync + 'static,
{
    fn from(v: T) -> Self {
        TypedValue::constant(v)
    }
}

impl<T> Clone for TypedValue<T> {
    fn clone(&self) -> Self {
        TypedValue {
            inner: self.inner.clone(),
            phantom: Default::default(),
        }
    }
}

pub struct TypedValueRef<'a, T> {
    inner: &'a mut Value,
    phantom: std::marker::PhantomData<&'a T>,
}

impl<'a, T> TypedValueRef<'a, T> {
    fn new(inner: &'a mut Value) -> Self {
        TypedValueRef {
            inner,
            phantom: Default::default(),
        }
    }
}

impl<'a, T: Sync> TypedValueRef<'a, T> {
    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> std::result::Result<&'a T, Error> {
        futures::executor::block_on(self)
    }
}

/// A convenience trait for converting directly to a value.
pub trait IntoValue {
    fn into_value(self) -> Value;
}

impl<T> IntoValue for T
where
    T: Into<TypedValue<T>>,
{
    fn into_value(self) -> Value {
        self.into().into()
    }
}

impl<T> From<T> for Value
where
    T: IntoValue,
{
    fn from(v: T) -> Value {
        v.into_value()
    }
}

// Value is Send (as it is just pointers to Sync values), so TypedValue should be Send as well.
unsafe impl<T> Send for TypedValue<T> {}

impl<T> Future for TypedValue<T> {
    type Output = std::result::Result<Ref<T>, Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.inner) }, cx)
            .map(|v| v.map(|data| unsafe { Ref::new(data) }))
    }
}

impl<'a, T: 'a + Sync> Future for TypedValueRef<'a, T> {
    type Output = std::result::Result<&'a T, Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| s.inner) }, cx)
            .map(|v| v.map(|v| unsafe { std::mem::transmute::<&T, &'a T>((*v).as_ref::<T>()) }))
    }
}

impl<T> std::ops::Deref for TypedValue<T> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> AsRef<Value> for TypedValue<T> {
    fn as_ref(&self) -> &Value {
        &self.inner
    }
}

impl<T> From<TypedValue<T>> for Value {
    fn from(v: TypedValue<T>) -> Self {
        v.inner
    }
}
