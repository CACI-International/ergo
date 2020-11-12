//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use super::type_erase::Erased;
use crate::bst::{BstMap, BstSet};
use crate::future::{eager::Eager, BoxSharedFuture};
use crate::hash::HashFn;
use crate::types::*;
use crate::u128::U128;
use abi_stable::{
    std_types::{RArc, RResult},
    StableAbi,
};
use futures::future::{Future, FutureExt, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::BTreeMap;
use std::hash::Hash;
use std::pin::Pin;
use std::sync::Arc;

pub mod dependency;

use crate::depends;
use crate::error::*;
pub use dependency::*;

/// The Result yielded by a Value's future.
pub type Result = crate::Result<RArc<Erased>>;

/// Value identifiers.
pub type Id = u128;

/// Metadata identifiers.
///
/// This identifier uniquely describes the associated metadata content.
pub type MetadataId = u128;

/// Value result used for dynamically-typed Values.
#[derive(Debug, Clone, StableAbi)]
#[repr(C)]
pub struct AnyValue {
    inner: Value,
}

impl AnyValue {
    /// Create a new AnyValue.
    ///
    /// # Safety
    /// Callers must ensure the type corresponds to the data.
    fn new(inner: Value) -> Self {
        AnyValue { inner }
    }
}

impl GreaseType for AnyValue {
    fn grease_type() -> Type {
        Type::named(b"grease::value::AnyValue")
    }
}

/// The (type,data) pair within a Value.
#[derive(Debug, Clone, StableAbi)]
#[repr(C)]
enum ValueData {
    Typed {
        tp: RArc<Type>,
        fut: BoxSharedFuture<RResult<RArc<Erased>, Error>>,
    },
    Dynamic {
        fut: BoxSharedFuture<RResult<AnyValue, Error>>,
    },
    None,
}

impl ValueData {
    pub fn map_data<F, Fut>(self, f: F) -> Self
    where
        F: FnOnce(
                Eager<futures::future::BoxFuture<'static, crate::Result<InnerData>>>,
            ) -> Eager<Fut>
            + Send
            + 'static,
        Fut: Future<Output = crate::Result<InnerData>> + Send + 'static,
    {
        match self {
            ValueData::Typed { tp, fut } => ValueData::Typed {
                tp,
                fut: {
                    let rfut = fut
                        .into_eager()
                        .map(|r| r.into_result().map(|d| InnerData(InnerDataInner::Typed(d))));
                    BoxSharedFuture::from_eager(
                        f(rfut.boxed())
                            .map_ok(|InnerData(d)| match d {
                                InnerDataInner::Typed(d) => d,
                                _ => panic!("value inner data logic error"),
                            })
                            .map(|v| v.into()),
                    )
                },
            },
            ValueData::Dynamic { fut } => ValueData::Dynamic {
                fut: {
                    let rfut = fut.into_eager().map(|r| {
                        r.into_result()
                            .map(|d| InnerData(InnerDataInner::Dynamic(d)))
                    });
                    BoxSharedFuture::from_eager(
                        f(rfut.boxed())
                            .map_ok(|InnerData(d)| match d {
                                InnerDataInner::Dynamic(d) => d,
                                _ => panic!("value inner data logic error"),
                            })
                            .map(|v| v.into()),
                    )
                },
            },
            ValueData::None => ValueData::None,
        }
    }

    pub fn map_err<F, E>(self, f: F) -> ValueData
    where
        F: FnOnce(Error) -> E + Send + 'static,
        E: Into<Error>,
    {
        match self {
            ValueData::Typed { tp, fut } => ValueData::Typed {
                tp,
                fut: BoxSharedFuture::from_eager(
                    fut.into_eager().map(|r| r.map_err(|e| f(e).into())),
                ),
            },
            ValueData::Dynamic { fut } => ValueData::Dynamic {
                fut: BoxSharedFuture::from_eager(
                    fut.into_eager().map(|r| r.map_err(|e| f(e).into())),
                ),
            },
            ValueData::None => ValueData::None,
        }
    }
}

enum InnerDataInner {
    Typed(RArc<Erased>),
    Dynamic(AnyValue),
}

pub struct InnerData(InnerDataInner);

macro_rules! match_value_data {
    ( $e:expr => { $( $p:pat => $r:expr $(,)? )+ } ) => {
        match $e {
            $( $p => $r, )+
            ValueData::None => panic!("attempted to use empty value")
        }
    }
}

/// A value shared amongst tasks.
///
/// Values have a type, a future data value, metadata, and an id.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Value {
    id: U128,
    metadata: BstMap<U128, RArc<Erased>>,
    data: ValueData,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Value")
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
pub fn value_id(tp: Option<&Type>, deps: &Dependencies) -> Id {
    let mut h = HashFn::default();
    match tp {
        None => AnyValue::grease_type().hash(&mut h),
        Some(tp) => tp.hash(&mut h),
    }
    deps.hash(&mut h);
    h.finish_ext().into()
}

impl Value {
    /// Create a value with the given type, future, and dependencies.
    ///
    /// # Safety
    /// Callers must ensure the type corresponds to the future's data.
    pub unsafe fn new<F, D>(tp: RArc<Type>, value: F, deps: D) -> Self
    where
        F: Future<Output = std::result::Result<RArc<Erased>, Error>> + Send + 'static,
        D: Into<Dependencies>,
    {
        let deps = deps.into();
        let id = value_id(Some(tp.as_ref()), &deps);
        Self::with_id(tp, value, id)
    }

    /// Create a value with the given type, future, and id.
    ///
    /// This differs from `new` which derives the id from the dependencies. Prefer `new`.
    ///
    /// # Safety
    /// Callers must ensure the type corresponds to the future's data.
    pub unsafe fn with_id<F>(tp: RArc<Type>, value: F, id: u128) -> Self
    where
        F: Future<Output = std::result::Result<RArc<Erased>, Error>> + Send + 'static,
    {
        Value {
            id: id.into(),
            metadata: Default::default(),
            data: ValueData::Typed {
                tp,
                fut: BoxSharedFuture::new(value.map(RResult::from)),
            },
        }
    }

    /// Create an immediately-available Value from raw components.
    ///
    /// # Safety
    /// Callers must ensure the type corresponds to the data.
    pub unsafe fn ready(
        tp: Arc<Type>,
        data: Arc<Erased>,
        metadata: BTreeMap<u128, Arc<Erased>>,
        id: u128,
    ) -> Self {
        Value {
            id: id.into(),
            metadata: metadata
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
            data: ValueData::Typed {
                tp: tp.into(),
                fut: BoxSharedFuture::ready(RResult::ROk(RArc::from(data))),
            },
        }
    }

    /// Create a dynamic-typed value with the future and dependencies.
    pub fn dyn_new<F, D>(type_and_value: F, deps: D) -> Self
    where
        F: Future<Output = std::result::Result<AnyValue, Error>> + Send + 'static,
        D: Into<Dependencies>,
    {
        let deps = deps.into();
        let id = value_id(None, &deps);
        Self::dyn_with_id(type_and_value, id)
    }

    /// Create a dynamic-typed value with the given future and id.
    ///
    /// This differs from `dyn_new` which derives the id from the dependencies. Prefer `dyn_new`.
    pub fn dyn_with_id<F>(type_and_value: F, id: u128) -> Self
    where
        F: Future<Output = std::result::Result<AnyValue, Error>> + Send + 'static,
    {
        Value {
            id: id.into(),
            metadata: Default::default(),
            data: ValueData::Dynamic {
                fut: BoxSharedFuture::new(type_and_value.map(RResult::from)),
            },
        }
    }

    /// If this value is dynamically typed, force the inner dynamic value. Otherwise return this
    /// value (as Err).
    pub async fn dyn_value(self) -> crate::Result<std::result::Result<Value, Value>> {
        match_value_data!(self.data => {
            ValueData::Dynamic { fut } => Ok(Ok(fut.await.into_result()?.inner)),
            ValueData::Typed {..} => Ok(Err(self))
        })
    }

    /// Create an AnyValue from this value.
    pub fn into_any_value(self) -> AnyValue {
        AnyValue::new(self)
    }

    /// Set dependencies of this value, producing a new value with a new identifier.
    ///
    /// The value will have the same type and evaluate to the same value as the original.
    pub fn set_dependencies<D>(self, deps: D) -> Value
    where
        D: Into<Dependencies>,
    {
        let id = self.value_id(deps);
        Value {
            id: id.into(),
            ..self
        }
    }

    /// Cause an error to occur if this value's future is ever evaluated.
    pub fn unevaluated(self) -> Value {
        Value {
            data: ValueData::None,
            ..self
        }
    }

    fn value_id<D>(&self, deps: D) -> Id
    where
        D: Into<Dependencies>,
    {
        value_id(
            match_value_data! { &self.data => {
                ValueData::Typed { tp, .. } => Some(tp.as_ref()),
                ValueData::Dynamic { .. } => None,
            } },
            &deps.into(),
        )
    }

    /// Return a value that computes this value, discards the result (on success), and returns the
    /// given value.
    pub fn then(self, v: Value) -> Value {
        let deps = depends![self, v];
        let id = v.value_id(deps);
        Value {
            id: id.into(),
            metadata: v.metadata,
            data: match_value_data!(v.data => {
                ValueData::Typed { tp, fut } => ValueData::Typed {
                    tp,
                    fut: BoxSharedFuture::from_eager(self.into_eager().and_then_eager(move |_|
                                fut.into_eager().map(RResult::into_result).into_future()
                            ).map(RResult::from))
                }
                ValueData::Dynamic { fut } => ValueData::Dynamic {
                    fut: BoxSharedFuture::from_eager(self.into_eager().and_then_eager(move |_|
                                 fut.into_eager().map(RResult::into_result).into_future()
                            ).map(RResult::from))
                }
            }),
        }
    }

    /// Map the data of this value, retaining the value type and identity.
    ///
    /// This is useful to inserting side-effects into the value execution.
    pub fn map_data<F, Fut>(self, f: F) -> Self
    where
        F: FnOnce(
                Eager<futures::future::BoxFuture<'static, crate::Result<InnerData>>>,
            ) -> Eager<Fut>
            + Send
            + 'static,
        Fut: Future<Output = crate::Result<InnerData>> + Send + 'static,
    {
        Value {
            data: self.data.map_data(f),
            ..self
        }
    }

    /// Map the error value, returning an identical value with an altered error.
    pub fn map_err<F, E>(self, f: F) -> Value
    where
        F: FnOnce(Error) -> E + Send + 'static,
        E: Into<Error>,
    {
        Value {
            data: self.data.map_err(f),
            ..self
        }
    }

    /// Try to convert this Value to a TypedValue.
    ///
    /// If the conversion fails, the given function is used to get an Error from the type.
    pub fn typed<T: GreaseType, F, Fut>(
        self,
        on_error: F,
    ) -> futures::future::BoxFuture<'static, crate::Result<TypedValue<T>>>
    where
        F: FnOnce(&Type) -> Fut + Send + 'static,
        Fut: std::future::Future<Output = Error> + Send,
    {
        async move {
            match_value_data!(self.data => {
                ValueData::Typed { ref tp, .. } => {
                    if &**tp == &T::grease_type() {
                        Ok(unsafe { TypedValue::from_value(self) })
                    } else {
                        Err(on_error(&*tp).await)
                    }
                }
                ValueData::Dynamic { fut } => Ok(unsafe {
                    TypedValue::from_value(Value {
                        data: ValueData::Typed {
                            tp: RArc::new(T::grease_type()),
                            fut: BoxSharedFuture::from_eager(fut.into_eager().then(move |r| async move {
                                match r {
                                    RResult::ROk(AnyValue {inner}) => {
                                        let v = match inner.typed::<T, F, Fut>(on_error).await {
                                            Ok(v) => v,
                                            Err(e) => return RResult::RErr(e)
                                        };
                                        let v: Value = v.into();
                                        v.await.into()
                                    },
                                    RResult::RErr(e) => RResult::RErr(e)
                                }
                            }).await),
                        },
                        ..self
                    })
                })
            })
        }.boxed()
    }

    /// Try to convert this Value by reference to a TypedValue reference.
    ///
    /// This will only return Some if the type is immediately known (i.e. not dynamic).
    pub fn typed_ref<'a, T: GreaseType>(&'a mut self) -> Option<TypedValueRef<'a, T>> {
        match &self.data {
            ValueData::Typed { tp, .. } => {
                if &**tp == &T::grease_type() {
                    Some(TypedValueRef::new(self))
                } else {
                    None
                }
            }
            _ => None,
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
    ///
    /// This may force the value to be evaluated if dynamically typed.
    pub fn grease_type(&self) -> futures::future::BoxFuture<'_, crate::Result<&Type>> {
        async move {
            match &self.data {
                ValueData::Dynamic { fut } => {
                    if let RResult::ROk(v) = fut.clone().await {
                        let _unused = v.inner.grease_type().await;
                    }
                }
                _ => (),
            }
            self.peek_type().unwrap()
        }
        .boxed()
    }

    /// Get the type of the contained value if immediately available.
    pub fn grease_type_immediate(&self) -> Option<&Type> {
        match_value_data!(&self.data => {
            ValueData::Typed { tp, ..} => Some(&**tp),
            ValueData::Dynamic { .. } => None
        })
    }

    /// Get the type of the contained value, if immediately available.
    ///
    /// This may be an error in the case of a dynamically-typed value.
    pub fn peek_type(&self) -> Option<crate::Result<&Type>> {
        match_value_data!(&self.data => {
            ValueData::Typed { tp, .. } => Some(Ok(&**tp)),
            ValueData::Dynamic { ref fut } => fut.peek().and_then(|r| match r {
                RResult::ROk(ref v) => v.inner.peek_type(),
                RResult::RErr(e) => Some(Err(e.clone())),
            })
        })
    }

    /// Call a function on the type of this value.
    ///
    /// If the type is immediately available, the function is called immediately and any error will
    /// be eagerly returned, otherwise it is delayed in the returned future until the future is
    /// executed.
    pub async fn with_type<F, Fut, R>(
        &self,
        f: F,
    ) -> crate::Result<futures::future::BoxFuture<'static, crate::Result<R>>>
    where
        F: FnOnce(&Type) -> Fut + Send + 'static,
        Fut: Future<Output = crate::Result<R>> + Send + 'static,
        R: Send + 'static,
    {
        match self.peek_type() {
            Some(Err(e)) => Err(e),
            Some(Ok(t)) => f(t).await.map(|r| futures::future::ok(r).boxed()),
            None => {
                let this = self.clone();
                Ok(async move { this.grease_type().and_then(f).await }.boxed())
            }
        }
    }

    /// Return a new value based on the type of this one.
    ///
    /// If the type is immediately available, the function will be called immediately.
    pub async fn type_and_then_value<F, Fut, D>(self, f: F, deps: D) -> crate::Result<Value>
    where
        F: FnOnce(Type, Value) -> Fut + Send + 'static,
        Fut: Future<Output = crate::Result<Value>> + Send + 'static,
        D: Into<Dependencies>,
    {
        match self.peek_type() {
            Some(Err(e)) => Err(e),
            Some(Ok(t)) => f(t.clone(), self).await,
            None => {
                let v = self;
                let deps = depends![v, ^deps.into()];
                Ok(Value::dyn_new(
                    async move { Ok(f(v.grease_type().await?.clone(), v).await?.into_any_value()) },
                    deps,
                ))
            }
        }
    }

    /// Get the result of the value, if immediately available.
    pub fn peek(&self) -> Option<Result> {
        match_value_data!(&self.data => {
            ValueData::Typed { fut, .. } => fut.peek().map(|v| RResult::into_result(v.clone())),
            ValueData::Dynamic { fut } => fut
                .peek()
                .and_then(|v| match v {
                    RResult::ROk(v) => v.inner.peek(),
                    RResult::RErr(e) => Some(Err(e.clone()))
                })
        })
    }

    /// Return a value based on the data of this one.
    ///
    /// If the data is immediately available, the function will be called immediately.
    pub async fn and_then_value<F, Fut, D>(self, f: F, deps: D) -> crate::Result<Value>
    where
        F: FnOnce(Value) -> Fut + Send + 'static,
        Fut: Future<Output = crate::Result<Value>> + Send + 'static,
        D: Into<Dependencies>,
    {
        match self.peek() {
            Some(Err(e)) => Err(e),
            Some(Ok(_)) => f(self).await,
            None => {
                let v = self;
                let deps = depends![v, ^deps.into()];
                Ok(Value::dyn_new(
                    async move { Ok(f(v).await?.into_any_value()) },
                    deps,
                ))
            }
        }
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

    /// Get an Eager version of this Value's future.
    pub fn into_eager(self) -> Eager<futures::future::BoxFuture<'static, Result>> {
        match_value_data!(self.data => {
            ValueData::Typed { fut, .. } => fut.into_eager().map(RResult::into_result).boxed(),
            ValueData::Dynamic { fut } => fut
                .into_eager()
                .map(RResult::into_result)
                .and_then_eager(|v| v.inner.into_eager().into_future())
                .boxed(),
        })
    }
}

impl Future for Value {
    type Output = Result;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let me = &mut *self;
        match_value_data!(&mut me.data => {
            ValueData::Typed { fut, .. } => Future::poll(Pin::new(fut), cx).map(RResult::into_result),
            ValueData::Dynamic { fut } => {
                match Future::poll(Pin::new(fut), cx) {
                    Poll::Pending => Poll::Pending,
                    Poll::Ready(RResult::ROk(v)) => {
                        *me = v.inner;
                        Future::poll(self, cx)
                    },
                    Poll::Ready(RResult::RErr(e)) => Poll::Ready(Err(e))
                }
            }
        })
    }
}

/// Match expression for Types.
///
/// Matching is based on types, not patterns, and each type must implement GreaseType. The else
/// case is required.
///
/// The evaluation context is similar to regular match expressions, in that flow-control statements
/// will pertain to the calling code.
#[macro_export]
macro_rules! match_value_type {
    ( $type:expr => { $( $t:ty => $e:expr $(,)? )+ => $else:expr } ) => {
        {
            let grease__match_value_type__tp: &$crate::types::Type = $type;
            $( if grease__match_value_type__tp == &<$t as $crate::types::GreaseType>::grease_type() { $e } else )+ { $else }
        }
    };
}

/// Match expression for Values.
///
/// Matching is based on types, not patterns, and each type must implement GreaseType. The else
/// case is required. All cases must be in the form `|name| body` that evaluate to a final type T,
/// and expressions must all agree on this final type. For each `body`, `name` will be bound to a
/// TypedValue according to the case type.
///
/// The macro evaluates to a `impl Future<Output = grease::Result<T>>`, where `T` is the case return
/// type. Thus, the try operator (`?`) may be used within case expressions.
///
/// If `peek` is prepended, the macro evaluates to a `impl Future<Output = grease::Result<impl
/// Future<Output = grease::Result<T>>>>`, evaluating the match in the outer future if the type is
/// available synchronously.
///
/// If the macro is called with the form `match_value!([val], typed [type] => ...)`, the branches
/// are executed immediately and the value is returned without a future or result wrapper.
#[macro_export]
macro_rules! match_value {
    ( $value:expr => $branches:tt ) => {
        async {
            match $value.grease_type().await {
                Err(e) => return Err(e),
                Ok(tp) => Ok($crate::match_value!($value, typed tp => $branches))
            }
        }
    };
    ( peek $value:expr => $branches:tt ) => {
        {
            let match_value__value = $value.clone();
            $value.with_type(move |tp| {
                let tp = tp.clone();
                async move {
                    Ok($crate::match_value!(match_value__value, typed &tp => $branches))
                }
            })
        }
    };
    ( $value:expr , typed $ty:expr => { $( $t:ty => |$bind:pat| $e:expr $(,)? )+ => |$elsebind:pat| $else:expr } ) => {
        $crate::match_value_type!($ty => {
            $( $t => {
                let $bind = unsafe { $crate::value::TypedValue::<$t>::from_value($value) };
                $e
            } ),+
            => {
                let $elsebind = $value;
                $else
            }
        })
    };
}

/// A reference to a TypedValue result.
pub type Ref<T> = crate::type_erase::Ref<T, RArc<Erased>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedValue<T> {
    inner: Value,
    phantom: std::marker::PhantomData<Arc<T>>,
}

impl<T: GreaseType> TypedValue<T> {
    /// Create a typed value from the given value.
    ///
    /// ### Safety
    /// The caller must ensure that the Value has type `T`.
    pub unsafe fn from_value(v: Value) -> Self {
        TypedValue {
            inner: v,
            phantom: Default::default(),
        }
    }
}

impl<T: GreaseType + Send + Sync + 'static> TypedValue<T> {
    /// Create a typed value from the given future and dependencies.
    pub fn new<F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = std::result::Result<T, Error>> + Send + 'static,
        D: Into<Dependencies>,
    {
        unsafe {
            TypedValue::from_value(Value::new(
                RArc::new(T::grease_type()),
                value.map_ok(|r| RArc::new(Erased::new(r))),
                deps,
            ))
        }
    }

    /// Create a typed value from the given future (which cannot fail) and dependencies.
    pub fn ok<F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = T> + Send + 'static,
        D: Into<Dependencies>,
    {
        Self::new(value.map(Ok), deps)
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
        let id = value_id(Some(&T::grease_type()), &deps.into());
        unsafe {
            TypedValue::from_value(Value::ready(
                std::sync::Arc::new(T::grease_type()),
                std::sync::Arc::new(Erased::new(data)),
                Default::default(),
                id,
            ))
        }
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
