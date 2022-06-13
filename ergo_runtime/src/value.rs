//! Script values.

use crate::abi_stable::{
    bst::{BstMap, BstSet},
    future, sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox},
    type_erase::{Eraseable, Erased},
    u128::U128,
    StableAbi,
};
use crate::dependency::{
    Dependencies, DependenciesConstant, GetDependencies, GetDependenciesConstant,
};
use crate::hash::HashFn;
use crate::type_system::{ErgoType, Type};
use crate::Context;
use cachemap::CacheMap;

/// A shared runtime value.
///
/// Values have an id and metadata, and either a type and associated data or a future to evaluate
/// it.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Value {
    id: RArc<ValueId>,
    metadata: BstMap<U128, RArc<Erased>>,
    data: ValueData,
}

/// Metadata key types.
///
/// Keys generate the metadata id and have an associated output type.
pub trait MetadataKey {
    type Value: Eraseable;

    /// Get the metadata identifier from this key.
    fn id(&self) -> u128;
}

/// A value's identity information.
#[derive(Debug, Clone, Copy, Eq, StableAbi)]
#[repr(C)]
pub struct IdInfo<T> {
    pub id: T,
    pub eval_for_id: bool,
}

impl<T> IdInfo<T> {
    /// Create a new IdInfo with the given identity.
    pub fn new(id: T) -> Self {
        IdInfo {
            id,
            eval_for_id: false,
        }
    }

    /// Set the eval_for_id flag.
    pub fn eval_for_id(mut self, eval_for_id: bool) -> Self {
        self.eval_for_id = eval_for_id;
        self
    }

    /// Map a function on the identity value.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> IdInfo<U> {
        IdInfo {
            id: f(self.id),
            eval_for_id: self.eval_for_id,
        }
    }
}

impl<T> std::borrow::Borrow<T> for IdInfo<T> {
    fn borrow(&self) -> &T {
        &self.id
    }
}

impl<T: PartialEq> PartialEq for IdInfo<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Ord> Ord for IdInfo<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T: PartialOrd> PartialOrd for IdInfo<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for IdInfo<T> {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.id.hash(h);
    }
}

pub type Identity = IdInfo<u128>;

impl<T: std::borrow::Borrow<Self>> std::iter::Sum<T> for Identity {
    fn sum<I: Iterator<Item = T>>(iter: I) -> Self {
        let mut h = HashFn::default();
        let mut eval_for_id = false;
        for i in iter {
            let r = i.borrow();
            std::hash::Hash::hash(&r.id, &mut h);
            eval_for_id |= r.eval_for_id;
        }
        Identity {
            id: h.finish_ext(),
            eval_for_id,
        }
    }
}

mod value_id {
    use super::*;
    use crate::abi_stable::{future::RawWaker, std_types::RVec};
    use std::sync::atomic::{AtomicU8, Ordering};

    #[sabi_trait]
    trait GetId: Send {
        #[sabi(last_prefix_field)]
        fn get(&self) -> future::BoxFuture<'static, IdInfo<U128>>;
    }

    impl<F> GetId for F
    where
        F: FnOnce() -> future::BoxFuture<'static, IdInfo<U128>> + Send + Clone,
    {
        fn get(&self) -> future::BoxFuture<'static, IdInfo<U128>> {
            (self.clone())()
        }
    }

    #[derive(StableAbi)]
    #[repr(C)]
    union State {
        deps: std::mem::ManuallyDrop<GetId_TO<'static, RBox<()>>>,
        wakers: std::mem::ManuallyDrop<RVec<RawWaker>>,
        id: U128,
    }

    // First two bits communicate which field of `state` is valid.
    const STATE_MASK: u8 = 1 << 0 | 1 << 1;
    // const STATE_NONE: u8 = 0;
    const STATE_DEPS: u8 = 1 << 0;
    const STATE_WAKERS: u8 = 1 << 1;
    const STATE_ID: u8 = 1 << 0 | 1 << 1;

    // Next two bits communicate the access history.
    const ACCESS_MASK: u8 = 1 << 2 | 1 << 3;
    const ACCESS_NOT_ACCESSED: u8 = 0;
    const ACCESS_ACCESSED: u8 = 1 << 2;
    const ACCESS_EVALUATED: u8 = 1 << 2 | 1 << 3; // Note this sets both bits, allowing `ACCESSED` to serve as a flag.

    // Next bit designates whether the `wakers` are in use.
    const USING_WAKERS: u8 = 1 << 4;

    // Next two bits are flags related to the identity.
    const EVAL_FOR_ID: u8 = 1 << 5;

    #[derive(StableAbi)]
    #[repr(C)]
    pub struct ValueId {
        state: std::cell::UnsafeCell<State>,
        tag: AtomicU8,
    }

    impl std::fmt::Debug for ValueId {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            let v = self.tag.load(Ordering::Acquire);
            match v & ACCESS_MASK {
                ACCESS_NOT_ACCESSED => write!(f, "<unaccessed>"),
                ACCESS_ACCESSED => write!(f, "<evaluating>"),
                ACCESS_EVALUATED => {
                    if (v & STATE_MASK) == STATE_ID {
                        write!(f, "{:032x}", unsafe {
                            self.state.get().as_ref().unwrap_unchecked().id.as_ref()
                        })
                    } else {
                        write!(f, "<error>")
                    }
                }
                _ => panic!("invalid value id state"),
            }?;
            if v & EVAL_FOR_ID != 0 {
                write!(f, " (eval_for_id)")?;
            }
            Ok(())
        }
    }

    unsafe impl Sync for ValueId {}

    impl ValueId {
        pub fn new<F, Fut>(id: F) -> Self
        where
            F: FnOnce() -> Fut + Send + Clone + 'static,
            Fut: std::future::Future<Output = Identity> + Send,
        {
            ValueId {
                state: std::cell::UnsafeCell::new(State {
                    deps: std::mem::ManuallyDrop::new(GetId_TO::from_value(
                        || future::BoxFuture::new(async move { id().await.map(|v| v.into()) }),
                        TD_Opaque,
                    )),
                }),
                tag: AtomicU8::new(ACCESS_NOT_ACCESSED | STATE_DEPS),
            }
        }

        pub fn id(id: u128) -> Self {
            ValueId {
                state: std::cell::UnsafeCell::new(State { id: id.into() }),
                tag: AtomicU8::new(ACCESS_EVALUATED | STATE_ID),
            }
        }

        pub fn from_value(mut v: Value) -> Self {
            Self::new(move || async move { v.eval_id().await })
        }

        pub fn from_deps(deps: Dependencies) -> Self {
            Self::new(move || async move { deps.id().await })
        }

        pub fn from_constant_deps(deps: DependenciesConstant) -> Self {
            let id = deps.id();
            let mut ret = Self::id(id.id);
            ret.set_eval_for_id(id.eval_for_id);
            ret
        }

        pub async fn immediate(deps: Dependencies) -> Self {
            let id = deps.id().await;
            let mut ret = Self::id(id.id);
            ret.set_eval_for_id(id.eval_for_id);
            ret
        }

        pub fn try_get(&self) -> Option<&u128> {
            (self.tag.load(Ordering::Acquire) & STATE_MASK == STATE_ID).then(|| {
                // Safety: if the tag has STATE_ID, the state _must_ be a U128 (and will not
                // change)
                unsafe { self.state.get().as_ref().unwrap_unchecked().id.as_ref() }
            })
        }

        pub fn set_eval_for_id(&mut self, eval_for_id: bool) {
            if eval_for_id {
                *self.tag.get_mut() |= EVAL_FOR_ID;
            } else {
                *self.tag.get_mut() &= !EVAL_FOR_ID;
            }
        }

        fn eval_for_id(&self) -> bool {
            self.tag.load(Ordering::Relaxed) & EVAL_FOR_ID != 0
        }

        /// Returns the identity.
        pub fn get<'a>(&'a self, value: &'a Value) -> Get<'a> {
            Get::new(self, value)
        }
    }

    mod get_impl {
        use super::*;
        use std::future::Future;
        use std::pin::Pin;
        use std::task::{Context, Poll};

        #[pin_project::pin_project]
        pub struct Get<'a> {
            value_id: &'a ValueId,
            value: &'a Value,
            to_poll: Option<future::BoxFuture<'static, IdInfo<U128>>>,
        }

        impl<'a> Get<'a> {
            pub fn new(value_id: &'a ValueId, value: &'a Value) -> Self {
                Get {
                    value_id,
                    value,
                    to_poll: None,
                }
            }
        }

        const MAX_SPINS: usize = 20;

        impl<'a> Future for Get<'a> {
            type Output = Identity;

            fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
                let proj = self.project();
                let mut spins = 0;
                loop {
                    match proj.to_poll {
                        Some(fut) => {
                            match Pin::new(fut).poll(cx) {
                                Poll::Pending => break Poll::Pending,
                                Poll::Ready(id) => {
                                    // Set the state to STATE_NONE
                                    proj.value_id.tag.fetch_and(!STATE_MASK, Ordering::Acquire);
                                    // Wait for nothing to be accessing the wakers
                                    while proj.value_id.tag.load(Ordering::Acquire) & USING_WAKERS
                                        != 0
                                    {
                                        // Should be a fairly short wait.
                                        spins += 1;
                                        if spins > MAX_SPINS {
                                            std::thread::yield_now();
                                        } else {
                                            std::hint::spin_loop();
                                        }
                                    }
                                    // Safety: the state must be wakers, not accessed by anything else
                                    let state = unsafe {
                                        proj.value_id.state.get().as_mut().unwrap_unchecked()
                                    };
                                    let wakers =
                                        unsafe { std::mem::ManuallyDrop::take(&mut state.wakers) };
                                    state.id = id.id;
                                    proj.value_id.tag.fetch_or(STATE_ID, Ordering::Relaxed);
                                    if id.eval_for_id {
                                        proj.value_id.tag.fetch_or(EVAL_FOR_ID, Ordering::Relaxed);
                                    }
                                    proj.value_id
                                        .tag
                                        .fetch_or(ACCESS_EVALUATED, Ordering::Release);
                                    for waker in wakers {
                                        waker.into_waker().wake();
                                    }
                                }
                            }
                        }
                        None => (),
                    }

                    let access = proj
                        .value_id
                        .tag
                        .fetch_or(ACCESS_ACCESSED, Ordering::Acquire)
                        & ACCESS_MASK;
                    if access == ACCESS_NOT_ACCESSED {
                        // Safety: if unevaluated and unaccessed, the state must be deps
                        debug_assert!(
                            proj.value_id.tag.load(Ordering::Relaxed) & STATE_MASK == STATE_DEPS
                        );
                        let state =
                            unsafe { proj.value_id.state.get().as_mut().unwrap_unchecked() };
                        proj.value_id.tag.fetch_and(!STATE_MASK, Ordering::Release);
                        let deps = unsafe { std::mem::ManuallyDrop::take(&mut state.deps) };
                        state.wakers = std::mem::ManuallyDrop::new(RVec::new());
                        proj.value_id.tag.fetch_or(STATE_WAKERS, Ordering::Release);
                        let fut = deps.get();
                        *proj.to_poll = Some(fut);
                    } else if access == ACCESS_ACCESSED {
                        if proj
                            .value_id
                            .tag
                            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |v| {
                                ((v & STATE_MASK == STATE_WAKERS) && (v & USING_WAKERS == 0))
                                    .then(|| v | USING_WAKERS)
                            })
                            .is_ok()
                        {
                            let waker: RawWaker = cx.into();
                            // Safety: the state must be wakers, and we must be the only one
                            // accessing it (having set the USING_WAKERS flag).
                            let state =
                                unsafe { proj.value_id.state.get().as_mut().unwrap_unchecked() };
                            unsafe { &mut state.wakers }.push(waker);
                            proj.value_id
                                .tag
                                .fetch_and(!USING_WAKERS, Ordering::Release);
                            break Poll::Pending;
                        } else {
                            // There are three cases:
                            // 1. deps -> wakers transition (WAKERS_SET unset), which is pretty
                            //    quick (moving RBox, creating RVec).
                            // 2. wakers -> id transition (WAKERS_SET unset), which is pretty
                            //    quick (moving RVec, copying Identity).
                            // 3. USING_WAKERS set, which will usually be fast though the
                            //    vector growing may be slow.
                            spins += 1;
                            if spins > MAX_SPINS {
                                std::thread::yield_now();
                            } else {
                                std::hint::spin_loop();
                            }
                        }
                    } else if access == ACCESS_EVALUATED {
                        let state = proj.value_id.tag.load(Ordering::Relaxed) & STATE_MASK;
                        if state == STATE_ID {
                            // Safety: the state must be `id`
                            let id =
                                unsafe { proj.value_id.state.get().as_ref().unwrap_unchecked().id }
                                    .into();
                            break Poll::Ready(Identity {
                                id,
                                eval_for_id: proj.value_id.eval_for_id(),
                            });
                        } else {
                            panic!("invalid value id state");
                        }
                    } else {
                        panic!("invalid value id access state");
                    }
                }
            }
        }
    }

    pub use get_impl::Get;

    impl Drop for ValueId {
        fn drop(&mut self) {
            let state = *self.tag.get_mut() & STATE_MASK;
            if state == STATE_DEPS {
                // Safety: the state must be deps
                unsafe { std::mem::ManuallyDrop::drop(&mut self.state.get_mut().deps) };
            } else if state == STATE_WAKERS {
                // Safety: the state must be wakers
                unsafe { std::mem::ManuallyDrop::drop(&mut self.state.get_mut().wakers) };
            }
        }
    }

    impl From<Value> for ValueId {
        fn from(v: Value) -> Self {
            Self::from_value(v)
        }
    }

    impl From<u128> for ValueId {
        fn from(id: u128) -> Self {
            Self::id(id)
        }
    }

    impl From<Dependencies> for ValueId {
        fn from(deps: Dependencies) -> Self {
            Self::from_deps(deps)
        }
    }

    impl From<DependenciesConstant> for ValueId {
        fn from(deps: DependenciesConstant) -> Self {
            Self::from_constant_deps(deps)
        }
    }

    impl<T: Into<ValueId>> From<IdInfo<T>> for ValueId {
        fn from(s: IdInfo<T>) -> Self {
            let mut v = s.id.into();
            v.set_eval_for_id(s.eval_for_id);
            v
        }
    }
}

pub use value_id::ValueId;

impl Value {
    /// Create a new evaluated Value with the given identity.
    ///
    /// ### Safety
    /// The caller must ensure the data corresponds to the type.
    pub unsafe fn new<T>(tp: RArc<Type>, data: RArc<Erased>, id: T) -> Self
    where
        T: Into<ValueId>,
    {
        Value {
            id: RArc::new(id.into()),
            metadata: Default::default(),
            data: ValueData::Typed { tp, data },
        }
    }

    /// Create a typed, evaluated value.
    pub fn evaluated<T: ErgoType + Eraseable + GetDependencies>(value: T) -> Self {
        let deps = value.get_depends();
        Self::with_id(value, deps)
    }

    /// Create a constant (typed, evaluated, identified) value.
    pub fn constant<T: ErgoType + Eraseable + GetDependenciesConstant>(value: T) -> Self {
        let deps = value.get_depends();
        Self::with_id(value, deps)
    }

    /// Create a typed, evaluated value with the given identity.
    pub fn with_id<T: ErgoType + Eraseable, D>(value: T, id: D) -> Self
    where
        D: Into<ValueId>,
    {
        unsafe { Self::new(RArc::new(T::ergo_type()), RArc::new(Erased::new(value)), id) }
    }

    /// Create a dynamically-typed (unevaluated) value with the given identity.
    ///
    /// `FnOnce + Clone` is used rather than `Fn` because this is more convenient for callers when
    /// moving values into the returned Future.
    pub fn dynamic<F, Fut, D>(func: F, id: D) -> Self
    where
        F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
        Fut: std::future::Future<Output = Value> + Send,
        D: Into<ValueId>,
    {
        let next = DynamicNext_TO::from_value(
            std::sync::Arc::new(DynamicCached {
                func,
                complete: Default::default(),
            }),
            TD_Opaque,
        );
        Value {
            id: RArc::new(id.into()),
            metadata: Default::default(),
            data: ValueData::Dynamic { next },
        }
    }

    /// Create a dynamically-typed (unevaluated) value with the given identity which represents an
    /// impure operation.
    ///
    /// This is just like `dynamic`, except the resulting value will not cache evaluated results
    /// based on identity (i.e. `func` will _always_ be called when the value is evaluated).
    pub fn dynamic_impure<F, Fut, D>(func: F, id: D) -> Self
    where
        F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
        Fut: std::future::Future<Output = Value> + Send,
        D: Into<ValueId>,
    {
        let next =
            DynamicNext_TO::from_value(std::sync::Arc::new(DynamicImpure { func }), TD_Opaque);
        Value {
            id: RArc::new(id.into()),
            metadata: Default::default(),
            data: ValueData::Dynamic { next },
        }
    }

    /// Get the value's immediate identity.
    pub async fn immediate_id(&self) -> Identity {
        self.id.get(self).await
    }

    /// Try to get the value's immediate identity, if available.
    pub fn try_immediate_id(&self) -> Option<&u128> {
        self.id.try_get()
    }

    /// Get the value's identity, evaluating the value as necessary to get a more accurate
    /// identity.
    pub async fn eval_id(&mut self) -> Identity {
        let ctx = Context::global();
        let _token = ctx.progress.attempt_progress();
        loop {
            let id = self.immediate_id().await;
            if !id.eval_for_id {
                break;
            }
            match ctx.progress.eval_once_checking_progress(self).await {
                Ok(true) => break,
                Ok(false) => continue,
                Err(_) => {
                    // Bail out, assuming the error will be caught in later evaluation.
                    // FIXME this would be better if the function returned a Result, however the
                    // places where it's used don't have Results either. Maybe interact directly
                    // with the error scope?
                    break;
                }
            }
        }
        self.immediate_id().await
    }

    /// Get the value's identity, evaluating a clone of the value as necessary to get a more
    /// accurate identity.
    pub async fn id(&self) -> u128 {
        let mut v = self.clone();
        v.eval_id().await.id
    }

    /// Get the value's referential identity, which is unique to a particular Value instance in
    /// memory.
    pub fn referential_id(&self) -> usize {
        self.id.as_ref() as *const ValueId as usize
    }

    /// Get the value's type, if evaluated.
    pub fn ergo_type(&self) -> Option<&Type> {
        match &self.data {
            ValueData::Typed { tp, .. } => Some(tp.as_ref()),
            _ => None,
        }
    }

    /// Get the value's data, if evaluated.
    pub fn data(&self) -> Option<&Erased> {
        match &self.data {
            ValueData::Typed { data, .. } => Some(data.as_ref()),
            _ => None,
        }
    }

    /// Check whether this value has the given type.
    pub fn is_type<T: ErgoType>(&self) -> bool {
        match &self.data {
            ValueData::Typed { tp, .. } => tp.as_ref() == &T::ergo_type(),
            _ => false,
        }
    }

    /// Get this value as the given type, if evaluated to that type.
    pub fn as_type<T: ErgoType>(self) -> Result<TypedValue<T>, Self> {
        match &self.data {
            ValueData::Typed { tp, .. } => {
                if tp.as_ref() == &T::ergo_type() {
                    return Ok(unsafe { TypedValue::from_value(self) });
                }
            }
            _ => (),
        }
        Err(self)
    }

    /// Get an IdentifiedValue from this value.
    pub async fn as_identified(self) -> IdentifiedValue {
        IdentifiedValue::from_value(self).await
    }

    /// Get an EvaluatedValue from this value.
    pub async fn as_evaluated(self) -> EvaluatedValue {
        EvaluatedValue::from_value(self).await
    }

    /// Get this value as the given type if evaluated to that type.
    pub fn as_ref<T: ErgoType>(&self) -> Option<&T> {
        match &self.data {
            ValueData::Typed { tp, data } if tp.as_ref() == &T::ergo_type() => {
                Some(unsafe { data.as_ref().as_ref::<T>() })
            }
            _ => None,
        }
    }

    /// Get this value as the given mutable type, if evaluated to that type and no other references
    /// exist.
    pub fn as_mut<T: ErgoType>(&mut self) -> Option<&mut T> {
        match &mut self.data {
            ValueData::Typed { tp, data } if tp.as_ref() == &T::ergo_type() => {
                if let Some(data) = RArc::get_mut(data) {
                    Some(unsafe { data.as_mut::<T>() })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Set the dependencies of this value.
    pub fn set_dependencies<D>(&mut self, deps: D)
    where
        D: Into<ValueId>,
    {
        self.id = RArc::new(deps.into());
    }

    /// Set a metadata entry for this value.
    pub fn set_metadata<T: MetadataKey>(&mut self, key: &T, value: T::Value) {
        self.metadata
            .insert(key.id().into(), RArc::new(Erased::new(value)));
    }

    /// Copy the metadata from another value.
    pub fn copy_metadata(&mut self, from: &Self) {
        self.metadata = from.metadata.clone();
    }

    /// Clear a metadata entry for this value.
    pub fn clear_metadata<T: MetadataKey>(&mut self, key: &T) {
        self.metadata.remove(&key.id());
    }

    /// Get a metadata entry for this value.
    pub fn get_metadata<T: MetadataKey>(&self, key: &T) -> Option<Ref<T::Value>> {
        self.metadata
            .get(&key.id())
            .map(|v| unsafe { Ref::new(v.clone()) })
    }

    /// Evaluate the value once (if dynamic).
    pub async fn eval_once(&mut self) {
        if let ValueData::Dynamic { .. } = &self.data {
            let id = self.immediate_id().await.map(|v| v.into());
            match std::mem::replace(&mut self.data, ValueData::None) {
                ValueData::Dynamic { next } => {
                    let Value { id, metadata, data } = next.next(id).await;
                    self.id = id;
                    self.metadata.extend(metadata);
                    self.data = data;
                }
                _ => panic!("unexpected value data state"),
            }
        }
    }

    /// Return whether this value is fully evaluated (and has a type and data immediately available).
    pub fn is_evaluated(&self) -> bool {
        match &self.data {
            ValueData::Typed { .. } => true,
            _ => false,
        }
    }
}

/// Match expression for Values.
///
/// Matching is based on types implementing ErgoType. You may write the match expression
/// approximately as you normally would, except that the match value comes first (followed by a
/// comma), each pattern must be a struct, tuple struct, or 'or' pattern, and the final pattern
/// must be a wildcard or identifier pattern (as the required else pattern). The struct patterns
/// are matched by reference to the value's data, and the else pattern is matched to the original
/// value (if the value is dynamically typed or none of the type cases matched).
///
/// ## Example
/// ```
/// # use ergo_runtime::value::{match_value, Value};
/// # use ergo_runtime::types::{Unit, Bool, MapEntry};
/// let value: Value = Unit.into();
/// let result = match_value! { value,
///     Bool(b) => b,
///     MapEntry { key, value } => true,
///     _ => false
/// };
/// assert_eq!(result, false);
/// ```
pub use ergo_runtime_macro::match_value;

/// A reference to a type-erased result.
pub type Ref<T> = crate::abi_stable::type_erase::Ref<T, RArc<Erased>>;

/// A value with a known rust type.
#[derive(StableAbi)]
#[repr(C)]
#[sabi(phantom_field = "phantom: RArc<T>")]
pub struct TypedValue<T> {
    inner: Value,
    phantom: std::marker::PhantomData<RArc<T>>,
}

impl<T> TypedValue<T> {
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

impl<T: ErgoType + Eraseable> TypedValue<T> {
    /// Create a typed value.
    pub fn new(data: T) -> Self
    where
        T: GetDependencies,
    {
        unsafe { TypedValue::from_value(Value::evaluated(data)) }
    }

    /// Create a value with constant dependencies.
    pub fn constant(data: T) -> Self
    where
        T: GetDependenciesConstant,
    {
        unsafe { TypedValue::from_value(Value::constant(data)) }
    }

    /// Create a value with the given identity.
    pub fn with_id<D: Into<ValueId>>(data: T, id: D) -> Self {
        unsafe { TypedValue::from_value(Value::with_id(data, id)) }
    }

    /// Extract the Value's type as an owned value.
    pub fn to_owned(self) -> T
    where
        T: Clone,
    {
        match self.inner.data {
            ValueData::Typed { data, .. } => match RArc::try_unwrap(data) {
                Ok(e) => unsafe { e.to_owned() },
                Err(arc) => unsafe { arc.as_ref().as_ref::<T>() }.clone(),
            },
            _ => panic!("invalid TypedValue"),
        }
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

impl<T> std::ops::Deref for TypedValue<T> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for TypedValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> AsRef<T> for TypedValue<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.inner.data().unwrap().as_ref::<T>() }
    }
}

impl<T> From<TypedValue<T>> for Value {
    fn from(v: TypedValue<T>) -> Self {
        v.inner
    }
}

impl<T> std::borrow::Borrow<Value> for TypedValue<T> {
    fn borrow(&self) -> &Value {
        &self.inner
    }
}

#[sabi_trait]
trait DynamicNext: Clone + Send + Sync {
    #[sabi(last_prefix_field)]
    fn next(self, id: IdInfo<U128>) -> future::BoxFuture<'static, Value>;
}

struct DynamicCached<F> {
    func: F,
    complete: CacheMap<u128, futures::lock::Mutex<Option<Value>>>,
}

impl<F, Fut> DynamicNext for std::sync::Arc<DynamicCached<F>>
where
    F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
    Fut: std::future::Future<Output = Value> + Send,
{
    fn next(self, id: IdInfo<U128>) -> future::BoxFuture<'static, Value> {
        future::BoxFuture::new(async move {
            let mut guard = self.complete.cache_default(id.id.into()).lock().await;
            if let Some(v) = &*guard {
                v.clone()
            } else {
                let val = (self.func.clone())().await;
                *guard = Some(val.clone());
                drop(guard);
                val
            }
        })
    }
}

struct DynamicImpure<F> {
    func: F,
}

impl<F, Fut> DynamicNext for std::sync::Arc<DynamicImpure<F>>
where
    F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
    Fut: std::future::Future<Output = Value> + Send,
{
    fn next(self, _id: IdInfo<U128>) -> future::BoxFuture<'static, Value> {
        future::BoxFuture::new(async move { (self.func.clone())().await })
    }
}

/// The data within a Value.
#[derive(Clone, StableAbi)]
#[repr(C)]
enum ValueData {
    Typed {
        tp: RArc<Type>,
        data: RArc<Erased>,
    },
    Dynamic {
        next: DynamicNext_TO<'static, RBox<()>>,
    },
    None,
}

impl std::fmt::Debug for ValueData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValueData::None => write!(f, "None"),
            ValueData::Dynamic { .. } => write!(f, "Dynamic"),
            ValueData::Typed { tp, .. } => write!(f, "Typed ({:?})", tp),
        }
    }
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

impl<T> std::fmt::Debug for TypedValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("TypedValue")
            .field("inner", &self.inner)
            .finish()
    }
}

/// A value with an immediately-available identity.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct IdentifiedValue(Value);

impl IdentifiedValue {
    pub async fn from_value(mut v: Value) -> Self {
        v.eval_id().await;
        IdentifiedValue(v)
    }

    pub async fn from_value_immediate(v: Value) -> Self {
        v.immediate_id().await;
        IdentifiedValue(v)
    }

    pub fn id(&self) -> &u128 {
        // Safety: you cannot construct this type without first fetching the id
        unsafe { self.0.try_immediate_id().unwrap_unchecked() }
    }

    /// Get the mutable Value.
    ///
    /// # Safety
    /// The caller must ensure than any mutation to the underlying Value will leave the identity in
    /// a state where it is still immediately available.
    pub unsafe fn value_mut(&mut self) -> &mut Value {
        &mut self.0
    }
}

impl std::ops::Deref for IdentifiedValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// IdentifiedValue is not DerefMut because this may invalidate the id state (e.g. if a value has
// its identity changed).

impl From<IdentifiedValue> for Value {
    fn from(v: IdentifiedValue) -> Self {
        v.0
    }
}

impl std::borrow::Borrow<Value> for IdentifiedValue {
    fn borrow(&self) -> &Value {
        &self.0
    }
}

impl PartialEq for IdentifiedValue {
    fn eq(&self, other: &IdentifiedValue) -> bool {
        self.id() == other.id()
    }
}

impl Eq for IdentifiedValue {}

impl PartialOrd for IdentifiedValue {
    fn partial_cmp(&self, other: &IdentifiedValue) -> Option<std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
    }
}

impl Ord for IdentifiedValue {
    fn cmp(&self, other: &IdentifiedValue) -> std::cmp::Ordering {
        self.id().cmp(&other.id())
    }
}

impl std::borrow::Borrow<u128> for IdentifiedValue {
    fn borrow(&self) -> &u128 {
        self.id()
    }
}

impl std::hash::Hash for IdentifiedValue {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.id().hash(h);
    }
}

/// A value with an immediately-available identity and type.
#[derive(Clone, Debug, StableAbi, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct EvaluatedValue(IdentifiedValue);

impl EvaluatedValue {
    pub async fn from_value(mut v: Value) -> Self {
        if let Err(e) = Context::eval(&mut v).await {
            // Ensure that, one way or another, `v` is an evaluated value.  We could change this to
            // return Errors early, but it's possible somebody would want an `EvaluatedValue` that
            // is an `Error`.
            if !v.is_evaluated() {
                v = e.into();
            }
        }
        debug_assert!(v.is_evaluated());
        EvaluatedValue(IdentifiedValue::from_value_immediate(v).await)
    }

    pub fn as_identified(self) -> IdentifiedValue {
        self.0
    }

    pub fn id(&self) -> &u128 {
        self.0.id()
    }

    /// Check whether the evaluate value is an error, and if so return Err.
    pub fn check_error(self) -> crate::Result<Self> {
        Ok(EvaluatedValue(IdentifiedValue(crate::try_value!(
            self.0 .0
        ))))
    }

    /// Get the mutable Value.
    ///
    /// # Safety
    /// The caller must ensure than any mutation to the underlying Value will leave the identity in
    /// a state where it is still immediately available.
    pub unsafe fn value_mut(&mut self) -> &mut Value {
        self.0.value_mut()
    }
}

impl std::ops::Deref for EvaluatedValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<EvaluatedValue> for IdentifiedValue {
    fn from(v: EvaluatedValue) -> Self {
        v.0
    }
}

impl From<EvaluatedValue> for Value {
    fn from(v: EvaluatedValue) -> Self {
        v.0.into()
    }
}

impl std::borrow::Borrow<IdentifiedValue> for EvaluatedValue {
    fn borrow(&self) -> &IdentifiedValue {
        &self.0
    }
}

impl std::borrow::Borrow<Value> for EvaluatedValue {
    fn borrow(&self) -> &Value {
        self.0.borrow()
    }
}

impl std::borrow::Borrow<u128> for EvaluatedValue {
    fn borrow(&self) -> &u128 {
        self.0.borrow()
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
        self.into().inner
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

impl<T> From<T> for IdentifiedValue
where
    T: GetDependenciesConstant + IntoValue,
{
    fn from(v: T) -> IdentifiedValue {
        let val = v.into_value();
        assert!(val.try_immediate_id().is_some());
        IdentifiedValue(val)
    }
}

impl<T> From<T> for EvaluatedValue
where
    T: GetDependenciesConstant + IntoValue,
{
    fn from(v: T) -> EvaluatedValue {
        let val = v.into_value();
        assert!(val.is_evaluated());
        assert!(val.try_immediate_id().is_some());
        EvaluatedValue(IdentifiedValue(val))
    }
}
