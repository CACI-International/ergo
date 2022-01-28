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

/// A value's identity information with accompanying evaluation marker.
#[derive(Debug, Clone, Copy, Eq, StableAbi)]
#[repr(C)]
pub struct EvalForId<T> {
    pub id: T,
    pub should_eval: bool,
}

impl<T> EvalForId<T> {
    pub fn set(id: T) -> Self {
        EvalForId {
            id,
            should_eval: true,
        }
    }

    pub fn clear(id: T) -> Self {
        EvalForId {
            id,
            should_eval: false,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> EvalForId<U> {
        EvalForId {
            id: f(self.id),
            should_eval: self.should_eval,
        }
    }
}

impl<T> std::borrow::Borrow<T> for EvalForId<T> {
    fn borrow(&self) -> &T {
        &self.id
    }
}

impl<T: PartialEq> PartialEq for EvalForId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Ord> Ord for EvalForId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T: PartialOrd> PartialOrd for EvalForId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for EvalForId<T> {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.id.hash(h);
    }
}

pub type Identity = EvalForId<u128>;

impl std::iter::Sum for Identity {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut h = HashFn::default();
        let mut should_eval = false;
        for i in iter {
            std::hash::Hash::hash(&i.id, &mut h);
            should_eval |= i.should_eval;
        }
        Identity {
            id: h.finish_ext(),
            should_eval,
        }
    }
}

impl<'a> std::iter::Sum<&'a Self> for Identity {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        let mut h = HashFn::default();
        let mut should_eval = false;
        for i in iter {
            std::hash::Hash::hash(&i.id, &mut h);
            should_eval |= i.should_eval;
        }
        Identity {
            id: h.finish_ext(),
            should_eval,
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
        fn get(self, v: &Value) -> future::BoxFuture<'static, EvalForId<U128>>;
    }

    impl<F> GetId for F
    where
        F: FnOnce(Value) -> future::BoxFuture<'static, EvalForId<U128>> + Send,
    {
        fn get(self, v: &Value) -> future::BoxFuture<'static, EvalForId<U128>> {
            self(v.clone())
        }
    }

    #[derive(StableAbi)]
    #[repr(C)]
    union State {
        deps: std::mem::ManuallyDrop<GetId_TO<'static, RBox<()>>>,
        wakers: std::mem::ManuallyDrop<RVec<RawWaker>>,
        id: U128,
    }

    const ACCESSED: u8 = 1 << 0;
    const ID_SET: u8 = 1 << 1;
    const WAKERS_SET: u8 = 1 << 2;
    const USING_WAKERS: u8 = 1 << 3;
    const EVAL_ID: u8 = 1 << 4;

    const MASK_STATE: u8 = ACCESSED | ID_SET;

    #[derive(StableAbi)]
    #[repr(C)]
    pub struct ValueId {
        state: std::cell::UnsafeCell<State>,
        // Zeroth bit indicates whether the value has ever been accessed before.
        // First bit indicates whether `state` has `wakers` (and implies the zeroth bit).
        // Second bit indicates whether `state` has `id` (and implies the zeroth bit).
        // Third bit indicates whether the value should be evaluated to get an identity.
        tag: AtomicU8,
    }

    impl std::fmt::Debug for ValueId {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            let v = self.tag.load(Ordering::Acquire);
            match v & MASK_STATE {
                0 => write!(f, "<unaccessed>"),
                ACCESSED => write!(f, "<evaluating>"),
                MASK_STATE => write!(f, "{:032x}", unsafe {
                    self.state.get().as_ref().unwrap_unchecked().id.as_ref()
                }),
                _ => panic!("invalid value id state"),
            }?;
            if v & EVAL_ID != 0 {
                write!(f, " (should_eval)")?;
            }
            Ok(())
        }
    }

    unsafe impl Sync for ValueId {}

    impl ValueId {
        pub fn new<F, Fut>(id: F) -> Self
        where
            F: FnOnce(Value) -> Fut + Send + 'static,
            Fut: std::future::Future<Output = Identity> + Send,
        {
            ValueId {
                state: std::cell::UnsafeCell::new(State {
                    deps: std::mem::ManuallyDrop::new(GetId_TO::from_value(
                        |v| future::BoxFuture::new(async move { id(v).await.map(|v| v.into()) }),
                        TU_Opaque,
                    )),
                }),
                tag: AtomicU8::new(0),
            }
        }

        pub fn from_value(mut v: Value) -> Self {
            Self::new(move |_| async move { v.eval_id().await })
        }

        pub fn from_deps(deps: Dependencies) -> Self {
            Self::new(move |_| async move { deps.id().await })
        }

        pub fn from_constant_deps(deps: DependenciesConstant) -> Self {
            Self::id({
                let mut h = HashFn::default();
                std::hash::Hash::hash(&deps, &mut h);
                h.finish_ext()
            })
        }

        pub async fn immediate(deps: Dependencies) -> Self {
            let id = deps.id().await;
            let mut ret = Self::id(id.id);
            ret.set_eval_id(id.should_eval);
            ret
        }

        pub fn id(id: u128) -> Self {
            ValueId {
                state: std::cell::UnsafeCell::new(State { id: id.into() }),
                tag: AtomicU8::new(ACCESSED | ID_SET),
            }
        }

        pub fn try_get(&self) -> Option<&u128> {
            (self.tag.load(Ordering::Acquire) & ID_SET != 0).then(|| {
                // Safety: if the tag has ID_SET, the state _must_ be a U128
                unsafe { self.state.get().as_ref().unwrap_unchecked().id.as_ref() }
            })
        }

        pub fn set_eval_id(&mut self, eval_id: bool) {
            if eval_id {
                *self.tag.get_mut() |= EVAL_ID;
            } else {
                *self.tag.get_mut() &= !EVAL_ID;
            }
        }

        fn eval_id(&self) -> bool {
            self.tag.load(Ordering::Relaxed) & EVAL_ID != 0
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
            to_poll: Option<future::BoxFuture<'static, EvalForId<U128>>>,
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

        impl<'a> Future for Get<'a> {
            type Output = Identity;

            fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
                let proj = self.project();
                loop {
                    if let Some(fut) = proj.to_poll {
                        match Pin::new(fut).poll(cx) {
                            Poll::Pending => return Poll::Pending,
                            Poll::Ready(id) => {
                                proj.value_id.tag.fetch_and(!WAKERS_SET, Ordering::Acquire);
                                // Wait for nothing to be accessing the wakers
                                while proj.value_id.tag.load(Ordering::Acquire) & USING_WAKERS != 0
                                {
                                    // Should be a fairly short wait.
                                    std::hint::spin_loop();
                                }
                                // Safety: the state must be wakers, not accessed by anything else
                                let state = unsafe {
                                    proj.value_id.state.get().as_mut().unwrap_unchecked()
                                };
                                let wakers =
                                    unsafe { std::mem::ManuallyDrop::take(&mut state.wakers) };
                                state.id = id.id;
                                if id.should_eval {
                                    proj.value_id.tag.fetch_or(EVAL_ID, Ordering::Relaxed);
                                }
                                proj.value_id.tag.fetch_or(ID_SET, Ordering::Release);
                                for waker in wakers {
                                    waker.into_waker().wake();
                                }
                                break;
                            }
                        }
                    }
                    match proj.value_id.tag.fetch_or(ACCESSED, Ordering::Acquire) & MASK_STATE {
                        0 => {
                            // Safety: if the tag is 0b00, the state must be deps
                            let state =
                                unsafe { proj.value_id.state.get().as_mut().unwrap_unchecked() };
                            let deps = unsafe { std::mem::ManuallyDrop::take(&mut state.deps) };
                            state.wakers = std::mem::ManuallyDrop::new(RVec::new());
                            proj.value_id.tag.fetch_or(WAKERS_SET, Ordering::Release);
                            *proj.to_poll = Some(deps.get(proj.value));
                        }
                        ACCESSED => {
                            if proj
                                .value_id
                                .tag
                                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |v| {
                                    ((v & WAKERS_SET != 0) && (v & USING_WAKERS == 0))
                                        .then(|| v | USING_WAKERS)
                                })
                                .is_ok()
                            {
                                let waker: RawWaker = cx.into();
                                // Safety: the state must be wakers, and we must be the only one
                                // accessing it (having set the USING_WAKERS flag).
                                let state = unsafe {
                                    proj.value_id.state.get().as_mut().unwrap_unchecked()
                                };
                                unsafe { &mut state.wakers }.push(waker);
                                proj.value_id
                                    .tag
                                    .fetch_and(!USING_WAKERS, Ordering::Release);
                                return Poll::Pending;
                            } else {
                                // There are three cases:
                                // 1. deps -> wakers transition (WAKERS_SET unset), which is pretty
                                //    quick (moving RBox, creating RMutex<RVec>).
                                // 2. wakers -> id transition (WAKERS_SET unset), which is pretty
                                //    quick (moving RMutex<RVec>, copying Identity).
                                // 3. USING_WAKERS set, which will usually be fast though the
                                //    vector growing may be slow.
                                //
                                // TODO might want to yield the thread sometimes
                                std::hint::spin_loop();
                            }
                        }
                        MASK_STATE => break,
                        _ => panic!("invalid value id state"),
                    }
                }
                debug_assert!(proj.value_id.tag.load(Ordering::Relaxed) & MASK_STATE == MASK_STATE);
                // Safety: the tag must have ID_SET, so the state _must_ be a U128
                let id = unsafe { proj.value_id.state.get().as_ref().unwrap_unchecked().id }.into();
                Poll::Ready(Identity {
                    id,
                    should_eval: proj.value_id.eval_id(),
                })
            }
        }
    }

    pub use get_impl::Get;

    impl Drop for ValueId {
        fn drop(&mut self) {
            let tag = *self.tag.get_mut();
            if tag & MASK_STATE == 0 {
                // Safety: if the tag is 0b00, the state must be deps
                unsafe { std::mem::ManuallyDrop::drop(&mut self.state.get_mut().deps) };
            } else if tag & WAKERS_SET != 0 {
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

    impl<T: Into<ValueId>> From<EvalForId<T>> for ValueId {
        fn from(s: EvalForId<T>) -> Self {
            let mut v = s.id.into();
            v.set_eval_id(s.should_eval);
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
            std::sync::Arc::new(futures::lock::Mutex::new(DynamicNextState::Pending(
                func,
                Default::default(),
            ))),
            TU_Opaque,
        );
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
        while self.immediate_id().await.should_eval && !self.is_evaluated() {
            self.eval_once().await;
        }
        self.immediate_id().await
    }

    /// Get the value's identity, evaluating a clone of the value as necessary to get a more
    /// accurate identity.
    pub async fn id(&self) -> u128 {
        let mut v = self.clone();
        v.eval_id().await.id
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
        match std::mem::replace(&mut self.data, ValueData::None) {
            ValueData::Dynamic { next } => {
                let Value { id, metadata, data } = next.next().await;
                self.id = id;
                self.metadata.extend(metadata);
                self.data = data;
            }
            o => self.data = o,
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

/// Metadata key types.
///
/// Keys generate the metadata id and have an associated output type.
pub trait MetadataKey {
    type Value: Eraseable;

    /// Get the metadata identifier from this key.
    fn id(&self) -> u128;
}

#[sabi_trait]
trait DynamicNext: Clone + Send + Sync {
    fn ref_id(&self) -> usize;

    #[sabi(last_prefix_field)]
    fn next(self) -> future::BoxFuture<'static, Value>;
}

impl<F, Fut> DynamicNext for std::sync::Arc<futures::lock::Mutex<DynamicNextState<F>>>
where
    F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
    Fut: std::future::Future<Output = Value> + Send,
{
    fn ref_id(&self) -> usize {
        self.as_ref() as *const futures::lock::Mutex<_> as usize
    }

    fn next(self) -> future::BoxFuture<'static, Value> {
        future::BoxFuture::new(async move {
            let lock_id = self.ref_id();
            if Context::with(|ctx| ctx.evaluating.locking_would_deadlock(lock_id)) {
                return crate::error!(error: "deadlock detected").into();
            }

            let mut guard = self.lock().await;
            match &mut *guard {
                DynamicNextState::Pending(func, complete) => {
                    if !complete.is_empty() {
                        let ids = Context::with(|ctx| ctx.dynamic_scope.ids());
                        if let Some(v) = complete.get(&ids) {
                            return v.clone();
                        }
                    }
                    Context::with(|ctx| ctx.evaluating.locked(lock_id));
                    let func_inner = func.clone();
                    let (val, accessed) =
                        Context::fork(|ctx| ctx.evaluating.push(lock_id), async move {
                            let val = func_inner().await;
                            let accessed = Context::with(|ctx| ctx.dynamic_scope.accessed());
                            (val, accessed)
                        })
                        .await;
                    if accessed.len() == 0 {
                        debug_assert!(complete.is_empty());
                        *guard = DynamicNextState::Done(val.clone());
                    } else {
                        complete.insert(&accessed, val.clone());
                    }
                    drop(guard);
                    Context::with(|ctx| ctx.evaluating.unlocked(lock_id));
                    val
                }
                DynamicNextState::Done(val) => val.clone(),
            }
        })
    }
}

#[derive(Default)]
struct DynamicComplete {
    // The completed values and the number of entries they have in ids.
    values: Vec<(Value, usize)>,
    // HashMap of (keyid, valueid) -> index in values
    ids: std::collections::HashMap<(u128, u128), Vec<usize>>,
}

impl DynamicComplete {
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn insert(&mut self, entries: &[(u128, u128)], value: Value) {
        debug_assert!(!entries.is_empty());
        let index = self.values.len();
        self.values.push((value, entries.len()));
        for e in entries {
            self.ids.entry(*e).or_default().push(index);
        }
    }

    pub fn get(&self, entries: &[(u128, u128)]) -> Option<&Value> {
        let mut counts: Vec<_> = self.values.iter().map(|(v, s)| (v, *s)).collect();
        for e in entries {
            if let Some(indices) = self.ids.get(e) {
                for i in indices {
                    let c = &mut counts[*i];
                    c.1 -= 1;
                    if c.1 == 0 {
                        return Some(&c.0);
                    }
                }
            }
        }
        None
    }
}

enum DynamicNextState<F> {
    Pending(F, DynamicComplete),
    Done(Value),
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
