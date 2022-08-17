//! Script values.

use crate::abi_stable::{
    bst::{BstMap, BstSet},
    future, sabi_trait,
    sabi_trait::prelude::*,
    sabi_types::{RRef, RSmallBox},
    std_types::{RArc, ROption},
    type_erase::{Eraseable, Erased},
    u128::U128,
    StableAbi,
};
use crate::dependency::{Dependencies, DependenciesConstant, Dependency};
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
    inner: RArc<Inner>,
    metadata: BstMap<U128, RArc<Erased>>,
}

#[derive(Clone, StableAbi)]
#[repr(C)]
struct Inner {
    data: ValueData,
    id: Once<IdInfo<U128>>,
    late_bound: Once<LateBound>,
    next: ROption<Once<Value>>,
}

impl Inner {
    pub fn new(data: ValueData) -> Self {
        let typed = data.ergo_type().is_some();
        Inner {
            data,
            id: Default::default(),
            late_bound: Default::default(),
            next: if typed {
                ROption::RNone
            } else {
                ROption::RSome(Default::default())
            },
        }
    }

    pub async fn id(&self) -> Identity {
        self.id
            .get(async { self.data.id().await })
            .await
            .clone()
            .into()
    }

    pub async fn next(&self) -> Value {
        match &self.next {
            ROption::RNone => self.data.next().await,
            ROption::RSome(c) => c.get(async { self.data.next().await }).await.clone(),
        }
    }

    pub fn set_id<D>(&mut self, id: D)
    where
        D: Into<ValueId>,
    {
        let id = id.into();
        let const_id = match &id {
            ValueId::Id(id) => Some(id.clone()),
            ValueId::Lazy(_) | ValueId::Override { .. } => None,
        };
        self.data = ValueData::new(IdValueData {
            // TODO could avoid this clone with some null ValueData placeholder
            value_data: self.data.clone(),
            id,
        });
        if let Some(id) = const_id {
            self.id.set(id.into());
        } else {
            self.id = Default::default();
        }
        self.next.as_mut().map(|v| *v = Default::default());
        self.late_bound = Default::default();
    }

    pub fn late_bind(&mut self, scope: &LateScope) {
        self.data.late_bind(scope);
        let typed = self.data.ergo_type().is_some();
        self.id = Default::default();
        self.late_bound = Default::default();
        self.next = if typed {
            ROption::RNone
        } else {
            ROption::RSome(Default::default())
        };
    }

    pub fn late_bound(&self) -> &LateBound {
        self.late_bound.get_sync(|| self.data.late_bound())
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

/// A value's identity information.
#[derive(Debug, Clone, Copy, Eq, StableAbi)]
#[repr(C)]
pub struct IdInfo<T> {
    pub id: T,
    pub eval_for_id: bool,
}

/// Late-bound keys.
#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct LateBound {
    keys: BstSet<U128>,
}

impl LateBound {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn extend(&mut self, other: Self) {
        self.keys.extend(other.keys)
    }

    pub fn insert(&mut self, key: U128) {
        self.keys.insert(key);
    }

    pub fn bound_by(&self, scope: &LateScope) -> bool {
        self.keys.iter().any(|k| scope.scope.contains_key(k))
    }
}

/// Late-bound value scope.
#[derive(Debug, Default, Clone, StableAbi)]
#[repr(C)]
pub struct LateScope {
    pub scope: BstMap<U128, Value>,
}

impl LateScope {
    /// Create a LateScope with a single entry.
    pub fn with(key: U128, value: Value) -> Self {
        LateScope {
            scope: BstMap::from_iter([(key, value)]),
        }
    }
}

#[sabi_trait]
pub trait TypedValueData: Send + Sync + 'static {
    /// Get the type of the value.
    fn ergo_type(&self) -> Type;

    fn data(&self) -> *const ();
}

impl<T: ErgoType + Send + Sync + 'static> TypedValueData for T {
    fn ergo_type(&self) -> Type {
        T::ergo_type()
    }

    fn data(&self) -> *const () {
        self as *const T as *const ()
    }
}

#[sabi_trait]
pub trait LazyValueData: Send + Sync + 'static {
    /// Get the next value.
    fn next<'a>(&'a self) -> future::BoxFuture<'a, Value>;
}

#[derive(StableAbi)]
#[repr(C)]
pub enum ValueType<'a> {
    Typed(TypedValueData_TO<RRef<'a, ()>>),
    Lazy(LazyValueData_TO<RRef<'a, ()>>),
}

impl<'a> ValueType<'a> {
    pub fn typed<T: TypedValueData + 'static>(v: &'a T) -> Self {
        ValueType::Typed(TypedValueData_TO::from_ptr(RRef::new(v), TD_Opaque))
    }

    pub fn lazy<T: LazyValueData + 'static>(v: &'a T) -> Self {
        ValueType::Lazy(LazyValueData_TO::from_ptr(RRef::new(v), TD_Opaque))
    }
}

#[sabi_trait]
pub trait ValueDataInterface: Clone + Send + Sync + 'static {
    /// Get the identity of a value.
    fn id(&self) -> future::BoxFuture<IdInfo<U128>>;

    /// Provide late bindings to a value.
    fn late_bind(&mut self, scope: &LateScope);

    /// Get the set of late bindings in a value.
    fn late_bound(&self) -> LateBound;

    /// Get the value data.
    fn get(&self) -> ValueType;

    /// Hint that the value will be eval_for_id, to prevent `id` from being called.
    fn eval_for_id_hint(&self) -> future::BoxFuture<bool> {
        future::BoxFuture::new(async { false })
    }
}

#[derive(Clone, StableAbi)]
#[repr(transparent)]
struct ValueData(ValueDataInterface_TO<RSmallBox<(), [usize; 3]>>);

impl ValueData {
    pub fn new<T: ValueDataInterface + 'static>(interface: T) -> Self {
        ValueData(ValueDataInterface_TO::from_ptr(
            RSmallBox::new(interface),
            TD_Opaque,
        ))
    }

    pub async fn eval_for_id_hint(&self) -> bool {
        self.0.eval_for_id_hint().await
    }

    pub async fn id(&self) -> IdInfo<U128> {
        self.0.id().await
    }

    pub fn late_bind(&mut self, scope: &LateScope) {
        self.0.late_bind(scope);
    }

    pub fn is_typed(&self) -> bool {
        match self.0.get() {
            ValueType::Typed(_) => true,
            ValueType::Lazy(_) => false,
        }
    }

    pub fn ergo_type(&self) -> Option<Type> {
        match self.0.get() {
            ValueType::Typed(t) => Some(t.ergo_type()),
            ValueType::Lazy(_) => None,
        }
    }

    pub async fn next(&self) -> Value {
        match self.0.get() {
            ValueType::Typed(_) => panic!("invalid `next` call"),
            ValueType::Lazy(l) => l.next().await,
        }
    }

    pub unsafe fn as_ptr(&self) -> *const () {
        match self.0.get() {
            ValueType::Typed(t) => t.data(),
            ValueType::Lazy(_) => panic!("invalid `as_ref` call"),
        }
    }

    pub unsafe fn as_ref<T>(&self) -> &T {
        &*(self.as_ptr() as *const T)
    }

    pub unsafe fn as_mut<T>(&mut self) -> &mut T {
        &mut *(self.as_ptr() as *const T as *mut T)
    }

    pub unsafe fn into_owned<T: Clone>(self) -> T {
        // TODO possibly could make this work without clone (storing some destructure code)
        self.as_ref::<T>().clone()
    }
}

impl ValueDataInterface for ValueData {
    fn id(&self) -> future::BoxFuture<IdInfo<U128>> {
        self.0.id()
    }

    fn late_bind(&mut self, scope: &LateScope) {
        self.0.late_bind(scope);
    }

    fn late_bound(&self) -> LateBound {
        self.0.late_bound()
    }

    fn get(&self) -> ValueType {
        self.0.get()
    }

    fn eval_for_id_hint(&self) -> future::BoxFuture<bool> {
        self.0.eval_for_id_hint()
    }
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

impl From<Identity> for IdInfo<U128> {
    fn from(i: Identity) -> Self {
        i.map(|i| i.into())
    }
}

impl From<IdInfo<U128>> for Identity {
    fn from(i: IdInfo<U128>) -> Self {
        i.map(|i| i.into())
    }
}

impl<T: std::borrow::Borrow<Self>> std::iter::Sum<T> for Identity {
    fn sum<I: Iterator<Item = T>>(iter: I) -> Self {
        let mut combiner = IdentityCombiner::default();
        for i in iter {
            combiner.add(i.borrow());
        }
        combiner.finish()
    }
}

#[derive(Default)]
pub struct IdentityCombiner {
    h: HashFn,
    eval_for_id: bool,
}

impl IdentityCombiner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, v: &Identity) {
        std::hash::Hash::hash(&v.id, &mut self.h);
        self.eval_for_id |= v.eval_for_id;
    }

    pub fn finish(self) -> Identity {
        Identity {
            id: self.h.finish_ext(),
            eval_for_id: self.eval_for_id,
        }
    }
}

mod once {
    use crate::abi_stable::{future::RawWaker, std_types::RVec, StableAbi};
    use std::sync::atomic::{AtomicU8, Ordering};

    #[derive(StableAbi)]
    #[repr(C)]
    union State<T> {
        wakers: std::mem::ManuallyDrop<RVec<RawWaker>>,
        value: std::mem::ManuallyDrop<T>,
    }

    // First bit indicates whether the state has been accessed.
    const ACCESS_MASK: u8 = 1 << 0;
    const ACCESS_NOT_ACCESSED: u8 = 0;
    const ACCESS_ACCESSED: u8 = 1 << 0;

    // Next two bits communicate the `state`.
    const STATE_MASK: u8 = 1 << 1 | 1 << 2;
    const STATE_NONE: u8 = 0; // `state` is unset
    const STATE_WAKERS: u8 = 1 << 1; // `state` is `wakers`
    const STATE_VALUE: u8 = 1 << 2; // `state` is `value`

    // Next bit designates whether the `wakers` are in use.
    const USING_WAKERS: u8 = 1 << 3;

    #[derive(StableAbi)]
    #[repr(C)]
    pub struct Once<T> {
        state: std::cell::UnsafeCell<State<T>>,
        tag: AtomicU8,
    }

    impl<T: Clone> Clone for Once<T> {
        fn clone(&self) -> Self {
            let mut ret = Self::default();
            if let Some(v) = self.try_get() {
                ret.set(v.clone());
            }
            ret
        }
    }

    impl<T> Default for Once<T> {
        fn default() -> Self {
            Once {
                state: std::cell::UnsafeCell::new(unsafe {
                    std::mem::MaybeUninit::uninit().assume_init()
                }),
                tag: Default::default(),
            }
        }
    }

    impl<T> Once<T> {
        pub fn new() -> Self {
            Default::default()
        }

        pub fn set(&mut self, value: T) {
            let state = unsafe { self.state.get().as_mut().unwrap_unchecked() };
            state.value = std::mem::ManuallyDrop::new(value);
            self.tag = AtomicU8::new(ACCESS_ACCESSED | STATE_VALUE);
        }

        pub fn try_get(&self) -> Option<&T> {
            if self.tag.load(Ordering::Relaxed) & STATE_MASK == STATE_VALUE {
                // Safety: once the state is STATE_VALUE, it will never change and `value` is set
                Some(unsafe { &*self.state.get().as_ref().unwrap_unchecked().value })
            } else {
                None
            }
        }

        // Typically only one of `get` or `get_sync` should be called on a particular instance,
        // though they are safe to call concurrently.

        pub fn get<Fut>(&self, fut: Fut) -> Get<T, Fut> {
            Get::new(self, fut)
        }

        pub fn get_sync<F>(&self, f: F) -> &T
        where
            F: FnOnce() -> T,
        {
            match self.try_get() {
                Some(v) => v,
                None => {
                    let value = f();
                    let state = self.tag.fetch_or(ACCESS_ACCESSED, Ordering::Acquire);
                    if state & ACCESS_MASK == ACCESS_NOT_ACCESSED {
                        let state = unsafe { self.state.get().as_mut().unwrap_unchecked() };
                        state.value = std::mem::ManuallyDrop::new(value);
                        self.tag.fetch_or(STATE_VALUE, Ordering::Release);
                    } else {
                        // Should be a very short duration, just a move.
                        while self.tag.load(Ordering::Relaxed) & STATE_MASK != STATE_VALUE {
                            std::hint::spin_loop();
                        }
                    }
                    debug_assert!(self.tag.load(Ordering::Relaxed) & STATE_MASK == STATE_VALUE);
                    // Safety: once the state is STATE_VALUE, it will never change and `value` is set
                    unsafe { &*self.state.get().as_ref().unwrap_unchecked().value }
                }
            }
        }
    }

    unsafe impl<T: Sync> Sync for Once<T> {}
    unsafe impl<T: Send> Send for Once<T> {}

    impl<T> Drop for Once<T> {
        fn drop(&mut self) {
            let state = *self.tag.get_mut() & STATE_MASK;
            if state == STATE_WAKERS {
                // Safety: the state must be wakers
                unsafe { std::mem::ManuallyDrop::drop(&mut self.state.get_mut().wakers) };
            } else if state == STATE_VALUE {
                // Safety: the state must be wakers
                unsafe { std::mem::ManuallyDrop::drop(&mut self.state.get_mut().value) };
            }
        }
    }

    mod get_impl {
        use super::*;
        use std::future::Future;
        use std::pin::Pin;
        use std::task::{Context, Poll};

        #[pin_project::pin_project]
        pub struct Get<'a, T, Fut> {
            once: &'a Once<T>,
            #[pin]
            to_poll: Fut,
            should_poll: bool,
        }

        impl<'a, T, Fut> Get<'a, T, Fut> {
            pub fn new(once: &'a Once<T>, to_poll: Fut) -> Self {
                Get {
                    once,
                    to_poll,
                    should_poll: false,
                }
            }
        }

        const MAX_SPINS: usize = 20;

        impl<'a, T, Fut> Future for Get<'a, T, Fut>
        where
            Fut: Future<Output = T> + Send,
        {
            type Output = &'a T;

            fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
                let mut proj = self.project();
                let mut spins = 0;
                loop {
                    if *proj.should_poll {
                        match proj.to_poll.as_mut().poll(cx) {
                            Poll::Pending => break Poll::Pending,
                            Poll::Ready(value) => {
                                // Set the state to STATE_NONE
                                proj.once.tag.fetch_and(!STATE_MASK, Ordering::Acquire);
                                // Wait for nothing to be accessing the wakers
                                while proj.once.tag.load(Ordering::Acquire) & USING_WAKERS != 0 {
                                    // Should be a fairly short wait.
                                    spins += 1;
                                    if spins > MAX_SPINS {
                                        std::thread::yield_now();
                                    } else {
                                        std::hint::spin_loop();
                                    }
                                }
                                // Safety: the state must be wakers, not accessed by anything else
                                let state =
                                    unsafe { proj.once.state.get().as_mut().unwrap_unchecked() };
                                let wakers =
                                    unsafe { std::mem::ManuallyDrop::take(&mut state.wakers) };
                                state.value = std::mem::ManuallyDrop::new(value);
                                proj.once.tag.fetch_or(STATE_VALUE, Ordering::Release);
                                for waker in wakers {
                                    waker.into_waker().wake();
                                }
                            }
                        }
                    }

                    let state = proj.once.tag.fetch_or(ACCESS_ACCESSED, Ordering::Acquire);
                    if state & ACCESS_MASK == ACCESS_NOT_ACCESSED {
                        // Safety: if unevaluated and unaccessed, the state is empty
                        debug_assert!(
                            proj.once.tag.load(Ordering::Relaxed) & STATE_MASK == STATE_NONE
                        );
                        let state = unsafe { proj.once.state.get().as_mut().unwrap_unchecked() };
                        state.wakers = std::mem::ManuallyDrop::new(RVec::new());
                        proj.once.tag.fetch_or(STATE_WAKERS, Ordering::Release);
                        *proj.should_poll = true;
                    } else {
                        debug_assert!(state & ACCESS_MASK == ACCESS_ACCESSED);
                        if state & STATE_MASK == STATE_VALUE {
                            // Safety: the state must be `value`
                            break Poll::Ready(unsafe {
                                &*proj.once.state.get().as_ref().unwrap_unchecked().value
                            });
                        } else {
                            if proj
                                .once
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
                                    unsafe { proj.once.state.get().as_mut().unwrap_unchecked() };
                                unsafe { &mut state.wakers }.push(waker);
                                proj.once.tag.fetch_and(!USING_WAKERS, Ordering::Release);
                                break Poll::Pending;
                            } else {
                                // There are three cases:
                                // 1. initial access, which is pretty quick (creating RVec for wakers).
                                // 2. wakers -> value transition, which is pretty quick (moving RVec, moving value).
                                // 3. USING_WAKERS set, which will usually be fast though the vector growing may be slow.
                                spins += 1;
                                if spins > MAX_SPINS {
                                    std::thread::yield_now();
                                } else {
                                    std::hint::spin_loop();
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub use get_impl::Get;
}

pub use once::Once;

pub enum VisitInfo<'a> {
    Value(&'a Value),
    Immutable(&'a Value),
    Discriminant(u8),
}

impl crate::dependency::AsDependency for VisitInfo<'_> {
    fn as_dependency(&self) -> Dependency {
        match self {
            VisitInfo::Value(v) | VisitInfo::Immutable(v) => Dependency::Value((*v).clone()),
            VisitInfo::Discriminant(c) => Dependency::Constant(c.into()),
        }
    }
}

/// A trait to apply a function over inner values of a type.
///
/// This trait is unsafe for two reasons:
/// 1. The default implementation of `visit_mut`: it uses the implementation of `visit_info`, so
///    the `VisitInfo::Value` values that `visit_info` provides must all be safe to be modified,
///    and must all reside within the type (to maintain aliasing guarantees). So for instance, if
///    the implementation of `visit_info` calls `f` on a `VisitInfo::Value` static Value reference
///    (that is not within the type), or if it calls it on values that shouldn't be modified (such
///    as keys in a map; this is what `VisitInfo::Immutable` is for), an alternate `visit_mut` must
///    be provided.
/// 2. The default implementations of `visit_info` and `visit`: they are mutually recursive (for
///    implementation convenience), so at least one of `visit_info` or `visit` must be provided.
pub unsafe trait InnerValues {
    fn visit_info<'a, F: FnMut(VisitInfo<'a>)>(&'a self, mut f: F) {
        self.visit(move |v| f(VisitInfo::Value(v)))
    }

    fn visit<'a, F: FnMut(&'a Value)>(&'a self, mut f: F) {
        self.visit_info(move |v| match v {
            VisitInfo::Value(v) | VisitInfo::Immutable(v) => f(v),
            _ => (),
        })
    }

    fn visit_mut<'a, F: FnMut(&'a mut Value)>(&'a mut self, mut f: F) {
        self.visit_info(move |v| match v {
            VisitInfo::Value(v) => f(unsafe {
                (v as *const Value as *mut Value)
                    .as_mut()
                    .unwrap_unchecked()
            }),
            _ => (),
        })
    }
}

/// A trait to apply late bindings to a type.
pub trait LateBind {
    fn late_bind(&mut self, scope: &LateScope);
    fn late_bound(&self) -> LateBound;
}

#[derive(Clone)]
#[repr(transparent)]
struct TypedValueImpl<T>(T);

impl<T: ErgoType + InnerValues + Clone + Send + Sync + 'static> ValueDataInterface
    for TypedValueImpl<T>
{
    fn id(&self) -> future::BoxFuture<IdInfo<U128>> {
        future::BoxFuture::new(async move {
            let mut info = Vec::new();
            self.0.visit_info(|v| info.push(v));
            crate::depends![dyn T::ergo_type(), ^@info]
                .id()
                .await
                .into()
        })
    }

    fn late_bind(&mut self, scope: &LateScope) {
        self.0.visit_mut(|v| v.late_bind(scope));
    }

    fn late_bound(&self) -> LateBound {
        let mut ret = LateBound::new();
        self.0.visit(|v| ret.extend(v.late_bound().clone()));
        ret
    }

    fn get(&self) -> ValueType {
        ValueType::typed(&self.0)
    }
}

#[derive(Clone)]
#[repr(transparent)]
struct ConstantValueImpl<T>(T);

impl<T: ErgoType + std::hash::Hash> ConstantValueImpl<T> {
    pub fn id(&self) -> Identity {
        use std::hash::Hash;
        let mut hfn = HashFn::default();
        T::ergo_type().hash(&mut hfn);
        self.0.hash(&mut hfn);
        Identity::new(hfn.finish_ext())
    }
}

impl<T: ErgoType + std::hash::Hash + Clone + Send + Sync + 'static> ValueDataInterface
    for ConstantValueImpl<T>
{
    fn id(&self) -> future::BoxFuture<IdInfo<U128>> {
        future::BoxFuture::new(async move { self.id().into() })
    }

    fn late_bind(&mut self, _scope: &LateScope) {}

    fn late_bound(&self) -> LateBound {
        Default::default()
    }

    fn get(&self) -> ValueType {
        ValueType::typed(&self.0)
    }
}

#[repr(transparent)]
pub struct BoxedLazyValueId(Box<dyn LazyValueId>);

mod lazy_value_id_clone {
    pub trait LazyValueIdClone {
        fn do_clone(&self) -> super::BoxedLazyValueId;
    }

    impl<T: Clone + super::LazyValueId> LazyValueIdClone for T {
        fn do_clone(&self) -> super::BoxedLazyValueId {
            super::BoxedLazyValueId(Box::new(self.clone()))
        }
    }
}

impl Clone for BoxedLazyValueId {
    fn clone(&self) -> Self {
        self.0.do_clone()
    }
}

#[derive(Clone)]
pub enum ValueId {
    Id(Identity),
    Lazy(BoxedLazyValueId),
    Override {
        id: BoxedLazyValueId,
        eval_for_id: bool,
    },
}

pub trait LazyValueId:
    LateBind + lazy_value_id_clone::LazyValueIdClone + Send + Sync + 'static
{
    fn id(&self) -> futures::future::BoxFuture<Identity>;
}

impl LateBind for Value {
    fn late_bind(&mut self, scope: &LateScope) {
        Value::late_bind(self, scope);
    }

    fn late_bound(&self) -> LateBound {
        self.inner.late_bound().clone()
    }
}

impl LazyValueId for Value {
    fn id(&self) -> futures::future::BoxFuture<Identity> {
        futures::FutureExt::boxed(async move { self.clone().eval_id().await })
    }
}

impl LateBind for Dependencies {
    fn late_bind(&mut self, scope: &LateScope) {
        self.map_mut(|d| match d {
            Dependency::Value(v) => v.late_bind(scope),
            Dependency::Constant(_) => (),
        })
    }

    fn late_bound(&self) -> LateBound {
        let mut s = LateBound::default();
        self.map(|d| match d {
            Dependency::Value(v) => s.extend(v.late_bound().clone()),
            Dependency::Constant(_) => (),
        });
        s
    }
}

impl LazyValueId for Dependencies {
    fn id(&self) -> futures::future::BoxFuture<Identity> {
        futures::FutureExt::boxed(Self::id(self))
    }
}

impl From<DependenciesConstant> for ValueId {
    fn from(deps: DependenciesConstant) -> Self {
        ValueId::Id(deps.id())
    }
}

impl From<u128> for ValueId {
    fn from(i: u128) -> Self {
        ValueId::Id(Identity::new(i))
    }
}

impl<T: Into<ValueId>> From<IdInfo<T>> for ValueId {
    fn from(IdInfo { id, eval_for_id }: IdInfo<T>) -> Self {
        let id = id.into();
        match id {
            ValueId::Id(mut id) => {
                id.eval_for_id = eval_for_id;
                ValueId::Id(id)
            }
            ValueId::Lazy(id) | ValueId::Override { id, .. } => {
                ValueId::Override { id, eval_for_id }
            }
        }
    }
}

impl<T: LazyValueId> From<T> for ValueId {
    fn from(v: T) -> Self {
        ValueId::Lazy(BoxedLazyValueId(Box::new(v)))
    }
}

impl ValueId {
    pub async fn id(&self) -> Identity {
        match self {
            ValueId::Id(id) => id.clone(),
            ValueId::Lazy(l) => l.0.id().await,
            ValueId::Override { id, eval_for_id } => {
                let mut result = id.0.id().await;
                result.eval_for_id = *eval_for_id;
                result
            }
        }
    }

    pub fn late_bind(&mut self, scope: &LateScope) {
        match self {
            ValueId::Id(_) => (),
            ValueId::Lazy(l) => l.0.late_bind(scope),
            ValueId::Override { id, .. } => id.0.late_bind(scope),
        }
    }

    pub fn late_bound(&self) -> LateBound {
        match self {
            ValueId::Id(_) => Default::default(),
            ValueId::Lazy(l) => l.0.late_bound(),
            ValueId::Override { id, .. } => id.0.late_bound(),
        }
    }
}

#[derive(Clone)]
#[repr(C)]
struct IdValueData<T> {
    value_data: T,
    id: ValueId,
}

impl<T: ValueDataInterface> ValueDataInterface for IdValueData<T> {
    fn id<'a>(&'a self) -> future::BoxFuture<'a, IdInfo<U128>> {
        future::BoxFuture::new(async move { self.id.id().await.into() })
    }

    fn late_bind(&mut self, scope: &LateScope) {
        self.value_data.late_bind(scope);
        self.id.late_bind(scope);
    }

    fn late_bound(&self) -> LateBound {
        let mut s = self.value_data.late_bound();
        s.extend(self.id.late_bound());
        s
    }

    fn get(&self) -> ValueType {
        self.value_data.get()
    }

    fn eval_for_id_hint(&self) -> future::BoxFuture<bool> {
        future::BoxFuture::new(async move {
            match self.id {
                ValueId::Override { id: _, eval_for_id } => eval_for_id,
                _ => false,
            }
        })
    }
}

/// Lazy value helpers.
pub mod lazy {
    use super::*;

    pub struct CapturesWitness<T: ?Sized>(std::marker::PhantomData<T>);

    impl<T> Default for CapturesWitness<T> {
        #[inline]
        fn default() -> Self {
            CapturesWitness(Default::default())
        }
    }

    impl<T> Clone for CapturesWitness<T> {
        #[inline]
        fn clone(&self) -> Self {
            CapturesWitness(self.0.clone())
        }
    }

    impl<T> Copy for CapturesWitness<T> {}

    impl<T> CapturesWitness<T> {
        #[inline]
        pub fn witness(v: T) -> (T, Self) {
            (v, Default::default())
        }

        #[inline]
        pub fn check(self, v: T) -> T {
            v
        }
    }

    #[derive(Clone)]
    pub struct LazyValueFn<Captures, F, const USE_CAPTURES_IN_ID: bool = true> {
        id: ValueId,
        captures: Captures,
        f: F,
    }

    impl<const U: bool, Captures, F> LazyValueFn<Captures, F, U> {
        pub fn new(id: impl Into<ValueId>, captures: Captures, f: F) -> Self {
            LazyValueFn {
                id: id.into(),
                captures,
                f,
            }
        }
    }

    pub trait LazyCaptures: Clone + Send + Sync + 'static {
        /// Get the identity of the captures.
        fn id(&self) -> futures::future::BoxFuture<Identity>;

        /// Late-bind the scope into the captures.
        fn late_bind(&mut self, scope: &LateScope);

        /// Late-bound keys in the captures.
        fn late_bound(&self) -> LateBound;

        /// Whether the value should be evaluated to get the identity.
        fn eval_for_id_hint(&self) -> futures::future::BoxFuture<bool> {
            futures::FutureExt::boxed(async { false })
        }
    }

    impl LazyCaptures for () {
        fn id(&self) -> futures::future::BoxFuture<Identity> {
            futures::FutureExt::boxed(async move { Identity::new(0) })
        }

        fn late_bind(&mut self, _scope: &LateScope) {}

        fn late_bound(&self) -> LateBound {
            Default::default()
        }
    }

    impl LazyCaptures for Value {
        fn id(&self) -> futures::future::BoxFuture<Identity> {
            futures::FutureExt::boxed(async move { self.clone().eval_id().await })
        }

        fn late_bind(&mut self, scope: &LateScope) {
            Value::late_bind(self, scope);
        }

        fn late_bound(&self) -> LateBound {
            self.inner.late_bound().clone()
        }

        fn eval_for_id_hint(&self) -> futures::future::BoxFuture<bool> {
            futures::FutureExt::boxed(self.inner.data.eval_for_id_hint())
        }
    }

    impl<T: Send + Sync + 'static> LazyCaptures for TypedValue<T> {
        fn id(&self) -> futures::future::BoxFuture<Identity> {
            LazyCaptures::id(&self.inner)
        }

        fn late_bind(&mut self, scope: &LateScope) {
            LazyCaptures::late_bind(&mut self.inner, scope);
        }

        fn late_bound(&self) -> LateBound {
            LazyCaptures::late_bound(&self.inner)
        }

        fn eval_for_id_hint(&self) -> futures::future::BoxFuture<bool> {
            LazyCaptures::eval_for_id_hint(&self.inner)
        }
    }

    impl<T: LazyCaptures> LazyCaptures for Vec<T> {
        fn id(&self) -> futures::future::BoxFuture<Identity> {
            futures::FutureExt::boxed(async move {
                let mut combiner = IdentityCombiner::default();
                for c in self {
                    combiner.add(&c.id().await);
                }
                combiner.finish()
            })
        }

        fn late_bind(&mut self, scope: &LateScope) {
            for c in self {
                c.late_bind(scope);
            }
        }

        fn late_bound(&self) -> LateBound {
            let mut s = LateBound::default();
            for c in self {
                s.extend(c.late_bound());
            }
            s
        }

        fn eval_for_id_hint(&self) -> futures::future::BoxFuture<bool> {
            futures::FutureExt::boxed(async move {
                for c in self {
                    if c.eval_for_id_hint().await {
                        return true;
                    }
                }
                false
            })
        }
    }

    macro_rules! impl_tuple {
        ( $( $name:ident )+ ) => {
            impl<$($name: LazyCaptures),+> LazyCaptures for ($($name,)+) {
                fn id(&self) -> futures::future::BoxFuture<Identity> {
                    futures::FutureExt::boxed(async move {
                        #[allow(non_snake_case)]
                        let ($($name,)+) = self;
                        let mut combiner = IdentityCombiner::default();
                        $(combiner.add(&$name.id().await);)+
                        combiner.finish()
                    })
                }

                fn late_bind(&mut self, scope: &LateScope) {
                    #[allow(non_snake_case)]
                    let ($($name,)+) = self;
                    $($name.late_bind(scope);)+
                }

                fn late_bound(&self) -> LateBound {
                    #[allow(non_snake_case)]
                    let ($($name,)+) = self;
                    let mut s = LateBound::default();
                    $(s.extend($name.late_bound());)+
                    s
                }

                fn eval_for_id_hint(&self) -> futures::future::BoxFuture<bool> {
                    futures::FutureExt::boxed(async move {
                        #[allow(non_snake_case)]
                        let ($($name,)+) = self;
                        $(if $name.eval_for_id_hint().await { return true; })+
                        false
                    })
                }
            }
        }
    }

    impl_tuple! { A }
    impl_tuple! { A B }
    impl_tuple! { A B C }
    impl_tuple! { A B C D }
    impl_tuple! { A B C D E }
    impl_tuple! { A B C D E F }
    impl_tuple! { A B C D E F G }
    impl_tuple! { A B C D E F G H }
    impl_tuple! { A B C D E F G H I }
    impl_tuple! { A B C D E F G H I J }

    // `FnOnce + Clone` is used rather than `Fn` because this is more convenient for callers when
    // moving values into the returned Future.

    impl<const B: bool, Captures, F, Fut> LazyValueData for LazyValueFn<Captures, F, B>
    where
        Self: Send + Sync + 'static,
        Captures: Clone,
        F: FnOnce(Captures) -> Fut + Clone,
        Fut: std::future::Future<Output = Value> + Send + 'static,
    {
        fn next(&self) -> future::BoxFuture<Value> {
            future::BoxFuture::new((self.f.clone())(self.captures.clone()))
        }
    }

    impl<const USE_CAPS: bool, Captures, F> ValueDataInterface for LazyValueFn<Captures, F, USE_CAPS>
    where
        Captures: LazyCaptures,
        Self: LazyValueData + Clone + 'static,
    {
        fn id<'a>(&'a self) -> future::BoxFuture<'a, IdInfo<U128>> {
            future::BoxFuture::new(async move {
                if USE_CAPS {
                    let mut combiner = IdentityCombiner::default();
                    combiner.add(&self.id.id().await);
                    combiner.add(&self.captures.id().await);
                    combiner.finish().into()
                } else {
                    self.id.id().await.into()
                }
            })
        }

        fn late_bind(&mut self, scope: &LateScope) {
            self.id.late_bind(scope);
            self.captures.late_bind(scope);
        }

        fn late_bound(&self) -> LateBound {
            let mut s = self.id.late_bound();
            s.extend(self.captures.late_bound());
            s
        }

        fn get(&self) -> ValueType {
            ValueType::lazy(self)
        }

        fn eval_for_id_hint(&self) -> future::BoxFuture<bool> {
            future::BoxFuture::new(self.captures.eval_for_id_hint())
        }
    }
}

impl Value {
    /// Create a new Value from anything implementing ValueDataInterface.
    pub fn new<T: ValueDataInterface + 'static>(value_data: T) -> Self {
        Value {
            inner: RArc::new(Inner::new(ValueData::new(value_data))),
            metadata: Default::default(),
        }
    }

    /// Create a typed value.
    ///
    /// This is intended for types which only contain other values (as the identity will only be
    /// based on the type and the inner values).
    pub fn typed<T: ErgoType + InnerValues + Clone + Eraseable>(value: T) -> Self {
        Self::new(TypedValueImpl(value))
    }

    /// Create a constant (typed, identified) value.
    ///
    /// This is intended for types which do not contain any values.
    pub fn constant<T: ErgoType + std::hash::Hash + Clone + Eraseable>(value: T) -> Self {
        let value_data = ConstantValueImpl(value);
        let id = value_data.id();
        let mut inner = Inner::new(ValueData::new(value_data));
        inner.id.set(id.into());
        Value {
            inner: RArc::new(inner),
            metadata: Default::default(),
        }
    }

    /// Create a typed value with the given identity.
    pub fn with_id<T: ErgoType + InnerValues + Clone + Eraseable, D>(value: T, id: D) -> Self
    where
        D: Into<ValueId>,
    {
        let mut inner = Inner::new(ValueData::new(TypedValueImpl(value)));
        inner.set_id(id);
        Value {
            inner: RArc::new(inner),
            metadata: Default::default(),
        }
    }

    /// Get the value's immediate identity.
    pub async fn immediate_id(&self) -> Identity {
        self.inner.id().await
    }

    /// Try to get the value's immediate identity, if available.
    pub fn try_immediate_id(&self) -> Option<&u128> {
        self.inner.id.try_get().map(|i| i.id.as_ref())
    }

    /// Get the value's identity, evaluating the value as necessary to get a more accurate
    /// identity.
    pub async fn eval_id(&mut self) -> Identity {
        let ctx = Context::global();
        let _token = ctx.progress.attempt_progress();
        loop {
            if !self.inner.data.eval_for_id_hint().await {
                let id = self.immediate_id().await;
                if !id.eval_for_id {
                    break;
                }
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
        self.inner.as_ref() as *const Inner as usize
    }

    /// Get the value's type, if evaluated.
    pub fn ergo_type(&self) -> Option<Type> {
        self.inner.data.ergo_type()
    }

    /// Return whether this value is evaluated (and has a type and data immediately available).
    pub fn is_evaluated(&self) -> bool {
        self.inner.data.is_typed()
    }

    /// Check whether this value has the given type.
    pub fn is_type<T: ErgoType>(&self) -> bool {
        match self.ergo_type() {
            Some(tp) if tp == T::ergo_type() => true,
            _ => false,
        }
    }

    /// Get this value as the given type, if evaluated to that type.
    pub fn as_type<T: ErgoType>(self) -> Result<TypedValue<T>, Self> {
        if self.is_type::<T>() {
            Ok(unsafe { TypedValue::from_value(self) })
        } else {
            Err(self)
        }
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
        if self.is_type::<T>() {
            Some(unsafe { self.inner.data.as_ref::<T>() })
        } else {
            None
        }
    }

    /// Get this value as the given mutable type, if evaluated to that type and no other references
    /// exist.
    pub fn as_mut<T: ErgoType>(&mut self) -> Option<&mut T> {
        if self.is_type::<T>() {
            if let Some(inner) = RArc::get_mut(&mut self.inner) {
                return Some(unsafe { inner.data.as_mut::<T>() });
            }
        }
        None
    }

    /// Get a pointer to the data contained in this value (if typed).
    pub fn data_ptr(&self) -> Option<*const ()> {
        if self.is_evaluated() {
            Some(unsafe { self.inner.data.as_ptr() })
        } else {
            None
        }
    }

    /// Set the identity of this value.
    pub fn set_identity<D>(&mut self, id: D)
    where
        D: Into<ValueId>,
    {
        RArc::make_mut(&mut self.inner).set_id(id);
    }

    /// Set the value as impure. This will prevent `next` values from being cached (calling `next`
    /// every time a value is needed).
    ///
    /// Calling this (even with `impure` set to false) will clear any existing cached result.
    pub fn impure(&mut self, impure: bool) {
        RArc::make_mut(&mut self.inner).next = if impure {
            ROption::RNone
        } else {
            ROption::RSome(Default::default())
        };
    }

    /// Late-bind the given scope in this value.
    pub fn late_bind(&mut self, scope: &LateScope) {
        if self.late_bound().bound_by(scope) {
            RArc::make_mut(&mut self.inner).late_bind(scope);
        }
    }

    /// Get the late bindings for this value.
    pub fn late_bound(&self) -> &LateBound {
        self.inner.late_bound()
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
    ///
    /// Returns whether the value was evaluated (and changed).
    pub async fn eval_once(&mut self) -> bool {
        if !self.is_evaluated() {
            let Value { inner, metadata } = self.inner.next().await;
            self.inner = inner;
            self.metadata.extend(metadata);
            true
        } else {
            false
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

impl<T: ErgoType + Clone + Eraseable> TypedValue<T> {
    /// Create a typed value.
    pub fn new(data: T) -> Self
    where
        T: InnerValues,
    {
        unsafe { TypedValue::from_value(Value::typed(data)) }
    }

    /// Create a value with constant dependencies.
    pub fn constant(data: T) -> Self
    where
        T: std::hash::Hash,
    {
        unsafe { TypedValue::from_value(Value::constant(data)) }
    }

    /// Create a value with the given identity.
    pub fn with_id<D: Into<ValueId>>(data: T, id: D) -> Self
    where
        T: InnerValues,
    {
        unsafe { TypedValue::from_value(Value::with_id(data, id)) }
    }

    /// Extract the Value's type as an owned value.
    pub fn into_owned(self) -> T {
        match RArc::try_unwrap(self.inner.inner) {
            Ok(inner) => unsafe { inner.data.into_owned() },
            Err(arc) => unsafe { arc.data.as_ref::<T>() }.clone(),
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
        unsafe { self.inner.inner.data.as_ref::<T>() }
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

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Value")
            .field(
                "metadata",
                &self.metadata.iter().map(|(k, _)| k).collect::<Vec<_>>(),
            )
            .field("id", &self.inner.id.try_get())
            .field(
                "next",
                &self.inner.next.as_ref().map(|v| v.try_get().is_some()),
            )
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
    T: std::hash::Hash + IntoValue,
{
    fn from(v: T) -> IdentifiedValue {
        let val = v.into_value();
        assert!(val.try_immediate_id().is_some());
        IdentifiedValue(val)
    }
}

impl<T> From<T> for EvaluatedValue
where
    T: std::hash::Hash + IntoValue,
{
    fn from(v: T) -> EvaluatedValue {
        let val = v.into_value();
        assert!(val.is_evaluated());
        assert!(val.try_immediate_id().is_some());
        EvaluatedValue(IdentifiedValue(val))
    }
}
