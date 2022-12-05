//! The ABI-stable futures within values.

// Silences warnings on the unsafe methods in sabi_trait traits.
#![allow(unused_unsafe)]

use abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    sabi_types::RMut,
    std_types::{RBox, RSliceMut},
    StableAbi,
};
use futures::{future, future::FutureExt};
use std::collections::BTreeMap;
use std::pin::Pin;
use std::task;

pub mod eager;
pub mod stream;

#[derive(Debug, StableAbi)]
#[repr(C)]
struct RawWakerVTable {
    clone: extern "C" fn(*const ()) -> RawWaker,
    wake: extern "C" fn(*const ()),
    wake_by_ref: extern "C" fn(*const ()),
    drop: extern "C" fn(*const ()),
}

unsafe fn waker_clone(v: *const ()) -> task::RawWaker {
    let r = (v as *const RawWaker).as_ref().unwrap();
    (r.vtable.clone)(r.data).into()
}

unsafe fn waker_wake(v: *const ()) {
    let r = Box::from_raw(v as *mut RawWaker);
    (r.vtable.wake)(r.data)
}

unsafe fn waker_wake_by_ref(v: *const ()) {
    let r = (v as *const RawWaker).as_ref().unwrap();
    (r.vtable.wake_by_ref)(r.data)
}

unsafe fn waker_drop(v: *const ()) {
    let r = Box::from_raw(v as *mut RawWaker);
    (r.vtable.drop)(r.data)
}

const RAW_WAKER_VTABLE: task::RawWakerVTable =
    task::RawWakerVTable::new(waker_clone, waker_wake, waker_wake_by_ref, waker_drop);

extern "C" fn internal_waker_clone(v: *const ()) -> RawWaker {
    let waker = unsafe { (v as *const task::Waker).as_ref() }.expect("waker state invalid");
    RawWaker {
        data: Box::into_raw(Box::new(waker.clone())) as *const (),
        vtable: &INTERNAL_RAW_WAKER_VTABLE,
    }
}

extern "C" fn internal_waker_wake(v: *const ()) {
    let waker = unsafe { Box::from_raw(v as *mut task::Waker) };
    waker.wake()
}

extern "C" fn internal_waker_wake_by_ref(v: *const ()) {
    let waker = unsafe { (v as *const task::Waker).as_ref() }.expect("waker state invalid");
    waker.wake_by_ref()
}

extern "C" fn internal_waker_drop(v: *const ()) {
    unsafe { Box::from_raw(v as *mut task::Waker) };
}

const INTERNAL_RAW_WAKER_VTABLE: RawWakerVTable = RawWakerVTable {
    clone: internal_waker_clone,
    wake: internal_waker_wake,
    wake_by_ref: internal_waker_wake_by_ref,
    drop: internal_waker_drop,
};

#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct RawWaker {
    data: *const (),
    vtable: &'static RawWakerVTable,
}

impl From<RawWaker> for task::RawWaker {
    fn from(f: RawWaker) -> Self {
        task::RawWaker::new(Box::into_raw(Box::new(f)) as *const (), &RAW_WAKER_VTABLE)
    }
}

impl RawWaker {
    pub fn into_waker(self) -> task::Waker {
        unsafe { task::Waker::from_raw(self.into()) }
    }
}

// RawWaker is Send and Sync because the Waker from which it is created is Send and Sync.
unsafe impl Send for RawWaker {}
unsafe impl Sync for RawWaker {}

#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct Context(pub RawWaker);

impl Context {
    pub fn new(cx: &mut task::Context<'_>) -> Self {
        Context(cx.into())
    }

    pub fn into_raw_waker(self) -> task::RawWaker {
        self.0.into()
    }

    pub fn into_waker(self) -> task::Waker {
        self.0.into_waker()
    }
}

impl From<&mut task::Context<'_>> for RawWaker {
    fn from(cx: &mut task::Context) -> Self {
        RawWaker {
            data: Box::into_raw(Box::new(cx.waker().clone())) as *const (),
            vtable: &INTERNAL_RAW_WAKER_VTABLE,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableAbi)]
#[repr(u8)]
pub enum Poll<T> {
    Ready(T),
    Pending,
}

impl<T> Poll<T> {
    pub fn into_poll(self) -> task::Poll<T> {
        match self {
            Self::Ready(v) => task::Poll::Ready(v),
            Self::Pending => task::Poll::Pending,
        }
    }
}

impl<T> From<task::Poll<T>> for Poll<T> {
    fn from(v: task::Poll<T>) -> Self {
        match v {
            task::Poll::Ready(v) => Poll::Ready(v),
            task::Poll::Pending => Poll::Pending,
        }
    }
}

impl<T> From<Poll<T>> for task::Poll<T> {
    fn from(v: Poll<T>) -> Self {
        v.into_poll()
    }
}

#[sabi_trait]
trait LocalFuture {
    type Output;

    #[sabi(last_prefix_field)]
    fn poll(&mut self, cx: Context) -> Poll<Self::Output>;
}

#[sabi_trait]
trait Future: Send {
    type Output;

    #[sabi(last_prefix_field)]
    fn poll(&mut self, cx: Context) -> Poll<Self::Output>;
}

#[sabi_trait]
trait SharedFuture: Clone + Send + Sync {
    type Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output>;

    #[sabi(last_prefix_field)]
    fn peek(&self) -> Option<&Self::Output>;
}

pub mod shared {
    use std::future::Future;

    pub trait SharedFuture: Clone + Future
    where
        Self::Output: Clone,
    {
        fn peek(&self) -> Option<&Self::Output>;
    }

    /*
    pub trait SharedFutureExt: SharedFuture
    where
        Self::Output: Clone,
    {
        fn map<U, F>(self, f: F) -> Map<Self, F, U>
        where
            F: FnOnce(Self::Output) -> U,
        {
            Map(self, f, Arc::new(Mutex::new(None)))
        }
    }

    impl<T: SharedFuture> SharedFutureExt for T where T::Output: Clone {}

    #[pin_project::pin_project]
    #[derive(Clone)]
    pub struct Map<Fut, F, U>(#[pin] Fut, F, Arc<Mutex<Option<U>>>);

    impl<Fut, F, U> Future for Map<Fut, F, U>
    where
        Fut: Future,
        F: FnOnce(Fut::Output) -> U,
    {
        type Output = U;

        fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
            let proj = self.project();
            proj.0.poll(cx).map(|v| (proj.1)(v))
        }
    }

    impl<Fut, F: Clone, U: Clone> SharedFuture for Map<Fut, F, U>
    where
        Fut: SharedFuture,
        Fut::Output: Clone,
        F: FnOnce(Fut::Output) -> U,
    {
        fn peek(&self) -> Option<&Self::Output> {
            if let Ok(mut guard) = self.2.lock() {
                if guard.is_none() {
                    *guard = self.0.peek().map(|v| self.1(v.clone()));
                }

                guard.as_ref()
            } else {
                None
            }
        }
    }
    */
}

/// A boxed, ABI-stable local future.
///
/// Unlike `BoxFuture`, `Send` is not required.
#[derive(StableAbi)]
#[repr(C)]
pub struct LocalBoxFuture<'a, T> {
    inner: LocalFuture_TO<'a, RBox<()>, T>,
}

impl<'a, T> LocalBoxFuture<'a, T> {
    pub fn new<Fut: future::Future<Output = T> + 'a>(f: Fut) -> Self {
        LocalBoxFuture {
            inner: LocalFuture_TO::from_value(f, TD_Opaque),
        }
    }
}

impl<Fut> LocalFuture for Fut
where
    Fut: future::Future,
{
    type Output = <Fut as future::Future>::Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output> {
        let waker = unsafe { task::Waker::from_raw(cx.0.into()) };
        let mut ctx = task::Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these futures will _only_ be within a Box (and
        // are moved into the box), so we guarantee that the future will not be moved out.
        future::Future::poll(unsafe { Pin::new_unchecked(self) }, &mut ctx).into()
    }
}

impl<'a, T> future::Future for LocalBoxFuture<'a, T> {
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<Self::Output> {
        let me = &mut *self;
        me.inner.poll(Context(cx.into())).into()
    }
}

/// A boxed, ABI-stable future.
#[derive(StableAbi)]
#[repr(C)]
pub struct BoxFuture<'a, T> {
    inner: Future_TO<'a, RBox<()>, T>,
}

impl<'a, T> BoxFuture<'a, T> {
    /// Create a new boxed future.
    pub fn new<Fut: future::Future<Output = T> + Send + 'a>(f: Fut) -> Self {
        BoxFuture {
            inner: Future_TO::from_value(f, TD_Opaque),
        }
    }
}

pub trait Boxed<'a, T> {
    fn boxed(self) -> BoxFuture<'a, T>;
}

impl<'a, T: StableAbi, Fut> Boxed<'a, T> for Fut
where
    Fut: future::Future<Output = T> + Send + 'a,
{
    fn boxed(self) -> BoxFuture<'a, T> {
        BoxFuture::new(self)
    }
}

impl<Fut> Future for Fut
where
    Fut: future::Future + Send,
{
    type Output = <Fut as future::Future>::Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output> {
        let waker = unsafe { task::Waker::from_raw(cx.0.into()) };
        let mut ctx = task::Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these futures will _only_ be within a Box, so we
        // guarantee that the future will not be moved out.
        future::Future::poll(unsafe { Pin::new_unchecked(self) }, &mut ctx).into()
    }
}

impl<T> future::Future for BoxFuture<'_, T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<Self::Output> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
            .poll(Context(cx.into()))
            .into()
    }
}

/// A boxed, ABI-stable future for use with Values.
///
/// These futures resolve to a shared value, and thus are cloneable and support peeking at the
/// value.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct BoxSharedFuture<T> {
    inner: SharedFuture_TO<'static, RBox<()>, T>,
}

impl<T> std::fmt::Debug for BoxSharedFuture<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("BoxSharedFuture")
            .field("inner_ready", &self.inner.peek().is_some())
            .finish()
    }
}

impl<T: StableAbi + Clone + Send + Sync> BoxSharedFuture<T> {
    /// Create a new boxed shared future.
    pub fn new<Fut: future::Future<Output = T> + Send + 'static>(f: Fut) -> Self {
        BoxSharedFuture {
            inner: SharedFuture_TO::from_value(f.shared(), TD_Opaque),
        }
    }

    /// Create a new boxed shared future from an Eager.
    pub fn from_eager<Fut>(f: eager::Eager<Fut>) -> Self
    where
        Fut: future::Future<Output = T> + Send + 'static,
        T: Send + Sync + Clone + std::marker::Unpin,
    {
        BoxSharedFuture {
            inner: SharedFuture_TO::from_value(f.shared().into_future(), TD_Opaque),
        }
    }

    /// Create a new boxed shared future that is immediately ready.
    pub fn ready(v: T) -> Self
    where
        T: 'static,
    {
        BoxSharedFuture {
            inner: SharedFuture_TO::from_value(Ready(Some(v)), TD_Opaque),
        }
    }
}

pub struct ForEach<T, Key, GetKey, F, Fut> {
    inner: BoxSharedFuture<T>,
    get_key: GetKey,
    get_fut: F,
    stored: BTreeMap<Key, Pin<Box<Fut>>>,
}

// # Safety
// The `Fut` parameter is only ever accessed with a mutable reference, so it only requires Send for
// `ForEach` to be `Sync`.
unsafe impl<T: Send + Sync, Key: Send + Sync, GetKey: Send + Sync, F: Send + Sync, Fut: Send> Sync
    for ForEach<T, Key, GetKey, F, Fut>
{
}

impl<T: Clone, Key: Ord, GetKey: Clone, Fut, F: Clone> Clone for ForEach<T, Key, GetKey, F, Fut> {
    fn clone(&self) -> Self {
        ForEach {
            inner: self.inner.clone(),
            get_key: self.get_key.clone(),
            get_fut: self.get_fut.clone(),
            stored: Default::default(),
        }
    }
}

pub struct FutureResult(crate::type_erase::Erased);

pub type BoxFutureResult =
    Pin<Box<dyn futures::future::Future<Output = FutureResult> + 'static + Send + Sync>>;

impl<T> BoxSharedFuture<T> {
    /// Returns a reference to the future's value if it has been evaluated.
    pub fn peek(&self) -> Option<&T> {
        self.inner.peek()
    }

    /// Creates a BoxedSharedFuture that allows the value to be evaluated with some side effects
    /// for each instance.
    pub fn for_each<Key, GetKey, F, Fut>(self, key: GetKey, f: F) -> Self
    where
        Key: Ord + Send + Sync + 'static,
        GetKey: Fn() -> Key + Clone + Send + Sync + 'static,
        F: Fn(BoxFutureResult) -> Fut + Clone + Send + Sync + 'static,
        T: Clone + Send + Sync + 'static,
        Fut: futures::future::Future<Output = FutureResult> + Send + 'static,
    {
        BoxSharedFuture {
            inner: SharedFuture_TO::from_value(
                ForEach {
                    inner: self,
                    get_key: key,
                    get_fut: f,
                    stored: Default::default(),
                },
                TD_Opaque,
            ),
        }
    }
}

impl<T: Clone> BoxSharedFuture<T> {
    /// Convert the boxed shared future into an Eager.
    pub fn into_eager(self) -> eager::Eager<Self> {
        match self.peek() {
            Some(v) => eager::Eager::Ready(v.clone()),
            None => eager::Eager::Pending(self),
        }
    }
}

impl<T, Key, GetKey, F, Fut> SharedFuture for ForEach<T, Key, GetKey, F, Fut>
where
    Key: Ord + Send + Sync + 'static,
    GetKey: Fn() -> Key + Clone + Send + Sync + 'static,
    F: Fn(BoxFutureResult) -> Fut + Clone + Send + Sync,
    T: Clone + Send + Sync + 'static,
    Fut: futures::future::Future<Output = FutureResult> + Send,
{
    type Output = T;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output> {
        let key = (self.get_key)();
        let mut fut = match self.stored.remove(&key) {
            Some(v) => v,
            None => {
                let fut = Box::pin(
                    self.inner
                        .clone()
                        .map(|v| FutureResult(crate::type_erase::Erased::new(v))),
                );
                Box::pin((self.get_fut)(fut))
            }
        };

        let waker = unsafe { task::Waker::from_raw(cx.0.into()) };
        let mut ctx = task::Context::from_waker(&waker);
        let result = future::Future::poll(fut.as_mut(), &mut ctx)
            .map(|FutureResult(v)| unsafe { v.to_owned::<T>() });
        if result.is_pending() {
            self.stored.insert(key, fut);
        }
        result.into()
    }

    fn peek(&self) -> Option<&Self::Output> {
        self.inner.peek()
    }
}

impl<Fut> SharedFuture for future::Shared<Fut>
where
    Fut: future::Future + Send,
    <Fut as future::Future>::Output: Clone + Send + Sync,
{
    type Output = <Fut as future::Future>::Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output> {
        let waker = unsafe { task::Waker::from_raw(cx.0.into()) };
        let mut ctx = task::Context::from_waker(&waker);
        future::Future::poll(Pin::new(self), &mut ctx).into()
    }

    fn peek(&self) -> Option<&Self::Output> {
        self.peek()
    }
}

impl<Fut> SharedFuture for eager::IntoFuture<futures::future::Shared<Fut>>
where
    Fut: future::Future + Send,
    <Fut as future::Future>::Output: Clone + Send + Sync + std::marker::Unpin,
{
    type Output = Fut::Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output> {
        let waker = unsafe { task::Waker::from_raw(cx.0.into()) };
        let mut ctx = task::Context::from_waker(&waker);
        future::Future::poll(Pin::new(self), &mut ctx).into()
    }

    fn peek(&self) -> Option<&Self::Output> {
        if let Some(i) = &self.0 {
            match i {
                eager::Eager::Ready(v) => Some(v),
                eager::Eager::Pending(v) => v.peek(),
            }
        } else {
            None
        }
    }
}

#[derive(Clone)]
struct Shared<T>(T);

impl<T: shared::SharedFuture + Send + Sync + std::marker::Unpin> SharedFuture for Shared<T>
where
    T::Output: Clone,
{
    type Output = T::Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output> {
        let waker = unsafe { task::Waker::from_raw(cx.0.into()) };
        let mut ctx = task::Context::from_waker(&waker);
        future::Future::poll(Pin::new(&mut self.0), &mut ctx).into()
    }

    fn peek(&self) -> Option<&Self::Output> {
        shared::SharedFuture::peek(&self.0)
    }
}

#[derive(Clone)]
struct Ready<T>(Option<T>);

impl<T> SharedFuture for Ready<T>
where
    T: Clone + Send + Sync,
{
    type Output = T;

    fn poll(&mut self, _cx: Context) -> Poll<Self::Output> {
        Poll::Ready(self.0.take().expect("ready future polled more than once"))
    }

    fn peek(&self) -> Option<&Self::Output> {
        self.0.as_ref()
    }
}

impl<T> future::Future for BoxSharedFuture<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<Self::Output> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
            .poll(Context(cx.into()))
            .into()
    }
}

#[sabi_trait]
trait BufMut {
    fn remaining_mut(&self) -> usize;

    unsafe fn advance_mut(&mut self, cnt: usize);

    #[sabi(last_prefix_field)]
    fn bytes_mut<'a>(&'a mut self) -> RSliceMut<'a, u8>;
}

#[derive(StableAbi)]
#[repr(C)]
struct BufMutWrap<'a>(BufMut_TO<'a, RMut<'a, ()>>);

impl<'a> bytes::BufMut for BufMutWrap<'a> {
    fn remaining_mut(&self) -> usize {
        self.0.remaining_mut()
    }

    unsafe fn advance_mut(&mut self, cnt: usize) {
        self.0.advance_mut(cnt)
    }

    fn bytes_mut(&mut self) -> &mut [std::mem::MaybeUninit<u8>] {
        let r: &mut [u8] = self.0.bytes_mut().into();
        unsafe { std::mem::transmute(r) }
    }
}

impl<B: bytes::BufMut> BufMut for B {
    fn remaining_mut(&self) -> usize {
        bytes::BufMut::remaining_mut(self)
    }

    unsafe fn advance_mut(&mut self, cnt: usize) {
        bytes::BufMut::advance_mut(self, cnt)
    }

    fn bytes_mut(&mut self) -> RSliceMut<u8> {
        let r: RSliceMut<std::mem::MaybeUninit<u8>> = bytes::BufMut::bytes_mut(self).into();
        unsafe { std::mem::transmute(r) }
    }
}
