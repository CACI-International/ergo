//! The ABI-stable futures within values.

use abi_stable::{sabi_trait, sabi_trait::prelude::*, std_types::RBox, StableAbi};
use futures::{future, future::FutureExt};
use std::pin::Pin;
use std::task;

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
    let r = (v as *const RawWaker).as_ref().unwrap();
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
    let waker = unsafe { (v as *const Option<task::Waker>).as_ref() }.expect("waker state invalid");
    RawWaker {
        data: Box::into_raw(Box::new(waker.clone())) as *const (),
        vtable: &INTERNAL_RAW_WAKER_VTABLE,
    }
}

extern "C" fn internal_waker_wake(v: *const ()) {
    let waker = unsafe { (v as *mut Option<task::Waker>).as_mut() }.expect("waker state invalid");
    waker.take().expect("waker state invalid").wake()
}

extern "C" fn internal_waker_wake_by_ref(v: *const ()) {
    let waker = unsafe { (v as *const Option<task::Waker>).as_ref() }.expect("waker state invalid");
    waker.as_ref().expect("waker state invalid").wake_by_ref()
}

extern "C" fn internal_waker_drop(v: *const ()) {
    unsafe { Box::from_raw(v as *mut Option<task::Waker>) };
}

const INTERNAL_RAW_WAKER_VTABLE: RawWakerVTable = RawWakerVTable {
    clone: internal_waker_clone,
    wake: internal_waker_wake,
    wake_by_ref: internal_waker_wake_by_ref,
    drop: internal_waker_drop,
};

#[derive(Debug, StableAbi)]
#[repr(C)]
struct RawWaker {
    data: *const (),
    vtable: &'static RawWakerVTable,
}

impl From<RawWaker> for task::RawWaker {
    fn from(f: RawWaker) -> Self {
        task::RawWaker::new(Box::into_raw(Box::new(f)) as *const (), &RAW_WAKER_VTABLE)
    }
}

#[derive(Debug, StableAbi)]
#[repr(C)]
struct Context(RawWaker);

impl From<&mut task::Context<'_>> for RawWaker {
    fn from(cx: &mut task::Context) -> Self {
        RawWaker {
            data: Box::into_raw(Box::new(Some(cx.waker().clone()))) as *const (),
            vtable: &INTERNAL_RAW_WAKER_VTABLE,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableAbi)]
#[repr(u8)]
enum Poll<T> {
    Ready(T),
    Pending,
}

impl<T> Poll<T> {
    fn into_poll(self) -> task::Poll<T> {
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
trait Future: Send {
    type Output;

    #[sabi(last_prefix_field)]
    fn poll(&mut self, cx: Context) -> Poll<Self::Output>;
}

#[sabi_trait]
trait SharedFuture: Clone + Debug + Send + Sync {
    type Output;

    fn poll(&mut self, cx: Context) -> Poll<Self::Output>;

    fn is_terminated(&self) -> bool;

    #[sabi(last_prefix_field)]
    fn peek(&self) -> Option<&Self::Output>;
}

/// A boxed, ABI-stable future.
#[derive(StableAbi)]
#[repr(C)]
pub struct BoxFuture<T> {
    inner: Future_TO<'static, RBox<()>, T>,
}

impl<T: StableAbi> BoxFuture<T> {
    /// Create a new boxed future.
    pub fn new<Fut: future::Future<Output = T> + Send + 'static>(f: Fut) -> Self {
        BoxFuture {
            inner: Future_TO::from_value(f, TU_Opaque),
        }
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
        // Safe to use Pin::new_unchecked because these futures will _only_ be within a Box (and
        // are moved into the box), so we guarantee that the future will not be moved out.
        future::Future::poll(unsafe { Pin::new_unchecked(self) }, &mut ctx).into()
    }
}

impl<T> future::Future for BoxFuture<T> {
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
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct BoxSharedFuture<T> {
    inner: SharedFuture_TO<'static, RBox<()>, T>,
}

impl<T: StableAbi + Clone + Send + Sync> BoxSharedFuture<T> {
    /// Create a new boxed shared future.
    pub fn new<Fut: future::Future<Output = T> + Send + 'static>(f: Fut) -> Self {
        BoxSharedFuture {
            inner: SharedFuture_TO::from_value(f.shared(), TU_Opaque),
        }
    }
}

impl<T> BoxSharedFuture<T> {
    /// Returns a reference to the future's value if it has been evaluated.
    pub fn peek(&self) -> Option<&T> {
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

    fn is_terminated(&self) -> bool {
        future::FusedFuture::is_terminated(self)
    }

    fn peek(&self) -> Option<&Self::Output> {
        self.peek()
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

impl<T> future::FusedFuture for BoxSharedFuture<T> {
    fn is_terminated(&self) -> bool {
        self.inner.is_terminated()
    }
}
