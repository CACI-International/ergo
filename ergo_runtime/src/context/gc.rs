//! Runtime garbage collector.

use crate::abi_stable::{
    closure::ClosureOnce,
    std_types::{RArc, ROption},
    StableAbi,
};
use crate::gc;
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::thread;
use std::time::{Duration, Instant as Clock};

/// The amount of time between garbage collections.
const GC_INTERVAL: Duration = Duration::from_secs(1);

#[derive(Default, Clone, Copy, StableAbi)]
#[repr(C)]
pub struct ContextPile;

pub type Pile = gc::atomic::Pile<ContextPile>;

pub type GarbageCollectorImpl = gc::GarbageCollector<Pile>;

impl gc::atomic::WithPile for ContextPile {
    fn with_pile<R, F: FnOnce(&Pile) -> R>(&self, f: F) -> Option<R> {
        super::CURRENT_CONTEXT.with(|opt_ctx| opt_ctx.map(|ctx| f(&ctx.global.gc.pile)))
    }
}

#[derive(StableAbi)]
#[repr(C)]
pub struct GarbageCollector {
    pile: Pile,
    done: ManuallyDrop<ClosureOnce<(), ()>>,
}

impl std::fmt::Debug for GarbageCollector {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("GarbageCollector").finish_non_exhaustive()
    }
}

impl GarbageCollector {
    pub fn new() -> Self {
        let done = RArc::new(AtomicBool::new(false));
        let done_token = done.clone();
        let mut collector: GarbageCollectorImpl = Default::default();
        let pile = collector.pile.clone();
        let handle = thread::Builder::new()
            .name("ergo_runtime gc".into())
            .spawn(move || {
                let mut next = Clock::now();
                while !done_token.load(Ordering::Relaxed) {
                    next += GC_INTERVAL;
                    thread::park_timeout(next - Clock::now());
                    let start = Clock::now();
                    collector.collect();
                    log::info!("GC collect took {:?}", start.elapsed());
                }
                collector.collect();
            })
            .expect("failed to create GC thread");
        let done = ClosureOnce::<(), ()>::new(move || {
            done.store(true, Ordering::Relaxed);
            handle.thread().unpark();
            if handle.join().is_err() {
                log::error!("GC thread panicked");
            }
        });
        GarbageCollector {
            pile,
            done: ManuallyDrop::new(done),
        }
    }

    #[inline]
    pub fn create<T: gc::GcRefs>(value: T) -> gc::Gc<T> {
        GarbageCollectorImpl::new(value)
    }
}

impl Drop for GarbageCollector {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::take(&mut self.done) }.call();
    }
}

/// An active garbage scope.
///
/// Anything added to this scope will be rooted until the entire scope is no longer referenced.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct GarbageScope {
    list: ROption<RArc<gc::GcScopeRoot<GarbageScopeList>>>,
}

impl Default for GarbageScope {
    fn default() -> Self {
        let list = GarbageCollector::create(GarbageScopeList::default());
        let ret = GarbageScope {
            list: ROption::RSome(RArc::new(gc::GcScopeRoot::new(&list))),
        };
        gc::Gc::track(&list);
        ret
    }
}

impl GarbageScope {
    pub(crate) fn empty() -> Self {
        GarbageScope {
            list: ROption::RNone,
        }
    }

    pub(crate) fn push(&self, v: gc::Gc<crate::value::Inner>) {
        if let ROption::RSome(list) = &self.list {
            list.push(v);
        }
    }

    /// Add the given value to the garbage scope.
    pub fn add(&self, v: &crate::Value) {
        if let ROption::RSome(list) = &self.list {
            list.add(v);
        }
    }
}

impl std::fmt::Debug for GarbageScope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("GarbageScope").finish_non_exhaustive()
    }
}

#[derive(Default, StableAbi)]
#[repr(C)]
struct GarbageScopeList {
    head: AtomicPtr<GarbageScopeValue>,
}

#[derive(StableAbi)]
#[repr(C)]
struct GarbageScopeValue {
    v: gc::Gc<crate::value::Inner>,
    next: AtomicPtr<Self>,
}

impl gc::GcRefs for GarbageScopeList {
    fn gc_refs(&self, visitor: &mut gc::Visitor) {
        let mut cur = self.head.load(Ordering::Acquire);
        while !cur.is_null() {
            let val = unsafe { cur.as_ref().unwrap_unchecked() };
            visitor.visit(&val.v);
            cur = val.next.load(Ordering::Acquire);
        }
    }
}

impl GarbageScopeList {
    pub(crate) fn push(&self, v: gc::Gc<crate::value::Inner>) {
        let ptr = Box::into_raw(Box::new(GarbageScopeValue {
            v,
            next: Default::default(),
        }));
        let mut head = self.head.load(Ordering::Acquire);
        loop {
            unsafe { ptr.as_ref().unwrap_unchecked() }
                .next
                .store(head, Ordering::Release);
            match self
                .head
                .compare_exchange_weak(head, ptr, Ordering::AcqRel, Ordering::Acquire)
            {
                Ok(_) => break,
                Err(h) => {
                    head = h;
                }
            }
        }
    }

    /// Add the given value to the garbage scope.
    pub fn add(&self, v: &crate::Value) {
        self.push(unsafe { v.inner.clone() })
    }
}

impl Drop for GarbageScopeList {
    fn drop(&mut self) {
        let cur = self.head.get_mut();
        while !cur.is_null() {
            let mut val = unsafe { Box::from_raw(*cur) };
            *cur = *val.next.get_mut();
        }
    }
}
