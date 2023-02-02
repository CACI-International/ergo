//! Runtime garbage collector.

use crate::gc;
use abi_stable::{std_types::RArc, StableAbi};
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};

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
    done: RArc<AtomicBool>,
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
        std::thread::spawn(move || {
            while !done_token.load(Ordering::Relaxed) {
                std::thread::sleep(std::time::Duration::from_secs(1));
                //collector.collect();
            }
            collector.collect();
        });
        GarbageCollector { pile, done }
    }

    #[inline]
    pub fn create<T: gc::GcRefs>(value: T) -> gc::Gc<T> {
        GarbageCollectorImpl::new(value)
    }
}

impl Drop for GarbageCollector {
    fn drop(&mut self) {
        self.done.store(true, Ordering::Relaxed);
    }
}

/// An active garbage scope.
///
/// Anything added to this scope will be rooted until the entire scope is no longer referenced.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct GarbageScope {
    list: RArc<gc::GcScopeRoot<GarbageScopeList>>,
}

impl Default for GarbageScope {
    fn default() -> Self {
        GarbageScope {
            list: RArc::new(gc::GcScopeRoot::new(&GarbageCollector::create(
                GarbageScopeList::default(),
            ))),
        }
    }
}

impl GarbageScope {
    pub(crate) fn push(&self, v: gc::Gc<crate::value::Inner>) {
        self.list.push(v);
    }

    /// Add the given value to the garbage scope.
    pub fn add(&self, v: &crate::Value) {
        self.list.add(v);
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
