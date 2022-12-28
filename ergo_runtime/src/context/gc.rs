//! Runtime GC.

use crate::gc;
use abi_stable::{std_types::RArc, StableAbi};
use std::sync::atomic::{AtomicBool, Ordering};

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
