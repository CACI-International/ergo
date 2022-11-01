//! Concurrently garbage-collected pointers.
//!
//! The garbage collection scheme here allows for collection to occur concurrently at any time,
//! without halting changes to the reference graph. For this to work, there are a few invariants
//! that must be upheld when changing the graph (adding references):
//!
//! * A reference must only be cloned from a reference that is in the graph.
//! * A cloned reference must be added to the graph prior to the original reference being dropped.
//!
//! If either of the above are not upheld, it's possible that multiple references are created, one
//! is dropped and collection occurs, dropping the data and leaving the other references in an
//! invalid state.
//!
//! Rather than keeping a list of all values that have been allocated (which would need locking on
//! allocation), this scheme only keeps the root references and has a list of dropped references
//! for consideration (which does not need locking). So rather than doing a traditional mark and
//! sweep (checking for inclusion) on all allocations, this traverses to determine everything
//! that's reachable and drops data related to dropped references that are no longer reachable
//! (checking for exclusion).

use abi_stable::{
    std_types::{RArc, RHashMap},
    StableAbi,
};
use std::cell::Cell;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

plugin_tls::thread_local! {
    static PILE: Cell<*const GarbagePile> = Cell::new(std::ptr::null());
}

/// A garbage-collected value.
///
/// These values are read-only.
#[derive(StableAbi)]
#[repr(transparent)]
pub struct Gc<T: GcRefs> {
    ptr: NonNull<T>,
}

impl<T: GcRefs> std::ops::Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: GcRefs> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: GcRefs> std::borrow::Borrow<T> for Gc<T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: GcRefs + std::fmt::Debug> std::fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&**self, f)
    }
}

impl<T: GcRefs + std::fmt::Display> std::fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&**self, f)
    }
}

impl<T: GcRefs> Gc<T> {
    pub fn new(value: T) -> Self {
        Gc {
            ptr: GcInfo::new(value),
        }
    }

    /// Safety: if you clone a Gc, the result is only guaranteed to be valid as long as the source
    /// Gc is valid (until the cloned Gc is added as a reference of some other valid value).
    pub unsafe fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }

    pub fn root(self: &Self) {
        self.info()
            .add_to_pile_with(|v| v | PILE_CONSIDER_ROOT | PILE_ROOT);
    }

    pub fn unroot(self: &Self) {
        self.info()
            .add_to_pile_with(|v| (v | PILE_CONSIDER_ROOT) & !PILE_ROOT);
    }

    fn info(&self) -> &GcInfo<T> {
        unsafe { GcInfo::from_ptr(self.ptr).as_ref() }
    }
}

impl<T: GcRefs> Drop for Gc<T> {
    fn drop(&mut self) {
        self.info().add_to_pile_with(|v| v | PILE_CONSIDER_DROP);
    }
}

/// Tie a Gc lifetime to (at least) a specific scope.
///
/// A Gc can only be actively rooted from one location at a time, so if you want this lifetime to
/// extend to multiples scopes, wrap it in e.g. an Arc (rather than creating multiple GcScopeRoots
/// for a specific Gc).
#[derive(StableAbi)]
#[repr(transparent)]
pub struct GcScopeRoot<T: GcRefs>(Gc<T>);

impl<T: GcRefs> GcScopeRoot<T> {
    pub fn new(pointer: &Gc<T>) -> Self {
        Gc::root(pointer);
        GcScopeRoot(unsafe { pointer.clone() })
    }
}

impl<T: GcRefs> Drop for GcScopeRoot<T> {
    fn drop(&mut self) {
        Gc::unroot(&self.0);
    }
}

impl<T: GcRefs> std::ops::Deref for GcScopeRoot<T> {
    type Target = Gc<T>;

    fn deref(&self) -> &Gc<T> {
        &self.0
    }
}

impl<T: GcRefs> AsRef<Gc<T>> for GcScopeRoot<T> {
    fn as_ref(&self) -> &Gc<T> {
        &**self
    }
}

impl<T: GcRefs> std::borrow::Borrow<Gc<T>> for GcScopeRoot<T> {
    fn borrow(&self) -> &Gc<T> {
        &**self
    }
}

impl<T: GcRefs + std::fmt::Debug> std::fmt::Debug for GcScopeRoot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&**self, f)
    }
}

impl<T: GcRefs + std::fmt::Display> std::fmt::Display for GcScopeRoot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&**self, f)
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct GcVTable<T: 'static> {
    gc_refs: extern "C" fn(&T, v: &mut Visitor),
    drop: extern "C" fn(NonNull<GcInfo<T>>),
}

pub trait GcRefs: Sized + 'static {
    fn gc_refs(&self, v: &mut Visitor);
}

impl GcRefs for () {
    fn gc_refs(&self, _v: &mut Visitor) {}
}

trait GcRefsTable: GcRefs {
    const VTABLE: GcVTable<Self> = GcVTable {
        gc_refs: Self::gc_refs_impl,
        drop: Self::drop_info,
    };

    extern "C" fn drop_info(ptr: NonNull<GcInfo<Self>>) {
        unsafe { std::mem::drop(Box::from_raw(ptr.as_ptr())) };
    }

    extern "C" fn gc_refs_impl(&self, v: &mut Visitor) {
        GcRefs::gc_refs(self, v);
    }
}

impl<T: GcRefs> GcRefsTable for T {}

#[derive(Default, StableAbi)]
#[repr(C)]
pub struct Visitor {
    visited: RHashMap<NonNull<GcInfo<()>>, ()>,
    to_drop: RHashMap<NonNull<GcInfo<()>>, ()>,
    visiting_dropping: bool,
}

impl Visitor {
    /// Visit the given Gc pointer.
    #[inline]
    pub fn visit<T: GcRefs>(&mut self, ptr: &Gc<T>) {
        self.visit_generic(unsafe { ptr.info().generic_ptr().as_ref() });
    }

    fn visit_generic(&mut self, info: &GcInfo<()>) {
        if self.visiting_dropping {
            // Only visit things which were not visited and aren't in the pile.
            if !self.visited.contains_key(&info.generic_ptr()) && !info.in_pile() {
                if self.to_drop.insert(info.generic_ptr(), ()).is_none() {
                    info.gc_refs(self);
                }
            }
        } else if self.visited.insert(info.generic_ptr(), ()).is_none() {
            info.gc_refs(self);
        }
    }

    fn visiting_dropping(&mut self) {
        self.visiting_dropping = true;
    }
}

/// The garbage pile which will be collected.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct GarbagePile {
    top: RArc<AtomicPtr<GcInfo<()>>>,
}

#[pin_project::pin_project]
pub struct ScopeAsync<'a, Fut> {
    collector: &'a GarbagePile,
    #[pin]
    fut: Fut,
}

impl<Fut: std::future::Future> std::future::Future for ScopeAsync<'_, Fut> {
    type Output = Fut::Output;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        let me = self.project();
        me.collector.scope(|| me.fut.poll(cx))
    }
}

impl GarbagePile {
    pub fn scope<R, F: FnOnce() -> R>(&self, f: F) -> R {
        let old = PILE.with(|c| c.replace(self as *const GarbagePile));
        let ret = f();
        PILE.with(|c| c.set(old));
        ret
    }

    pub fn scope_async<Fut: std::future::Future>(&self, fut: Fut) -> ScopeAsync<Fut> {
        ScopeAsync {
            collector: self,
            fut,
        }
    }

    pub fn with<R, F: FnOnce(&GarbagePile) -> R>(f: F) -> Option<R> {
        PILE.with(|c| unsafe { c.get().as_ref() }.map(f))
    }
}

/// A non-null but invalid pointer sentinel that represents the end of the `pile`.
///
/// Null cannot be used because the GcInfo `next` field must be able to distinguish being in the
/// pile. NonNull::dangling() doesn't guarantee that the value is the same for each call, and also
/// explicitly says the value shouldn't be used as a sentinel (even though the standard library
/// implementation would result in a memory location that could never be used).
///
/// The lower 3 bits of the `next` pointers are used to indicate actions.
const PILE_END: *mut GcInfo<()> = (1 << 3) as *mut _;

const PILE_CONSIDER_DROP: usize = 1 << 0;
const PILE_CONSIDER_ROOT: usize = 1 << 1;
const PILE_ROOT: usize = 1 << 2;

#[derive(StableAbi)]
#[repr(C)]
pub struct GarbageCollector {
    roots: RHashMap<NonNull<GcInfo<()>>, ()>,
    pub pile: GarbagePile,
}

impl Default for GarbageCollector {
    fn default() -> Self {
        GarbageCollector {
            roots: Default::default(),
            pile: GarbagePile {
                top: RArc::new(PILE_END.into()),
            },
        }
    }
}

impl GarbageCollector {
    pub fn collect(&mut self) {
        let mut top = self.pile.top.swap(PILE_END, Ordering::Acquire);
        if top == PILE_END {
            // No need to traverse anything.
            return;
        }

        // Reset all next pointers _prior_ to traversing allocations. If a `Gc` is dropped while we
        // are traversing, we'll not Drop it now (even if we don't encounter it in the traversal),
        // leaving it to be dropped the next time `collect` is called. If we didn't reset `next`
        // until after traversal, it's possible that we traverse, see the element, and then it's
        // dropped while we are still traversing but it wouldn't be added for consideration again
        // (so the memory would be leaked).

        let mut consider_drop = Vec::new();
        while top != PILE_END {
            let info = unsafe { top.as_ref().unwrap_unchecked() };
            let next = info.next.swap(0, Ordering::Relaxed);
            top = (next & !0b111) as *mut GcInfo<()>;
            if next & PILE_CONSIDER_DROP != 0 {
                consider_drop.push(info.generic_ptr());
            }
            if next & PILE_CONSIDER_ROOT != 0 {
                if next & PILE_ROOT != 0 {
                    self.roots.insert(info.generic_ptr(), ());
                } else {
                    self.roots.remove(&info.generic_ptr());
                }
            }
        }

        if consider_drop.is_empty() {
            // No need to traverse anything.
            return;
        }

        // Traverse all info starting at the roots.
        let mut v = Visitor::default();
        for gc in self.roots.keys() {
            v.visited.insert(*gc, ());
            unsafe { gc.as_ref() }.gc_refs(&mut v);
        }

        // Visit all dropped infos, recording all values and references (deeply) which aren't in
        // the visited set. Without visiting ancestors, each subsequent collect would only collect
        // the next generation of ancestors (as the Gc references are iteratively dropped).
        v.visiting_dropping();
        for infoptr in consider_drop {
            if !v.visited.contains_key(&infoptr) {
                // Drop the info if it hasn't been added to the pile while we were traversing.
                let info = unsafe { infoptr.as_ref() };
                if !info.in_pile() {
                    v.to_drop.insert(infoptr, ());
                    info.gc_refs(&mut v);
                }
            }
        }

        // Clear any current PILE so that anything dropped here won't add children to the drop
        // list.
        let old_scope = PILE.with(|c| c.replace(std::ptr::null()));
        // Anything in `to_drop` must _not_ have been in `visited`, and all descendents that are no
        // longer reachable are accounted for. Based on the assumptions of this collector
        // implementation, we don't need to check `next_dropped` for null, as the above loop and
        // `gc_refs` visit (after `visiting_dropping` is enabled) checks for this, and we may
        // assume that if the value wasn't reachable when we traversed and it hasn't been dropped,
        // then it won't be dropped (users are not allowed to keep such dangling values around;
        // they must be added to the graph prior to removing their source reference that was
        // already in the graph).
        for info in v.to_drop.keys() {
            unsafe {
                info.as_ref().drop();
            }
        }
        PILE.with(|c| c.set(old_scope));
    }
}

#[derive(StableAbi)]
#[repr(C, align(8))]
struct GcInfo<T: 'static> {
    /// This is similar to a `AtomicPtr<GcInfo<()>>`, acting as a pointer to the next element in the
    /// pile. It may be null (not in the pile), a valid pointer, or a sentinel pointer
    /// representing the end of the stack.
    ///
    /// The bottom 3 bits of the pointer are used to indicate the action(s) to be taken for this
    /// value.
    ///
    /// With these bits numbered 2, 1, 0 from most to least significant:
    /// * if bit 0 is set, the pointer should be considered for dropping
    /// * if bit 1 is set, the pointer should be added or removed as a root, based on bit 2
    next: AtomicUsize,
    vtable: &'static GcVTable<T>,
    /// The value is placed at the end of the struct so that `next_dropped` and `vtable` may be
    /// accessed in heterogeneous `GcInfo` values.
    ///
    /// The alignment here is important: we assume that `T` will have an alignment that is a
    /// divisor of 2*usize (the prior two pointers), as later we will transmute `T` to `()`.
    value: T,
}

impl<T: GcRefs> GcInfo<T> {
    pub fn new(value: T) -> NonNull<T> {
        let info = Box::leak(Box::new(GcInfo {
            next: Default::default(),
            vtable: &<T as GcRefsTable>::VTABLE,
            value,
        }));
        (&mut info.value).into()
    }

    /// Safety: `ptr` _must_ have been constructed with `new`.
    pub unsafe fn from_ptr(ptr: NonNull<T>) -> NonNull<Self> {
        // Use GcInfo<()> for a prototype of the size of the GcInfo header.
        const OFFSET: usize = std::mem::size_of::<GcInfo<()>>();
        NonNull::new_unchecked((ptr.as_ptr() as usize - OFFSET) as *mut Self)
    }

    pub fn generic_ptr(&self) -> NonNull<GcInfo<()>> {
        unsafe { NonNull::new_unchecked(self as *const GcInfo<T> as *mut GcInfo<()>) }
    }

    pub fn gc_refs(&self, v: &mut Visitor) {
        (self.vtable.gc_refs)(&self.value, v);
    }

    /// Safety: this can only be done with the info is not referenced anywhere else. After this is
    /// called, the info should not be accessed again.
    pub unsafe fn drop(&self) {
        (self.vtable.drop)(NonNull::new_unchecked(self as *const Self as *mut Self));
    }

    pub fn add_to_pile(&self) -> bool {
        GarbagePile::with(|pile| {
            let top_ptr = &pile.top;
            let mut old_top_ptr = top_ptr.load(Ordering::Relaxed);
            loop {
                if self
                    .next
                    .compare_exchange(
                        0,
                        old_top_ptr as usize,
                        Ordering::Relaxed,
                        Ordering::Relaxed,
                    )
                    .is_err()
                {
                    // Only add to the pile if previously 0, otherwise it is already in the pile.
                    break;
                }

                match top_ptr.compare_exchange_weak(
                    old_top_ptr,
                    self.generic_ptr().as_ptr(),
                    Ordering::Release,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => break,
                    Err(old) => old_top_ptr = old,
                }
            }
        })
        .is_some()
    }

    pub fn add_to_pile_with<F: FnMut(usize) -> usize>(&self, mut flags: F) {
        loop {
            if !self.add_to_pile() {
                break;
            }
            if self
                .next
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |v| {
                    if v == 0 {
                        None
                    } else {
                        Some(flags(v))
                    }
                })
                .is_ok()
            {
                break;
            }
        }
    }

    pub fn in_pile(&self) -> bool {
        self.next.load(Ordering::Relaxed) != 0
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    };

    #[derive(Default)]
    struct NodeChildren {
        children: HashMap<usize, Gc<Node>>,
        next: usize,
    }

    impl NodeChildren {
        pub fn add(&mut self, node: Gc<Node>) -> usize {
            let id = self.next;
            self.next += 1;
            self.children.insert(id, node);
            id
        }

        pub fn remove(&mut self, id: usize) {
            self.children.remove(&id);
        }
    }

    #[derive(Default)]
    struct Node {
        children: Mutex<NodeChildren>,
        drop: Option<Box<dyn FnOnce()>>,
    }

    impl GcRefs for Node {
        fn gc_refs(&self, v: &mut Visitor) {
            for (_, o) in self.children.lock().unwrap().children.iter() {
                v.visit(&o);
            }
        }
    }

    impl Node {
        pub fn new<F: FnOnce() + 'static>(drop: F) -> Self {
            Self {
                children: Default::default(),
                drop: Some(Box::new(drop)),
            }
        }

        pub fn insert(&self, child: Gc<Node>) -> usize {
            self.children.lock().unwrap().add(child)
        }

        pub fn remove(&self, id: usize) {
            self.children.lock().unwrap().remove(id);
        }
    }

    impl Drop for Node {
        fn drop(&mut self) {
            (self.drop.take().unwrap())();
        }
    }

    #[derive(Default)]
    struct NodeLifetimes {
        nodes: HashMap<&'static str, Arc<AtomicBool>>,
    }

    impl NodeLifetimes {
        pub fn node(&mut self, name: &'static str) -> Gc<Node> {
            let dropped_flag = Arc::new(AtomicBool::default());
            self.nodes.insert(name, dropped_flag.clone());
            Gc::new(Node::new(move || {
                dropped_flag.store(true, Ordering::Relaxed);
            }))
        }

        pub fn is_dropped(&self, name: &'static str) -> bool {
            self.nodes
                .get(name)
                .map(|f| f.load(Ordering::Relaxed))
                .unwrap_or(false)
        }

        pub fn check<A: AsRef<[&'static str]>, B: AsRef<[&'static str]>>(
            &self,
            exist: A,
            dropped: B,
        ) {
            for n in exist.as_ref() {
                assert!(!self.is_dropped(n), "expected '{}' to still exist", n);
            }
            for n in dropped.as_ref() {
                assert!(self.is_dropped(n), "expected '{}' to be dropped", n);
            }
        }
    }

    ///     root
    ///     /  \
    ///    a    e
    ///   / \    \
    ///  b   c    f
    ///      |
    ///      d
    #[test]
    fn dag() {
        let mut nl = NodeLifetimes::default();
        let mut gc = GarbageCollector::default();
        let pile = gc.pile.clone();
        pile.scope(|| {
            let root = nl.node("root");
            Gc::root(&root);
            let root_e = {
                let a = nl.node("a");
                a.insert(nl.node("b"));
                let c = nl.node("c");
                c.insert(nl.node("d"));
                a.insert(c);
                root.insert(a);

                let e = nl.node("e");
                e.insert(nl.node("f"));
                root.insert(e)
            };

            gc.collect();

            nl.check(["root", "a", "b", "c", "d", "e", "f"], []);

            root.remove(root_e);

            gc.collect();

            nl.check(["root", "a", "b", "c", "d"], ["e", "f"]);

            Gc::unroot(&root);
        });
        gc.collect();
        nl.check([], ["root", "a", "b", "c", "d", "e", "f"]);
    }

    ///    root
    ///     |
    ///     a
    ///    / \
    ///   b - c
    #[test]
    fn ref_loop() {
        let mut nl = NodeLifetimes::default();
        let mut gc = GarbageCollector::default();
        let pile = gc.pile.clone();
        pile.scope(|| {
            let root = nl.node("root");
            Gc::root(&root);
            let root_a = {
                let a = nl.node("a");
                let b = nl.node("b");
                let c = nl.node("c");
                c.insert(unsafe { a.clone() });
                b.insert(c);
                a.insert(b);
                root.insert(a)
            };

            gc.collect();

            nl.check(["root", "a", "b", "c"], []);

            root.remove(root_a);

            gc.collect();

            nl.check(["root"], ["a", "b", "c"]);

            Gc::unroot(&root);
        });
        gc.collect();
        nl.check([], ["root", "a", "b", "c"]);
    }

    #[test]
    fn gc_scope_root() {
        let mut nl = NodeLifetimes::default();
        let mut gc = GarbageCollector::default();
        let pile = gc.pile.clone();
        pile.scope(|| {
            let root = nl.node("root");
            Gc::root(&root);
            {
                let _scope_root;
                let root_a = {
                    let a = nl.node("a");
                    let b = nl.node("b");
                    let c = nl.node("c");
                    b.insert(c);
                    _scope_root = GcScopeRoot::new(&b);
                    a.insert(b);
                    root.insert(a)
                };

                gc.collect();

                nl.check(["root", "a", "b", "c"], []);

                root.remove(root_a);

                gc.collect();

                nl.check(["root", "b", "c"], ["a"]);
            }
            gc.collect();
            nl.check(["root"], ["a", "b", "c"]);

            Gc::unroot(&root);
        });
        gc.collect();
        nl.check([], ["root", "a", "b", "c"]);
    }
}
