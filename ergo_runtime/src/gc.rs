//! Concurrently garbage-collected pointers.
//!
//! The garbage collection scheme here allows for collection to occur concurrently at any time,
//! without halting changes to the reference graph. For this to work, there are a few invariants
//! that must be upheld when changing the graph (adding references):
//!
//! * A reference must only be cloned from a reference that is in the graph.
//! * A cloned reference must be added to the graph prior to the original reference being removed.
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

use abi_stable::{std_types::RHashMap, StableAbi};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, Ordering};

plugin_tls::thread_local! {
    static DISABLE_DROP: AtomicBool = AtomicBool::new(false);
}

/// A garbage-collected value.
///
/// These values are read-only.
#[derive(StableAbi)]
#[repr(transparent)]
pub struct Gc<T> {
    ptr: NonNull<T>,
}

unsafe impl<T: Sync> Sync for Gc<T> {}
unsafe impl<T: Send> Send for Gc<T> {}

impl<T> std::ops::Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T> std::borrow::Borrow<T> for Gc<T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&**self, f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&**self, f)
    }
}

impl<T> Gc<T> {
    pub fn new<PileInfo: GarbagePileItem + Default>(value: T) -> Self
    where
        T: GcRefs,
    {
        Gc {
            ptr: GcInfo::<PileInfo, T>::new(value),
        }
    }

    /// Safety: if you clone a Gc, the result is only guaranteed to be valid as long as the source
    /// Gc is valid (until the cloned Gc is added as a reference of some other valid value).
    pub unsafe fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }

    pub fn root(self: &Self) {
        self.info().add_root();
    }

    pub fn unroot(self: &Self) {
        self.info().remove_root();
    }

    fn info(&self) -> &GenericGcInfo {
        unsafe { GenericGcInfo::from_value_ptr(self.ptr).as_ref() }
    }
}

impl<T> Drop for Gc<T> {
    fn drop(&mut self) {
        if !DISABLE_DROP.with(|d| d.load(Ordering::Relaxed)) {
            self.info().consider_drop();
        }
    }
}

/// Tie a Gc lifetime to (at least) a specific scope.
///
/// A Gc can only be actively rooted from one location at a time, so if you want this lifetime to
/// extend to multiples scopes, wrap it in e.g. an Arc (rather than creating multiple GcScopeRoots
/// for a specific Gc).
#[derive(StableAbi)]
#[repr(transparent)]
pub struct GcScopeRoot<T>(Gc<T>);

impl<T> GcScopeRoot<T> {
    pub fn new(pointer: &Gc<T>) -> Self {
        Gc::root(pointer);
        GcScopeRoot(unsafe { pointer.clone() })
    }
}

impl<T> Drop for GcScopeRoot<T> {
    fn drop(&mut self) {
        Gc::unroot(&self.0);
    }
}

impl<T> std::ops::Deref for GcScopeRoot<T> {
    type Target = Gc<T>;

    fn deref(&self) -> &Gc<T> {
        &self.0
    }
}

impl<T> AsRef<Gc<T>> for GcScopeRoot<T> {
    fn as_ref(&self) -> &Gc<T> {
        &**self
    }
}

impl<T> std::borrow::Borrow<Gc<T>> for GcScopeRoot<T> {
    fn borrow(&self) -> &Gc<T> {
        &**self
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for GcScopeRoot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&**self, f)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for GcScopeRoot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&**self, f)
    }
}

pub trait GcRefs {
    fn gc_refs(&self, v: &mut Visitor);
}

impl GcRefs for () {
    fn gc_refs(&self, _v: &mut Visitor) {}
}

impl<T: GcRefs> GcRefs for Vec<T> {
    fn gc_refs(&self, v: &mut Visitor) {
        for item in self {
            item.gc_refs(v);
        }
    }
}

macro_rules! impl_tuple {
    ( $( $name: ident )+ ) => {
        impl<$($name: GcRefs),+> GcRefs for ($($name,)+) {
            fn gc_refs(&self, v: &mut Visitor) {
                #[allow(non_snake_case)]
                let ($($name,)+) = self;
                $($name.gc_refs(v);)+
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

#[derive(Default, StableAbi)]
#[repr(C)]
pub struct Visitor {
    visited: RHashMap<NonNull<GenericGcInfo>, ()>,
    to_drop: RHashMap<NonNull<GenericGcInfo>, ()>,
    visiting_dropping: bool,
}

impl Visitor {
    /// Visit the given Gc pointer.
    #[inline]
    pub fn visit<T: GcRefs>(&mut self, ptr: &Gc<T>) {
        self.visit_generic(ptr.info());
    }

    fn visit_generic(&mut self, info: &GenericGcInfo) {
        if self.visiting_dropping {
            // Only visit things which were not visited and aren't in the pile.
            if !self.visited.contains_key(&NonNull::from(info)) && !info.in_pile() {
                if self.to_drop.insert(NonNull::from(info), ()).is_none() {
                    info.gc_refs(self);
                }
            }
        } else if self.visited.insert(NonNull::from(info), ()).is_none() {
            info.gc_refs(self);
        }
    }

    fn visiting_dropping(&mut self) {
        self.visiting_dropping = true;
    }
}

pub struct Cleaner<'a> {
    roots: &'a mut RHashMap<NonNull<GenericGcInfo>, ()>,
    consider_drop: &'a mut Vec<NonNull<GenericGcInfo>>,
}

impl Cleaner<'_> {
    pub fn consider_drop(&mut self, info: NonNull<GenericGcInfo>) {
        self.consider_drop.push(info);
    }

    pub fn add_root(&mut self, info: NonNull<GenericGcInfo>) {
        self.roots.insert(info, ());
    }

    pub fn remove_root(&mut self, info: NonNull<GenericGcInfo>) {
        self.roots.remove(&info);
    }
}

pub trait GarbagePile {
    type Item: GarbagePileItem;

    fn clean(&mut self, cleaner: &mut Cleaner);
}

pub trait GarbagePileItem {
    fn consider_drop(&self);
    fn add_root(&self);
    fn remove_root(&self);
    fn is_in_pile(&self) -> bool;
}

#[derive(Default, StableAbi)]
#[repr(C)]
pub struct GarbageCollector<Pile> {
    roots: RHashMap<NonNull<GenericGcInfo>, ()>,
    pub pile: Pile,
}

unsafe impl<Pile: Send> Send for GarbageCollector<Pile> {}
unsafe impl<Pile: Sync> Sync for GarbageCollector<Pile> {}

impl<Pile: GarbagePile> GarbageCollector<Pile> {
    pub fn new<T: GcRefs>(value: T) -> Gc<T>
    where
        Pile::Item: Default,
    {
        Gc::new::<Pile::Item>(value)
    }

    pub fn collect(&mut self) {
        let mut consider_drop = Vec::new();
        let mut cleaner = Cleaner {
            roots: &mut self.roots,
            consider_drop: &mut consider_drop,
        };
        self.pile.clean(&mut cleaner);

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

        // Disable pile collection so that anything dropped here won't add children to the pile.
        DISABLE_DROP.with(|d| d.store(true, Ordering::Relaxed));
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
        DISABLE_DROP.with(|d| d.store(false, Ordering::Relaxed));
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct GcVTable {
    consider_drop: extern "C" fn(&GenericGcInfo),
    add_root: extern "C" fn(&GenericGcInfo),
    remove_root: extern "C" fn(&GenericGcInfo),
    in_pile: extern "C" fn(&GenericGcInfo) -> bool,
    gc_refs: extern "C" fn(&GenericGcInfo, v: &mut Visitor),
    drop: extern "C" fn(&GenericGcInfo),
}

#[derive(StableAbi)]
#[repr(C)]
pub struct GenericGcInfo {
    vtable: &'static GcVTable,
}

impl GenericGcInfo {
    pub fn gc_refs(&self, v: &mut Visitor) {
        (self.vtable.gc_refs)(self, v);
    }

    pub fn consider_drop(&self) {
        (self.vtable.consider_drop)(self);
    }

    pub fn add_root(&self) {
        (self.vtable.add_root)(self);
    }

    pub fn remove_root(&self) {
        (self.vtable.remove_root)(self);
    }

    pub fn in_pile(&self) -> bool {
        (self.vtable.in_pile)(self)
    }

    /// Safety: this can only be done when the info is not referenced anywhere else. After this is
    /// called, the info should not be accessed again.
    pub unsafe fn drop(&self) {
        (self.vtable.drop)(self);
    }

    /// Safety: this can only be called on pointers which were returned by GcInfo::new.
    pub unsafe fn from_value_ptr<T>(ptr: NonNull<T>) -> NonNull<GenericGcInfo> {
        NonNull::new_unchecked(offset_backward::<GenericGcInfo, _>(ptr.as_ptr()) as *mut _)
    }

    /// Safety: this can only be called on pointers which are in data allocated by GcInfo::new.
    pub unsafe fn from_pile_item_ptr<PileInfo>(ptr: NonNull<PileInfo>) -> NonNull<GenericGcInfo> {
        NonNull::new_unchecked(offset_forward::<GenericGcInfo, _>(ptr.as_ptr()) as *mut _)
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct GcInfo<PileItem, T> {
    pile_item: PileItem,
    generic: GenericGcInfo,
    /// The value is placed at the end of the struct so that prior fields may be accessed in
    /// heterogeneous `GcInfo` values.
    value: T,
}

unsafe fn offset_forward<To, From>(ptr: *const From) -> *const To {
    let ptr = ptr.add(1) as *const u8;
    ptr.add(ptr.align_offset(std::mem::align_of::<To>())) as *const To
}

unsafe fn offset_backward<To, From>(ptr: *const From) -> *const To {
    let mut ptr = ptr as *const u8;
    let offset = ptr.align_offset(std::mem::align_of::<To>());
    if offset > 0 {
        ptr = ptr.sub(std::mem::align_of::<To>() - offset);
    }
    (ptr as *const To).sub(1)
}

impl<T: GcRefs, PileInfo: GarbagePileItem> GcInfo<PileInfo, T> {
    const VTABLE: GcVTable = GcVTable {
        consider_drop: Self::consider_drop,
        add_root: Self::add_root,
        remove_root: Self::remove_root,
        in_pile: Self::in_pile,
        gc_refs: Self::gc_refs_impl,
        drop: Self::drop_info,
    };

    fn from_generic(generic: &GenericGcInfo) -> &Self {
        unsafe {
            (offset_backward::<PileInfo, _>(generic as *const _) as *const Self)
                .as_ref()
                .unwrap_unchecked()
        }
    }

    extern "C" fn consider_drop(generic: &GenericGcInfo) {
        Self::from_generic(generic).pile_item.consider_drop();
    }

    extern "C" fn add_root(generic: &GenericGcInfo) {
        Self::from_generic(generic).pile_item.add_root();
    }

    extern "C" fn remove_root(generic: &GenericGcInfo) {
        Self::from_generic(generic).pile_item.remove_root();
    }

    extern "C" fn in_pile(generic: &GenericGcInfo) -> bool {
        Self::from_generic(generic).pile_item.is_in_pile()
    }

    extern "C" fn gc_refs_impl(generic: &GenericGcInfo, v: &mut Visitor) {
        GcRefs::gc_refs(&Self::from_generic(generic).value, v);
    }

    extern "C" fn drop_info(generic: &GenericGcInfo) {
        unsafe {
            std::mem::drop(Box::from_raw(
                Self::from_generic(generic) as *const Self as *mut Self
            ))
        };
    }

    pub fn new(value: T) -> NonNull<T>
    where
        PileInfo: Default,
    {
        let info: &mut Self = Box::leak(Box::new(GcInfo {
            pile_item: Default::default(),
            generic: GenericGcInfo {
                vtable: &Self::VTABLE,
            },
            value,
        }));
        (&mut info.value).into()
    }
}

/// An implementation of a garbage pile using atomic operations.
pub mod atomic {
    use super::{Cleaner, GarbagePile, GarbagePileItem, GenericGcInfo};
    use abi_stable::{std_types::RArc, StableAbi};
    use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

    pub trait WithPile {
        fn with_pile<R, F: FnOnce(&Pile<Self>) -> R>(&self, f: F) -> Option<R>;
    }

    #[derive(Clone, StableAbi)]
    #[repr(C)]
    pub struct Pile<WP> {
        top: RArc<AtomicPtr<PileItem<WP>>>,
    }

    impl<WP> Default for Pile<WP> {
        fn default() -> Self {
            Pile {
                top: RArc::new(Self::PILE_END.into()),
            }
        }
    }

    impl<WP> Pile<WP> {
        /// A non-null but invalid pointer sentinel that represents the end of the `pile`.
        ///
        /// Null cannot be used because the PileItem `next` field must be able to distinguish being in the
        /// pile. NonNull::dangling() doesn't guarantee that the value is the same for each call, and also
        /// explicitly says the value shouldn't be used as a sentinel (even though the standard library
        /// implementation would result in a memory location that could never be used).
        ///
        /// The lower 3 bits of the `next` pointers are used to indicate actions.
        const PILE_END: *mut PileItem<WP> = (1 << 3) as *mut _;
    }

    impl<WP: WithPile> GarbagePile for Pile<WP> {
        type Item = PileItem<WP>;

        fn clean(&mut self, cleaner: &mut Cleaner) {
            let mut top = self.top.swap(Self::PILE_END, Ordering::Acquire);

            // Reset all next pointers _prior_ to traversing allocations. If a `Gc` is dropped while we
            // are traversing, we'll not Drop it now (even if we don't encounter it in the traversal),
            // leaving it to be dropped the next time `collect` is called. If we didn't reset `next`
            // until after traversal, it's possible that we traverse, see the element, and then it's
            // dropped while we are still traversing but it wouldn't be added for consideration again
            // (so the memory would be leaked).

            while top != Pile::PILE_END {
                let item = unsafe { top.as_ref().unwrap_unchecked() };
                let next = item.next.swap(0, Ordering::Relaxed);
                top = (next & !0b111) as *mut PileItem<WP>;
                let info = unsafe { GenericGcInfo::from_pile_item_ptr(item.into()) };
                if next & PILE_CONSIDER_DROP != 0 {
                    cleaner.consider_drop(info);
                }
                if next & PILE_CONSIDER_ROOT != 0 {
                    if next & PILE_ROOT != 0 {
                        cleaner.add_root(info);
                    } else {
                        cleaner.remove_root(info);
                    }
                }
            }
        }
    }

    /// Consider a pointer to be possibly dropped.
    const PILE_CONSIDER_DROP: usize = 1 << 0;
    /// Consider a pointer to be added or removed as a root.
    const PILE_CONSIDER_ROOT: usize = 1 << 1;
    /// Whether to add or remove the pointer as a root (if `PILE_CONSIDER_ROOT` is set).
    const PILE_ROOT: usize = 1 << 2;

    #[derive(Default, StableAbi)]
    #[repr(C)]
    pub struct PileItem<WP> {
        with_pile: WP,
        /// This is similar to a `AtomicPtr<PileItem>`, acting as a pointer to the next element in the
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
    }

    impl<WP: WithPile> PileItem<WP> {
        fn add_to_pile(&self) -> bool {
            self.with_pile
                .with_pile(|pile| {
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
                            self as *const Self as *mut Self,
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

        fn add_to_pile_with<F: FnMut(usize) -> usize>(&self, mut flags: F) {
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
    }

    impl<WP: WithPile> GarbagePileItem for PileItem<WP> {
        fn consider_drop(&self) {
            self.add_to_pile_with(|v| v | PILE_CONSIDER_DROP);
        }

        fn add_root(&self) {
            self.add_to_pile_with(|v| v | PILE_CONSIDER_ROOT | PILE_ROOT);
        }

        fn remove_root(&self) {
            self.add_to_pile_with(|v| (v | PILE_CONSIDER_ROOT) & !PILE_ROOT);
        }

        fn is_in_pile(&self) -> bool {
            self.next.load(Ordering::Relaxed) != 0
        }
    }

    pub mod thread_local {
        use super::*;
        use std::cell::Cell;

        #[derive(Default, Clone, Copy, StableAbi)]
        #[repr(C)]
        pub struct TlsPile;

        pub type Pile = super::Pile<TlsPile>;

        pub type GarbageCollector = super::super::GarbageCollector<Pile>;

        plugin_tls::thread_local! {
            static PILE: Cell<*const Pile> = Cell::new(std::ptr::null());
        }

        #[pin_project::pin_project]
        pub struct ScopeAsync<'a, Fut> {
            pile: &'a Pile,
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
                me.pile.scope(|| me.fut.poll(cx))
            }
        }

        impl Pile {
            pub fn scope<R, F: FnOnce() -> R>(&self, f: F) -> R {
                let old = PILE.with(|c| c.replace(self as *const Self));
                let ret = f();
                PILE.with(|c| c.set(old));
                ret
            }

            pub fn scope_async<Fut: std::future::Future>(&self, fut: Fut) -> ScopeAsync<Fut> {
                ScopeAsync { pile: self, fut }
            }
        }

        impl WithPile for TlsPile {
            fn with_pile<R, F: FnOnce(&super::Pile<Self>) -> R>(&self, f: F) -> Option<R> {
                PILE.with(|c| unsafe { c.get().as_ref() }.map(f))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::atomic::thread_local::GarbageCollector;
    use super::{Gc, GcRefs, GcScopeRoot, Visitor};
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
            GarbageCollector::new(Node::new(move || {
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

    ///    root  b
    ///     |   /|
    ///     a--/ c
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
