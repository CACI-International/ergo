//! Allow multiple threads to mutate a value without mutexes.
//!
//! Threads will queue mutations, and they must be polled by another thread. All mutations
//! occur in one thread (the polling thread). Mutations are guaranteed to be applied in the order
//! they were queued.

use std::sync::Arc;

pub struct ThreadedMut<T> {
    value: T,
    mutator: Mutator<T>,
}

pub struct Mutator<T> {
    queue: Arc<stack::Stack<Box<dyn FnOnce(&mut T)>>>,
}

impl<T> Clone for Mutator<T> {
    fn clone(&self) -> Self {
        Mutator {
            queue: self.queue.clone(),
        }
    }
}

impl<T> ThreadedMut<T> {
    pub fn new(value: T) -> Self {
        ThreadedMut {
            value,
            mutator: Mutator {
                queue: Arc::new(Default::default()),
            },
        }
    }

    pub fn flush(&mut self) {
        let mut old_stack = self.mutator.queue.take();
        let mut new_stack = stack::Stack::default();
        // Reverse the stack to apply mutations in-order.
        while let Some(i) = old_stack.pop_item_mut() {
            new_stack.push_item_mut(i);
        }
        while let Some(f) = new_stack.pop_mut() {
            f(&mut self.value);
        }
    }

    pub fn mutator(&self) -> &Mutator<T> {
        &self.mutator
    }
}

impl<T> std::ops::Deref for ThreadedMut<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> std::ops::DerefMut for ThreadedMut<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Mutator<T> {
    pub fn mutate<F: FnOnce(&mut T) + 'static>(&self, f: F) {
        self.queue.push(Box::new(f));
    }
}

mod atomic_box {
    use std::sync::atomic::{AtomicPtr, Ordering};

    /// Ownership similar to Box, but allows the inner pointer to be null and allows atomic operations.
    pub struct AtomicBox<T>(AtomicPtr<T>);

    impl<T> Default for AtomicBox<T> {
        fn default() -> Self {
            AtomicBox(Default::default())
        }
    }

    impl<T> AtomicBox<T> {
        pub fn new(value: T) -> Self {
            AtomicBox(AtomicPtr::new(Box::into_raw(Box::new(value))))
        }

        pub fn take(&self) -> Option<T> {
            let ptr = self.0.swap(std::ptr::null_mut(), Ordering::Relaxed);
            if !ptr.is_null() {
                // Safety: pointer is always created with `Box::into_raw`.
                Some(*unsafe { Box::from_raw(ptr) })
            } else {
                None
            }
        }

        pub fn is_null(&self) -> bool {
            self.0.load(Ordering::Relaxed).is_null()
        }

        /// Get the inner atomic pointer.
        ///
        /// This is unsafe because any changes to the AtomicPtr must maintain the guarantees of the
        /// AtomicBox (unique ownership of a valid pointer).
        pub unsafe fn inner_ptr(&self) -> &AtomicPtr<T> {
            &self.0
        }

        pub unsafe fn set_ptr(&mut self, ptr: *mut T) {
            *self.0.get_mut() = ptr;
        }

        pub unsafe fn get_ptr(&mut self) -> *mut T {
            *self.0.get_mut()
        }

        pub unsafe fn as_mut(&mut self) -> &mut T {
            &mut **self.0.get_mut()
        }
    }

    impl<T> Drop for AtomicBox<T> {
        fn drop(&mut self) {
            let ptr = *self.0.get_mut();
            if !ptr.is_null() {
                // Safety: pointer is always created with `Box::into_raw`.
                drop(unsafe { Box::from_raw(ptr) });
            }
        }
    }
}

mod stack {
    use super::atomic_box::AtomicBox;
    use std::sync::atomic::Ordering;

    pub struct Stack<T> {
        head: AtomicBox<SItem<T>>,
    }

    impl<T> Default for Stack<T> {
        fn default() -> Self {
            Stack {
                head: Default::default(),
            }
        }
    }

    pub struct Item<T>(AtomicBox<SItem<T>>);

    struct SItem<T> {
        next: AtomicBox<Self>,
        value: T,
    }

    impl<T> Stack<T> {
        pub fn push(&self, value: T) {
            self.push_item(Item(AtomicBox::new(SItem {
                next: Default::default(),
                value,
            })));
        }

        pub fn push_item(&self, Item(mut item): Item<T>) {
            debug_assert!(unsafe { item.as_mut() }.next.is_null());
            // Safety: When the loop exits, the previous `self.head` is owned by `item.next`, and
            // `item` is owned by `self.head`.
            unsafe {
                let head_ptr = self.head.inner_ptr();
                let mut old_head_ptr = head_ptr.load(Ordering::Relaxed);
                loop {
                    item.as_mut().next.set_ptr(old_head_ptr);
                    match head_ptr.compare_exchange_weak(
                        old_head_ptr,
                        item.get_ptr(),
                        Ordering::Release,
                        Ordering::Relaxed,
                    ) {
                        Ok(_) => break,
                        Err(old) => old_head_ptr = old,
                    }
                }
            }
            // `item` is now owned by `self.head`.
            std::mem::forget(item);
        }

        pub fn push_item_mut(&mut self, Item(mut item): Item<T>) {
            debug_assert!(unsafe { item.as_mut() }.next.is_null());
            // Safety: The previous `self.head` is owned by `item.next`, and `item` is owned by
            // `self.head`.
            unsafe {
                item.as_mut().next.set_ptr(self.head.get_ptr());
                self.head.set_ptr(item.get_ptr())
            }
            // `item` is now owned by `self.head`.
            std::mem::forget(item);
        }

        pub fn pop_item_mut(&mut self) -> Option<Item<T>> {
            if self.head.is_null() {
                return None;
            }
            let mut ret = Item(Default::default());
            // Safety: The previous `self.head` is owned by `ret`, and `self.head.next` is owned by
            // `self.head`.
            unsafe {
                ret.0.set_ptr(self.head.get_ptr());
                let next = self.head.as_mut().next.get_ptr();
                self.head.set_ptr(next);
                ret.0.as_mut().next.set_ptr(std::ptr::null_mut());
            }
            Some(ret)
        }

        pub fn pop_mut(&mut self) -> Option<T> {
            self.pop_item_mut()
                .map(|i| unsafe { i.0.take().unwrap_unchecked().value })
        }

        pub fn take(&self) -> Self {
            let mut ret = Self::default();
            // Safety: `self.head` is set to a null pointer, which is valid, and the returned
            // pointer is owned by the returned Stack.
            unsafe {
                let ptr = self
                    .head
                    .inner_ptr()
                    .swap(std::ptr::null_mut(), Ordering::Relaxed);
                ret.head.set_ptr(ptr);
            }
            ret
        }
    }
}
