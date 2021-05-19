//! Shared stream for n producers and n consumers.

use abi_stable::{
    external_types::RRwLock,
    std_types::{RArc, ROption},
    StableAbi,
};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(StableAbi)]
#[repr(C)]
struct SharedStreamItem<T> {
    item: ROption<T>,
    count: AtomicU64,
    next: Option<NonNull<SharedStreamItem<T>>>,
}

#[derive(StableAbi)]
#[repr(C)]
pub struct SharedStreamIter<T> {
    stream: SharedStream<T>,
    current: NonNull<SharedStreamItem<T>>,
    has_read_current: bool,
}

unsafe impl<T> Send for SharedStreamIter<T> {}
unsafe impl<T> Sync for SharedStreamIter<T> {}

impl<T> SharedStreamIter<T> {
    fn new(stream: SharedStream<T>, current: NonNull<SharedStreamItem<T>>) -> Self {
        unsafe { current.as_ref() }
            .count
            .fetch_add(1, Ordering::Relaxed);
        SharedStreamIter {
            stream,
            current,
            has_read_current: false,
        }
    }

    pub fn stream(&self) -> &SharedStream<T> {
        &self.stream
    }

    /// Get the current value, if any.
    ///
    /// The returned reference is valid across clones.
    pub fn current(&mut self) -> Option<&T> {
        if let ROption::RSome(v) = &unsafe { self.current.as_ref() }.item {
            self.has_read_current = true;
            Some(v)
        } else {
            None
        }
    }

    /// Return whether `current` will return a value.
    pub fn has_current(&self) -> bool {
        unsafe { self.current.as_ref() }.item.is_some()
    }

    /// Get a count of the immediately available items.
    ///
    /// This runs in O(n) time.
    pub fn count_available(&self) -> usize {
        let mut ret = 0;
        let mut item = unsafe { self.current.as_ref() };
        while let ROption::RSome(_) = &item.item {
            ret += 1;
            match &item.next {
                None => break,
                Some(v) => item = unsafe { v.as_ref() },
            }
        }
        ret
    }

    /// Get the next value, if any.
    ///
    /// We cannot implement std::iter::Iterator because this is a streaming iterator, which
    /// must retain the reference lifetime correctly.
    ///
    /// The returned reference is valid across clones.
    pub fn next(&mut self) -> Option<&T> {
        {
            let guard = self.stream.inner.read();
            if self.has_read_current {
                if let Some(next) = unsafe { self.current.as_ref() }.next {
                    unsafe { self.current.as_ref() }
                        .count
                        .fetch_sub(1, Ordering::Relaxed);
                    self.current = next;
                    unsafe { self.current.as_ref() }
                        .count
                        .fetch_add(1, Ordering::Relaxed);
                    self.has_read_current = false;
                    drop(guard);
                    self.stream.remove_unused();
                } else {
                    return None;
                }
            }
        }

        self.current()
    }
}

impl<T> Clone for SharedStreamIter<T> {
    fn clone(&self) -> Self {
        Self::new(self.stream.clone(), self.current)
    }
}

impl<T> Drop for SharedStreamIter<T> {
    fn drop(&mut self) {
        unsafe { self.current.as_ref() }
            .count
            .fetch_sub(1, Ordering::Relaxed);
        self.stream.remove_unused();
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct SharedStreamInner<T> {
    begin: Option<NonNull<SharedStreamItem<T>>>,
    end: Option<NonNull<SharedStreamItem<T>>>,
    item: unsafe extern "C" fn() -> NonNull<SharedStreamItem<T>>,
    drop_fn: extern "C" fn(NonNull<SharedStreamItem<T>>),
}

unsafe impl<T> Send for SharedStreamInner<T> {}
unsafe impl<T> Sync for SharedStreamInner<T> {}

impl<T> Drop for SharedStreamInner<T> {
    fn drop(&mut self) {
        debug_assert!(self.begin.is_none() && self.end.is_none());
    }
}

#[derive(StableAbi)]
#[repr(C)]
pub struct SharedStream<T> {
    inner: RArc<RRwLock<SharedStreamInner<T>>>,
}

impl<T> Clone for SharedStream<T> {
    fn clone(&self) -> Self {
        SharedStream {
            inner: self.inner.clone(),
        }
    }
}

impl<T> SharedStream<T> {
    pub fn new() -> (Self, SharedStreamIter<T>) {
        extern "C" fn drop_fn<T>(ptr: NonNull<SharedStreamItem<T>>) {
            drop(unsafe { Box::from_raw(ptr.as_ptr()) })
        }

        extern "C" fn item<T>() -> NonNull<SharedStreamItem<T>> {
            let b = Box::into_raw(Box::new(SharedStreamItem {
                item: ROption::RNone,
                count: AtomicU64::new(0),
                next: None,
            }));
            unsafe { NonNull::new_unchecked(b) }
        }

        let ret = SharedStream {
            inner: RArc::new(RRwLock::new(SharedStreamInner {
                begin: None,
                end: None,
                item: item::<T>,
                drop_fn: drop_fn::<T>,
            })),
        };

        let item = item::<T>();

        // Set begin/end of shared stream to the single item.
        {
            let mut guard = ret.inner.write();
            guard.begin = Some(item);
            guard.end = Some(item);
        }

        (ret.clone(), SharedStreamIter::new(ret, item))
    }

    /// Remove all consecutive values from beginning with 0 counts.
    fn remove_unused(&self) {
        let mut guard = self.inner.write();
        while let Some(mut v) = guard.begin {
            let item = unsafe { v.as_mut() };
            if item.count.load(Ordering::Relaxed) == 0 {
                guard.begin = item.next;
                (guard.drop_fn)(v);
                if guard.begin.is_none() {
                    guard.end = None;
                }
            } else {
                break;
            }
        }
    }

    /// Push a new value to the shared stream.
    pub fn push(&self, v: T) {
        let mut guard = self.inner.write();
        if let Some(mut e) = guard.end {
            let item = unsafe { e.as_mut() };
            item.item = ROption::RSome(v);
            item.next = Some(unsafe { (guard.item)() });
            guard.end = item.next;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shared_stream() {
        let (stream, mut iter) = SharedStream::new();
        let mut i2 = iter.clone();
        assert!(iter.current() == None);
        assert!(i2.current() == None);
        stream.push(10);
        stream.push(42);
        assert!(iter.next() == Some(&10));
        assert!(iter.next() == Some(&42));
        assert!(iter.next() == None);
        assert!(i2.next() == Some(&10));
        stream.push(200);
        assert!(i2.next() == Some(&42));
        assert!(i2.next() == Some(&200));
        assert!(i2.next() == None);
        assert!(iter.next() == Some(&200));
        stream.push(11);
        stream.push(12);
        drop(i2);
        assert!(iter.next() == Some(&11));
        drop(iter);
        stream.push(4);
        stream.push(5);
        stream.push(6);
    }

    mod counter {
        use std::sync::atomic::{AtomicU64, Ordering};

        pub struct Counter<'a>(&'a AtomicU64);
        impl<'a> Counter<'a> {
            fn new(count: &'a AtomicU64) -> Self {
                count.fetch_add(1, Ordering::Relaxed);
                Counter(count)
            }
        }

        impl<'a> Drop for Counter<'a> {
            fn drop(&mut self) {
                self.0.fetch_sub(1, Ordering::Relaxed);
            }
        }

        pub fn counted(v: &AtomicU64) -> Counter {
            Counter::new(v)
        }
    }

    #[test]
    fn memory_freed() {
        use counter::counted;
        use std::sync::atomic::AtomicU64;

        let count = AtomicU64::new(0);
        let (stream, mut iter) = SharedStream::new();
        stream.push(counted(&count));
        stream.push(counted(&count));
        stream.push(counted(&count));
        assert!(count.load(Ordering::Relaxed) == 3);
        iter.next();
        assert!(count.load(Ordering::Relaxed) == 3);
        iter.next();
        assert!(count.load(Ordering::Relaxed) == 2);
        let mut iter2 = iter.clone();
        stream.push(counted(&count));
        stream.push(counted(&count));
        assert!(count.load(Ordering::Relaxed) == 4);
        iter.next();
        iter.next();
        iter.next();
        assert!(count.load(Ordering::Relaxed) == 4);
        iter2.next();
        iter2.next();
        assert!(count.load(Ordering::Relaxed) == 3);
        drop(iter2);
        assert!(count.load(Ordering::Relaxed) == 1);
        drop(iter);
        assert!(count.load(Ordering::Relaxed) == 0);
        stream.push(counted(&count));
        stream.push(counted(&count));
        assert!(count.load(Ordering::Relaxed) == 0);
    }
}
