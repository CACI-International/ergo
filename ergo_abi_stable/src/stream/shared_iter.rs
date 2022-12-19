//! A shared iterator, allowing multiple consumers.

use super::shared_stream::*;
use abi_stable::{
    external_types::RMutex,
    std_types::{RArc, ROption},
    StableAbi,
};

#[derive(StableAbi)]
#[sabi(bound(I::Item: StableAbi))]
#[repr(C)]
pub struct SharedIterator<I: Iterator> {
    stream_iter: SharedStreamIter<I::Item>,
    source: RArc<RMutex<ROption<I>>>,
}

impl<I: Iterator> Clone for SharedIterator<I> {
    fn clone(&self) -> Self {
        SharedIterator {
            stream_iter: self.stream_iter.clone(),
            source: self.source.clone(),
        }
    }
}

impl<I: Iterator> std::fmt::Debug for SharedIterator<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("SharedIterator").finish()
    }
}

impl<I: Iterator> SharedIterator<I> {
    #[allow(dead_code)]
    /// Make a new SharedIterator from the given iterator.
    pub fn new(iter: I) -> Self {
        let (_, stream_iter) = SharedStream::new();
        SharedIterator {
            stream_iter,
            source: RArc::new(RMutex::new(ROption::RSome(iter))),
        }
    }
}

impl<I: Iterator> Iterator for SharedIterator<I>
where
    I::Item: Clone,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.stream_iter.current() {
                Some(v) => {
                    let v = v.clone();
                    // Move to next, but disregard value (we don't access it until the next
                    // iteration).
                    self.stream_iter.next();
                    break Some(v);
                }
                None => {
                    let mut guard = self.source.lock();
                    // Check again in case something else read a value while we waited for the lock.
                    if self.stream_iter.current().is_some() {
                        continue;
                    }
                    match &mut *guard {
                        ROption::RSome(iter) => match iter.next() {
                            Some(v) => {
                                self.stream_iter.stream().push(v);
                                continue;
                            }
                            None => {
                                *guard = ROption::RNone;
                                break None;
                            }
                        },
                        ROption::RNone => break None,
                    }
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let guard = self.source.lock();
        let (min, max) = match &*guard {
            ROption::RSome(iter) => iter.size_hint(),
            ROption::RNone => (0, Some(0)),
        };

        let count = self.stream_iter.count_available();
        (
            min + count,
            match max {
                None => None,
                Some(max) => Some(max + count),
            },
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shared_iter() {
        let v = vec![1, 2, 3, 4];
        let mut i1 = SharedIterator::new(v.into_iter());
        let mut i2 = i1.clone();
        assert!(i1.next() == Some(1));
        assert!(i1.next() == Some(2));
        assert!(i2.next() == Some(1));
        assert!(i1.next() == Some(3));
        assert!(i1.next() == Some(4));
        assert!(i1.next() == None);
        assert!(i2.next() == Some(2));
        assert!(i2.next() == Some(3));
        assert!(i2.next() == Some(4));
        assert!(i2.next() == None);
    }
}
