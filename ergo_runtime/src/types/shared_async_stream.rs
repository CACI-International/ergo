//! A shared futures::stream::Stream, allowing multiple consumers.

use super::shared_stream::*;
use abi_stable::{
    external_types::RMutex,
    std_types::{RArc, ROption},
    StableAbi,
};
use futures::stream::Stream;

#[derive(StableAbi)]
#[sabi(bound = "S::Item: StableAbi")]
#[repr(C)]
pub struct SharedAsyncStream<S: Stream> {
    stream_iter: SharedStreamIter<S::Item>,
    source: RArc<RMutex<ROption<S>>>,
}

impl<S: Stream> Clone for SharedAsyncStream<S> {
    fn clone(&self) -> Self {
        SharedAsyncStream {
            stream_iter: self.stream_iter.clone(),
            source: self.source.clone(),
        }
    }
}

impl<S: Stream> std::fmt::Debug for SharedAsyncStream<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("SharedAsyncStream").finish()
    }
}

impl<S: Stream> SharedAsyncStream<S> {
    /// Make a new SharedAsyncStream from the given stream.
    pub fn new(stream: S) -> Self {
        let (_, stream_iter) = SharedStream::new();
        SharedAsyncStream {
            stream_iter,
            source: RArc::new(RMutex::new(ROption::RSome(stream))),
        }
    }
}

impl<S: Stream + std::marker::Unpin> Stream for SharedAsyncStream<S>
where
    S::Item: Clone,
{
    type Item = S::Item;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Option<Self::Item>> {
        let me = &mut *self;
        loop {
            match me.stream_iter.current() {
                Some(v) => {
                    let v = v.clone();
                    // Move to next, but disregard value (we don't access it until the next
                    // iteration).
                    me.stream_iter.next();
                    break std::task::Poll::Ready(Some(v));
                }
                None => {
                    // XXX is it okay to use a "normal" mutex versus a futures-aware mutex here?
                    // It seems like it should be; any consumers which concurrently poll the inner
                    // stream while it's not ready will return pending, and will be woken and try
                    // again (and other state will negotiate which will actually push the value
                    // versus just taking the new values).
                    let mut guard = me.source.lock();
                    // Check again in case something else read a value while we waited for the lock.
                    if me.stream_iter.has_current() {
                        continue;
                    }
                    match &mut *guard {
                        ROption::RSome(stream) => {
                            match Stream::poll_next(std::pin::Pin::new(stream), cx) {
                                std::task::Poll::Pending => break std::task::Poll::Pending,
                                std::task::Poll::Ready(r) => match r {
                                    Some(v) => {
                                        me.stream_iter.stream().push(v);
                                        continue;
                                    }
                                    None => {
                                        *guard = ROption::RNone;
                                        break std::task::Poll::Ready(None);
                                    }
                                },
                            }
                        }
                        ROption::RNone => break std::task::Poll::Ready(None),
                    }
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let guard = self.source.lock();
        let (min, max) = match &*guard {
            ROption::RSome(stream) => stream.size_hint(),
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
