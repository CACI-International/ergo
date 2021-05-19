//! ABI-stable stream trait.

use super::{Context, Poll};
use abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RBox, ROption, Tuple2},
    StableAbi,
};

#[sabi_trait]
pub trait Stream: Send {
    type Item;

    fn poll_next(&mut self, cx: Context) -> Poll<ROption<Self::Item>>;

    fn size_hint(&self) -> Tuple2<usize, ROption<usize>> {
        (0, ROption::RNone).into()
    }
}

/// A boxed, ABI-stable stream.
#[derive(StableAbi)]
#[repr(C)]
pub struct BoxStream<'a, T> {
    inner: Stream_TO<'a, RBox<()>, T>,
}

impl<'a, T> BoxStream<'a, T> {
    /// Create a new boxed stream.
    pub fn new<Strm: futures::stream::Stream<Item = T> + Send + 'a>(strm: Strm) -> Self {
        BoxStream {
            inner: Stream_TO::from_value(strm, TU_Opaque),
        }
    }
}

impl<Strm: futures::stream::Stream + Send> Stream for Strm {
    type Item = Strm::Item;

    fn poll_next(&mut self, cx: Context) -> Poll<ROption<Self::Item>> {
        let waker = cx.into_waker();
        let mut ctx = std::task::Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these streams will _only_ be within a Box (and
        // are moved into the box), so we guarantee that the stream will not be moved out.
        futures::stream::Stream::poll_next(unsafe { std::pin::Pin::new_unchecked(self) }, &mut ctx)
            .map(|v| v.into())
            .into()
    }

    fn size_hint(&self) -> Tuple2<usize, ROption<usize>> {
        let (min, max) = futures::stream::Stream::size_hint(self);
        (min, max.into()).into()
    }
}

impl<'a, T> futures::stream::Stream for BoxStream<'a, T> {
    type Item = T;

    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Option<Self::Item>> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
            .poll_next(Context::new(cx))
            .into_poll()
            .map(|v| v.into())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (min, max) = self.inner.size_hint().into_tuple();
        (min, max.into())
    }
}
