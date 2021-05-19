//! The Stream type.
//!
//! Currently unused.

use crate as ergo_runtime;
use crate::abi_stable::{future::BoxFuture, std_types::ROption, type_erase::Erased, StableAbi};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies};

/// A stream type.
///
/// This differs from `Iter` in that you can partially consume streams and retain correct
/// dependency tracking.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Stream(StreamInner);

pub trait StreamInterface: Clone + std::fmt::Debug + Send + Sync {
    fn next(self) -> Option<(Value, TypedValue<Stream>)>;

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

impl<I: StreamInterface> StreamAbi for I {
    fn next(self) -> ROption<abi_stable::std_types::Tuple2<Value, TypedValue<Stream>>> {
        StreamInterface::next(self).map(|v| v.into()).into()
    }

    fn size_hint(&self) -> abi_stable::std_types::Tuple2<usize, ROption<usize>> {
        let (min, max) = StreamInterface::size_hint(self);
        (min, max.into()).into()
    }
}

impl Stream {
    /// Create a new Stream.
    pub fn new<I: StreamInterface + 'static>(interface: I) -> Self {
        Stream(StreamInner::Inner(StreamAbi_TO::from_value(
            interface, TU_Opaque,
        )))
    }
}

#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
enum StreamInner {
    Inner(StreamAbi_TO<'static, RBox<()>>),
    Pending(Value, TypedValue<Stream>),
    Empty,
}

#[sabi_trait]
trait StreamAbi: Clone + Debug + Send + Sync {
    fn next(self) -> ROption<abi_stable::std_types::Tuple2<Value, TypedValue<Stream>>>;

    #[sabi(last_prefix_field)]
    fn size_hint(&self) -> abi_stable::std_types::Tuple2<usize, ROption<usize>> {
        (0, ROption::RNone).into()
    }
}

impl StreamInner {
    fn take(&mut self) -> Self {
        std::mem::replace(self, StreamInner::Empty)
    }
}

impl futures::stream::Stream for Stream {
    type Item = crate::Result<Value>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Option<Self::Item>> {
        let me = &mut *self;
        loop {
            match me.0.take() {
                StreamInner::Inner(s) => match s.next().into_option() {
                    None => break std::task::Poll::Ready(None),
                    Some(v) => {
                        let (a, b) = v.into_tuple();
                        me.0 = StreamInner::Pending(a, b);
                    }
                },
                StreamInner::Pending(v, mut next) => {
                    match futures::Future::poll(std::pin::Pin::new(&mut next), cx) {
                        std::task::Poll::Pending => {
                            me.0 = StreamInner::Pending(v, next);
                            break std::task::Poll::Pending;
                        }
                        std::task::Poll::Ready(r) => match r {
                            Err(e) => break std::task::Poll::Ready(Some(Err(e))),
                            Ok(new_me) => {
                                *me = new_me.owned();
                                break std::task::Poll::Ready(Some(Ok(v)));
                            }
                        },
                    }
                }
                StreamInner::Empty => break std::task::Poll::Ready(None),
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            StreamInner::Inner(i) => {
                let (min, max) = i.size_hint().into_tuple();
                (min, max.into_option())
            }
            StreamInner::Empty => (0, Some(0)),
            _ => (0, None),
        }
    }
}
