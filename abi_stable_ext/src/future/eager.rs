//! Eager futures, which evaluate immediately if possible.

use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

/// A future container which supports eager evaluation of futures.
#[derive(Debug, Clone)]
pub enum Eager<Fut: Future> {
    Ready(Fut::Output),
    Pending(Fut),
}

/// Provide a Future interface over an Eagerly.
///
/// Use Ext::as_future to create this type.
pub struct IntoFuture<Fut: Future>(pub(crate) Option<Eager<Fut>>);

impl<Fut: Future> std::fmt::Debug for IntoFuture<Fut>
where
    Fut: std::fmt::Debug,
    Fut::Output: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("IntoFuture").field(&self.0).finish()
    }
}

impl<Fut: Future> Clone for IntoFuture<Fut>
where
    Fut: Clone,
    Fut::Output: Clone,
{
    fn clone(&self) -> Self {
        IntoFuture(self.0.clone())
    }
}

impl<Fut: Future + std::marker::Unpin> Future for IntoFuture<Fut>
where
    Fut::Output: std::marker::Unpin,
{
    type Output = Fut::Output;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let me = &mut *self;
        match me.0.take() {
            None => panic!("polling already-completed future"),
            Some(Eager::Ready(v)) => Poll::Ready(v),
            Some(Eager::Pending(mut fut)) => {
                let ret = Future::poll(Pin::new(&mut fut), cx);
                if ret.is_pending() {
                    me.0 = Some(Eager::Pending(fut));
                }
                ret
            }
        }
    }
}

impl<Fut: Future> Eager<Fut> {
    /// Convert the Eager into a type that implements Future.
    pub fn into_future(self) -> IntoFuture<Fut> {
        IntoFuture(Some(self))
    }

    pub async fn then<F, NewFut>(self, f: F) -> Eager<futures::future::Then<Fut, NewFut, F>>
    where
        F: FnOnce(Fut::Output) -> NewFut,
        NewFut: Future,
    {
        match self {
            Eager::Ready(v) => Eager::Ready(f(v).await),
            Eager::Pending(v) => Eager::Pending(futures::future::FutureExt::then(v, f)),
        }
    }

    pub fn then_eager<F, NewFut>(
        self,
        f: F,
    ) -> Eager<futures::future::Either<NewFut, futures::future::Then<Fut, IntoFuture<NewFut>, F>>>
    where
        F: FnOnce(Fut::Output) -> IntoFuture<NewFut>,
        NewFut: Future + std::marker::Unpin,
        NewFut::Output: std::marker::Unpin,
    {
        match self {
            Eager::Ready(v) => match f(v).0 {
                Some(Eager::Ready(v)) => Eager::Ready(v),
                Some(Eager::Pending(v)) => {
                    Eager::Pending(futures::future::FutureExt::left_future(v))
                }
                None => panic!("already executed future"),
            },
            Eager::Pending(v) => Eager::Pending(futures::future::FutureExt::right_future(
                futures::future::FutureExt::then(v, f),
            )),
        }
    }

    pub fn map<U, F>(self, f: F) -> Eager<futures::future::Map<Fut, F>>
    where
        F: FnOnce(Fut::Output) -> U,
    {
        match self {
            Eager::Ready(v) => Eager::Ready(f(v)),
            Eager::Pending(v) => Eager::Pending(futures::future::FutureExt::map(v, f)),
        }
    }

    pub fn shared(self) -> Eager<futures::future::Shared<Fut>>
    where
        Fut::Output: Clone,
    {
        match self {
            Eager::Ready(v) => Eager::Ready(v),
            Eager::Pending(v) => Eager::Pending(futures::future::FutureExt::shared(v)),
        }
    }

    pub fn boxed<'a>(self) -> Eager<futures::future::BoxFuture<'a, Fut::Output>>
    where
        Fut: Send + 'a,
    {
        match self {
            Eager::Ready(v) => Eager::Ready(v),
            Eager::Pending(v) => Eager::Pending(futures::future::FutureExt::boxed(v)),
        }
    }
}

impl<T, E, Fut: Future<Output = Result<T, E>>> Eager<Fut> {
    pub async fn and_then<F, NewFut, U>(
        self,
        f: F,
    ) -> Eager<futures::future::AndThen<Fut, NewFut, F>>
    where
        F: FnOnce(T) -> NewFut,
        NewFut: Future<Output = Result<U, E>>,
    {
        match self {
            Eager::Ready(Ok(v)) => Eager::Ready(f(v).await),
            Eager::Ready(Err(v)) => Eager::Ready(Err(v)),
            Eager::Pending(v) => Eager::Pending(futures::future::TryFutureExt::and_then(v, f)),
        }
    }

    pub fn and_then_eager<F, NewFut, U>(
        self,
        f: F,
    ) -> Eager<futures::future::Either<NewFut, futures::future::AndThen<Fut, IntoFuture<NewFut>, F>>>
    where
        F: FnOnce(T) -> IntoFuture<NewFut>,
        NewFut: Future<Output = Result<U, E>> + std::marker::Unpin,
        U: std::marker::Unpin,
        E: std::marker::Unpin,
    {
        match self {
            Eager::Ready(Ok(v)) => match f(v).0 {
                Some(Eager::Ready(v)) => Eager::Ready(v),
                Some(Eager::Pending(p)) => {
                    Eager::Pending(futures::future::FutureExt::left_future(p))
                }
                None => panic!("already executed future"),
            },
            Eager::Ready(Err(v)) => Eager::Ready(Err(v)),
            Eager::Pending(v) => Eager::Pending(futures::future::FutureExt::right_future(
                futures::future::TryFutureExt::and_then(v, f),
            )),
        }
    }

    pub async fn or_else<F, NewFut, O>(self, f: F) -> Eager<futures::future::OrElse<Fut, NewFut, F>>
    where
        F: FnOnce(E) -> NewFut,
        NewFut: Future<Output = Result<T, O>>,
    {
        match self {
            Eager::Ready(Ok(v)) => Eager::Ready(Ok(v)),
            Eager::Ready(Err(v)) => Eager::Ready(f(v).await),
            Eager::Pending(v) => Eager::Pending(futures::future::TryFutureExt::or_else(v, f)),
        }
    }

    pub fn map_ok<U, F>(self, f: F) -> Eager<futures::future::MapOk<Fut, F>>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Eager::Ready(v) => Eager::Ready(v.map(f)),
            Eager::Pending(v) => Eager::Pending(futures::future::TryFutureExt::map_ok(v, f)),
        }
    }

    pub fn map_err<O, F>(self, f: F) -> Eager<futures::future::MapErr<Fut, F>>
    where
        F: FnOnce(E) -> O,
    {
        match self {
            Eager::Ready(v) => Eager::Ready(v.map_err(f)),
            Eager::Pending(v) => Eager::Pending(futures::future::TryFutureExt::map_err(v, f)),
        }
    }
}

impl<Fut: Future> From<Fut> for Eager<Fut> {
    fn from(fut: Fut) -> Self {
        Eager::Pending(fut)
    }
}
