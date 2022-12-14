//! IO utilities for the runtime.

use crate::abi_stable::{
    future as abi_future, sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RBox, RResult, RSlice, RSliceMut},
    StableAbi,
};
use crate::error::DiagnosticInfo;
use futures::future::{BoxFuture, Future, FutureExt, TryFutureExt};
pub use futures::io::{AllowStdIo, AsyncRead, AsyncWrite};
use std::cmp;
use std::pin::Pin;
use std::task::{Context, Poll};

pub type Result<T> = std::io::Result<T>;

/// Types that support adding context information.
pub trait AddContext: Sized {
    fn add_context_with<S: AsRef<str>, F: FnOnce() -> S>(self, context: F) -> Self;

    fn add_context<S: AsRef<str>>(self, context: S) -> Self {
        self.add_context_with(|| context)
    }

    fn add_context_str<S: ToString>(self, context: S) -> Self {
        self.add_context_with(|| context.to_string())
    }
}

impl AddContext for std::io::Error {
    fn add_context_with<S: AsRef<str>, F: FnOnce() -> S>(self, context: F) -> Self {
        Self::new(self.kind(), format!("{}: {}", context().as_ref(), self))
    }
}

impl<T> AddContext for std::io::Result<T> {
    fn add_context_with<S: AsRef<str>, F: FnOnce() -> S>(self, context: F) -> Self {
        self.map_err(|e| e.add_context_with(context))
    }
}

/// Wrap tokio::io::AsyncRead/AsyncWrite types.
#[pin_project::pin_project]
pub struct Tokio<T> {
    #[pin]
    inner: T,
}

impl<T> Tokio<T> {
    pub fn new(inner: T) -> Self {
        Tokio { inner }
    }
}

impl<T: tokio::io::AsyncRead> AsyncRead for Tokio<T> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context, buf: &mut [u8]) -> Poll<Result<usize>> {
        let proj = self.project();
        let mut read_buf = tokio::io::ReadBuf::new(buf);
        match tokio::io::AsyncRead::poll_read(proj.inner, cx, &mut read_buf) {
            Poll::Ready(Ok(())) => Poll::Ready(Ok(read_buf.filled().len())),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<T: tokio::io::AsyncWrite> AsyncWrite for Tokio<T> {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, src: &[u8]) -> Poll<Result<usize>> {
        let proj = self.project();
        tokio::io::AsyncWrite::poll_write(proj.inner, cx, src)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let proj = self.project();
        tokio::io::AsyncWrite::poll_flush(proj.inner, cx)
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        let proj = self.project();
        tokio::io::AsyncWrite::poll_shutdown(proj.inner, cx)
    }
}

pub struct Copier<'a, R: ?Sized, W: ?Sized> {
    reader: &'a mut R,
    read_done: bool,
    writer: &'a mut W,
    pos: usize,
    cap: usize,
    amt: u64,
    buf: Box<[u8]>,
    immediate_flush: bool,
    need_flush: bool,
}

pub fn copy<'a, R, W>(reader: &'a mut R, writer: &'a mut W) -> Copier<'a, R, W>
where
    R: ?Sized + AsyncRead + std::marker::Unpin,
    W: ?Sized + AsyncWrite + std::marker::Unpin,
{
    Copier {
        reader,
        read_done: false,
        writer,
        amt: 0,
        pos: 0,
        cap: 0,
        buf: vec![0; 2048].into_boxed_slice(),
        immediate_flush: false,
        need_flush: false,
    }
}

pub fn copy_interactive<'a, R, W>(reader: &'a mut R, writer: &'a mut W) -> Copier<'a, R, W>
where
    R: ?Sized + AsyncRead + std::marker::Unpin,
    W: ?Sized + AsyncWrite + std::marker::Unpin,
{
    Copier {
        reader,
        read_done: false,
        writer,
        amt: 0,
        pos: 0,
        cap: 0,
        buf: vec![0; 2048].into_boxed_slice(),
        immediate_flush: true,
        need_flush: false,
    }
}

macro_rules! ready {
    ( $v:expr ) => {
        match $v {
            std::task::Poll::Ready(v) => v,
            std::task::Poll::Pending => return std::task::Poll::Pending,
        }
    };
}

impl<'a, R: ?Sized, W: ?Sized> Future for Copier<'a, R, W>
where
    R: AsyncRead + std::marker::Unpin,
    W: AsyncWrite + std::marker::Unpin,
{
    type Output = Result<u64>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        loop {
            // If we need to flush, do it first.
            if self.need_flush {
                let me = &mut *self;
                ready!(Pin::new(&mut *me.writer).poll_flush(cx))?;
                me.need_flush = false;
            }

            // If our buffer is empty, then we need to read some data to
            // continue.
            if self.pos == self.cap && !self.read_done {
                let me = &mut *self;
                let n = ready!(Pin::new(&mut *me.reader).poll_read(cx, &mut me.buf))?;
                if n == 0 {
                    self.read_done = true;
                } else {
                    self.pos = 0;
                    self.cap = n;
                }
            }

            // If our buffer has some data, let's write it out!
            while self.pos < self.cap {
                let me = &mut *self;
                let i = ready!(Pin::new(&mut *me.writer).poll_write(cx, &me.buf[me.pos..me.cap]))?;
                if i == 0 {
                    return Poll::Ready(Err(std::io::Error::new(
                        std::io::ErrorKind::WriteZero,
                        "write zero byte into writer",
                    )));
                } else {
                    if self.immediate_flush {
                        let me = &mut *self;
                        me.need_flush = true;
                    }
                    self.pos += i;
                    self.amt += i as u64;
                }
            }

            // If we've written all the data and we've seen EOF, flush out the
            // data and finish the transfer.
            if self.pos == self.cap && self.read_done {
                let me = &mut *self;
                ready!(Pin::new(&mut *me.writer).poll_flush(cx))?;
                return Poll::Ready(Ok(self.amt));
            }
        }
    }
}

pub trait AsyncReadExt {
    fn once(self) -> Once<Self>
    where
        Self: AsyncRead + Sized,
    {
        Once(Some(self))
    }
}

impl<T: AsyncRead> AsyncReadExt for T {}

pub struct Once<T>(Option<T>);

impl<T: AsyncRead + std::marker::Unpin> AsyncRead for Once<T> {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        buf: &mut [u8],
    ) -> Poll<Result<usize>> {
        let me = &mut *self;
        Poll::Ready(Ok(match &mut me.0 {
            None => 0,
            Some(v) => {
                let ret = ready!(Pin::new(v).poll_read(cx, buf))?;
                me.0 = None;
                ret
            }
        }))
    }
}

#[sabi_trait]
trait AsyncReadInterface: Send + Unpin {
    #[sabi(last_prefix_field)]
    fn poll_read(
        &mut self,
        cx: abi_future::Context,
        buf: RSliceMut<u8>,
    ) -> abi_future::Poll<RResult<usize, crate::Error>>;
}

#[derive(StableAbi)]
#[repr(C)]
pub struct BoxAsyncRead<'a> {
    inner: AsyncReadInterface_TO<'a, RBox<()>>,
}

impl<'a> BoxAsyncRead<'a> {
    pub fn new<R: AsyncRead + Send + std::marker::Unpin + 'a>(r: R) -> Self {
        BoxAsyncRead {
            inner: AsyncReadInterface_TO::from_value(r, TD_Opaque),
        }
    }
}

impl<R: AsyncRead + Send + std::marker::Unpin> AsyncReadInterface for R {
    fn poll_read(
        &mut self,
        cx: abi_future::Context,
        mut buf: RSliceMut<u8>,
    ) -> abi_future::Poll<RResult<usize, crate::Error>> {
        let waker = cx.into_waker();
        let mut ctx = Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these values will _only_ be within a Box (and
        // are moved into the box), so we guarantee that it will not be moved out.
        AsyncRead::poll_read(
            unsafe { Pin::new_unchecked(self) },
            &mut ctx,
            buf.as_mut_slice(),
        )
        .map(|v| v.into_diagnostic().map_err(|e| e.into()).into())
        .into()
    }
}

impl<'a> AsyncRead for BoxAsyncRead<'a> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context, buf: &mut [u8]) -> Poll<Result<usize>> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
            .poll_read(abi_future::Context::new(cx), buf.into())
            .into_poll()
            .map(|r| r.into_result().map_err(|e| e.into()))
    }
}

#[sabi_trait]
trait AsyncWriteInterface: Send {
    fn poll_write(
        &mut self,
        cx: abi_future::Context,
        src: RSlice<u8>,
    ) -> abi_future::Poll<RResult<usize, crate::Error>>;

    fn poll_flush(
        &mut self,
        cx: abi_future::Context,
    ) -> abi_future::Poll<RResult<(), crate::Error>>;

    #[sabi(last_prefix_field)]
    fn poll_close(
        &mut self,
        cx: abi_future::Context,
    ) -> abi_future::Poll<RResult<(), crate::Error>>;
}

#[derive(StableAbi)]
#[repr(C)]
#[pin_project::pin_project]
pub struct BoxAsyncWrite<'a> {
    inner: AsyncWriteInterface_TO<'a, RBox<()>>,
}

impl<'a> BoxAsyncWrite<'a> {
    pub fn new<W: AsyncWrite + Send + 'a>(w: W) -> Self {
        BoxAsyncWrite {
            inner: AsyncWriteInterface_TO::from_value(w, TD_Opaque),
        }
    }
}

impl<W: AsyncWrite + Send> AsyncWriteInterface for W {
    fn poll_write(
        &mut self,
        cx: abi_future::Context,
        src: RSlice<u8>,
    ) -> abi_future::Poll<RResult<usize, crate::Error>> {
        let waker = cx.into_waker();
        let mut ctx = Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these values will _only_ be within a Box (and
        // are moved into the box), so we guarantee that it will not be moved out.
        AsyncWrite::poll_write(
            unsafe { Pin::new_unchecked(self) },
            &mut ctx,
            src.as_slice(),
        )
        .map(|v| v.into_diagnostic().map_err(|e| e.into()).into())
        .into()
    }

    fn poll_flush(
        &mut self,
        cx: abi_future::Context,
    ) -> abi_future::Poll<RResult<(), crate::Error>> {
        let waker = cx.into_waker();
        let mut ctx = Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these values will _only_ be within a Box (and
        // are moved into the box), so we guarantee that it will not be moved out.
        AsyncWrite::poll_flush(unsafe { Pin::new_unchecked(self) }, &mut ctx)
            .map(|v| v.into_diagnostic().map_err(|e| e.into()).into())
            .into()
    }

    fn poll_close(
        &mut self,
        cx: abi_future::Context,
    ) -> abi_future::Poll<RResult<(), crate::Error>> {
        let waker = cx.into_waker();
        let mut ctx = Context::from_waker(&waker);
        // Safe to use Pin::new_unchecked because these values will _only_ be within a Box (and
        // are moved into the box), so we guarantee that it will not be moved out.
        AsyncWrite::poll_close(unsafe { Pin::new_unchecked(self) }, &mut ctx)
            .map(|v| v.into_diagnostic().map_err(|e| e.into()).into())
            .into()
    }
}

impl<'a> AsyncWrite for BoxAsyncWrite<'a> {
    fn poll_write(self: Pin<&mut Self>, cx: &mut Context<'_>, src: &[u8]) -> Poll<Result<usize>> {
        self.project()
            .inner
            .poll_write(abi_future::Context::new(cx), src.into())
            .into_poll()
            .map(|r| r.into_result().map_err(|e| e.into()))
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        self.project()
            .inner
            .poll_flush(abi_future::Context::new(cx))
            .into_poll()
            .map(|r| r.into_result().map_err(|e| e.into()))
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        self.project()
            .inner
            .poll_close(abi_future::Context::new(cx))
            .into_poll()
            .map(|r| r.into_result().map_err(|e| e.into()))
    }
}

// The following is adapted from tokio::io::blocking.
/// `T` should not implement _both_ Read and Write.
#[derive(Debug)]
pub struct Blocking<T> {
    inner: Option<T>,
    state: State<T>,
    /// `true` if the lower IO layer needs flushing
    need_flush: bool,
}

#[derive(Debug)]
struct Buf {
    buf: Vec<u8>,
    pos: usize,
}

const MAX_BUF: usize = 16 * 1024;

enum State<T> {
    Idle(Option<Buf>),
    Busy(BoxFuture<'static, Result<(Result<usize>, Buf, T)>>),
}

impl<T> std::fmt::Debug for State<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "State::")?;
        match self {
            State::Idle(v) => write!(f, "Idle({:?})", v),
            State::Busy(_) => write!(f, "Busy"),
        }
    }
}

impl<T> Blocking<T> {
    /// Create a new blocking instance which may support AsyncRead/AsyncWrite.
    pub fn new(inner: T) -> Blocking<T> {
        Blocking {
            inner: Some(inner),
            state: State::Idle(Some(Buf::with_capacity(0))),
            need_flush: false,
        }
    }
}

macro_rules! uninterruptibly {
    ($e:expr) => {{
        loop {
            match $e {
                Err(ref e) if e.kind() == std::io::ErrorKind::Interrupted => {}
                res => break res,
            }
        }
    }};
}

impl Buf {
    pub fn with_capacity(n: usize) -> Buf {
        Buf {
            buf: Vec::with_capacity(n),
            pos: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.buf.len() - self.pos
    }

    pub fn copy_to(&mut self, dst: &mut [u8]) -> usize {
        let n = cmp::min(self.len(), dst.len());
        dst[..n].copy_from_slice(&self.bytes()[..n]);
        self.pos += n;

        if self.pos == self.buf.len() {
            self.buf.truncate(0);
            self.pos = 0;
        }

        n
    }

    pub fn copy_from(&mut self, src: &[u8]) -> usize {
        assert!(self.is_empty());

        let n = cmp::min(src.len(), MAX_BUF);

        self.buf.extend_from_slice(&src[..n]);
        n
    }

    pub fn bytes(&self) -> &[u8] {
        &self.buf[self.pos..]
    }

    pub fn ensure_capacity_for(&mut self, bytes: &[u8]) {
        assert!(self.is_empty());

        let len = cmp::min(bytes.len(), MAX_BUF);

        if self.buf.len() < len {
            self.buf.reserve(len - self.buf.len());
        }

        unsafe {
            self.buf.set_len(len);
        }
    }

    pub fn read_from<T: std::io::Read>(&mut self, rd: &mut T) -> Result<usize> {
        let res = uninterruptibly!(rd.read(&mut self.buf));

        if let Ok(n) = res {
            self.buf.truncate(n);
        } else {
            self.buf.clear();
        }

        assert_eq!(self.pos, 0);

        res
    }

    pub fn write_to<T: std::io::Write>(&mut self, wr: &mut T) -> Result<()> {
        assert_eq!(self.pos, 0);

        // `write_all` already ignores interrupts
        let res = wr.write_all(&self.buf);
        self.buf.clear();
        res
    }
}

impl<T> futures::io::AsyncRead for Blocking<T>
where
    T: std::io::Read + std::marker::Unpin + Send + Sync + 'static,
{
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        dst: &mut [u8],
    ) -> Poll<Result<usize>> {
        loop {
            match self.state {
                State::Idle(ref mut buf_cell) => {
                    let mut buf = buf_cell.take().unwrap();

                    if !buf.is_empty() {
                        let n = buf.copy_to(dst);
                        *buf_cell = Some(buf);
                        return Poll::Ready(Ok(n));
                    }

                    buf.ensure_capacity_for(dst);
                    let mut inner = self.inner.take().unwrap();

                    self.state = State::Busy(
                        crate::Context::global()
                            .task
                            .spawn_blocking(move || {
                                let res = buf.read_from(&mut inner);
                                (res, buf, inner)
                            })
                            .map_err(|e| e.into())
                            .boxed(),
                    );
                }
                State::Busy(ref mut rx) => {
                    let (res, mut buf, inner) = match Pin::new(rx).poll(cx) {
                        Poll::Pending => {
                            return Poll::Pending;
                        }
                        Poll::Ready(v) => v,
                    }?;
                    self.inner = Some(inner);

                    match res {
                        Ok(_) => {
                            let n = buf.copy_to(dst);
                            self.state = State::Idle(Some(buf));
                            return Poll::Ready(Ok(n));
                        }
                        Err(e) => {
                            assert!(buf.is_empty());

                            self.state = State::Idle(Some(buf));
                            return Poll::Ready(Err(e.into()));
                        }
                    }
                }
            }
        }
    }
}

impl<T> futures::io::AsyncWrite for Blocking<T>
where
    T: std::io::Write + std::marker::Unpin + Send + Sync + 'static,
{
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        src: &[u8],
    ) -> Poll<Result<usize>> {
        loop {
            match self.state {
                State::Idle(ref mut buf_cell) => {
                    let mut buf = buf_cell.take().unwrap();

                    assert!(buf.is_empty());

                    let n = buf.copy_from(src);
                    let mut inner = self.inner.take().unwrap();

                    self.state = State::Busy(
                        crate::Context::global()
                            .task
                            .spawn_blocking(move || {
                                let n = buf.len();
                                let res = buf.write_to(&mut inner).map(|_| n);

                                (res, buf, inner)
                            })
                            .map_err(|e| e.into())
                            .boxed(),
                    );
                    self.need_flush = true;

                    return Poll::Ready(Ok(n));
                }
                State::Busy(ref mut rx) => {
                    let (res, buf, inner) = match Pin::new(rx).poll(cx) {
                        Poll::Pending => return Poll::Pending,
                        Poll::Ready(v) => v,
                    }?;
                    self.state = State::Idle(Some(buf));
                    self.inner = Some(inner);

                    // If error, return
                    res?;
                }
            }
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<()>> {
        loop {
            let need_flush = self.need_flush;
            match self.state {
                // The buffer is not used here
                State::Idle(ref mut buf_cell) => {
                    if need_flush {
                        let buf = buf_cell.take().unwrap();
                        let mut inner = self.inner.take().unwrap();

                        self.state = State::Busy(
                            crate::Context::global()
                                .task
                                .spawn_blocking(move || {
                                    let res = inner.flush().map(|_| 0);
                                    (res, buf, inner)
                                })
                                .map_err(|e| e.into())
                                .boxed(),
                        );

                        self.need_flush = false;
                    } else {
                        return Poll::Ready(Ok(()));
                    }
                }
                State::Busy(ref mut rx) => {
                    let (res, buf, inner) = match Pin::new(rx).poll(cx) {
                        Poll::Pending => return Poll::Pending,
                        Poll::Ready(v) => v,
                    }?;
                    self.state = State::Idle(Some(buf));
                    self.inner = Some(inner);

                    // If error, return
                    res?;
                }
            }
        }
    }

    fn poll_close(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Result<()>> {
        Poll::Ready(Ok(()))
    }
}
