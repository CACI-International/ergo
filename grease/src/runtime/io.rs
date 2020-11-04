//! IO utilities for the runtime.

use crate::Error;
use futures::future::{BoxFuture, Future, FutureExt};
use std::cmp;
use std::pin::Pin;
use std::task::{Context, Poll};

/// Types which can asyncronously read data.
pub trait AsyncRead {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
        buf: &mut [u8],
    ) -> Poll<Result<usize, Error>>;
}

/// Types which can asyncronously write data.
pub trait AsyncWrite {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
        buf: &[u8],
    ) -> Poll<Result<usize, Error>>;

    fn poll_flush(
        self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
    ) -> Poll<Result<(), Error>>;

    fn poll_shutdown(
        self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
    ) -> Poll<Result<(), Error>>;
}

pub struct Copy<'a, R: ?Sized, W: ?Sized> {
    reader: &'a mut R,
    read_done: bool,
    writer: &'a mut W,
    pos: usize,
    cap: usize,
    amt: u64,
    buf: Box<[u8]>,
    task: &'a super::TaskManager,
    immediate_flush: bool,
    need_flush: bool,
}

pub fn copy<'a, R, W>(
    task: &'a super::TaskManager,
    reader: &'a mut R,
    writer: &'a mut W,
) -> Copy<'a, R, W>
where
    R: ?Sized + AsyncRead + std::marker::Unpin,
    W: ?Sized + AsyncWrite + std::marker::Unpin,
{
    Copy {
        reader,
        read_done: false,
        writer,
        amt: 0,
        pos: 0,
        cap: 0,
        buf: vec![0; 2048].into_boxed_slice(),
        task,
        immediate_flush: false,
        need_flush: false,
    }
}

pub fn copy_interactive<'a, R, W>(
    task: &'a super::TaskManager,
    reader: &'a mut R,
    writer: &'a mut W,
) -> Copy<'a, R, W>
where
    R: ?Sized + AsyncRead + std::marker::Unpin,
    W: ?Sized + AsyncWrite + std::marker::Unpin,
{
    Copy {
        reader,
        read_done: false,
        writer,
        amt: 0,
        pos: 0,
        cap: 0,
        buf: vec![0; 2048].into_boxed_slice(),
        task,
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

impl<'a, R: ?Sized, W: ?Sized> Future for Copy<'a, R, W>
where
    R: AsyncRead + std::marker::Unpin,
    W: AsyncWrite + std::marker::Unpin,
{
    type Output = Result<u64, Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        loop {
            // If we need to flush, do it first.
            if self.need_flush {
                let me = &mut *self;
                ready!(Pin::new(&mut *me.writer).poll_flush(cx, &me.task))?;
                me.need_flush = false;
            }

            // If our buffer is empty, then we need to read some data to
            // continue.
            if self.pos == self.cap && !self.read_done {
                let me = &mut *self;
                let n = ready!(Pin::new(&mut *me.reader).poll_read(cx, &me.task, &mut me.buf))?;
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
                let i = ready!(Pin::new(&mut *me.writer).poll_write(
                    cx,
                    &me.task,
                    &me.buf[me.pos..me.cap]
                ))?;
                if i == 0 {
                    return Poll::Ready(Err(std::io::Error::new(
                        std::io::ErrorKind::WriteZero,
                        "write zero byte into writer",
                    )
                    .into()));
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
                ready!(Pin::new(&mut *me.writer).poll_flush(cx, &me.task))?;
                return Poll::Ready(Ok(self.amt));
            }
        }
    }
}

impl<T: ?Sized + AsyncRead + std::marker::Unpin> AsyncRead for &'_ mut T {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
        buf: &mut [u8],
    ) -> Poll<Result<usize, Error>> {
        Pin::new(&mut **self).poll_read(cx, task, buf)
    }
}

/// Implements the grease AsyncRead/AsyncWrite when T implements the tokio AsyncRead/AsyncWrite.
pub struct TokioWrapped<T>(T);

/// Wrap a tokio AsyncRead and/or AsyncWrite value.
pub fn wrap<T>(v: T) -> TokioWrapped<T> {
    TokioWrapped(v)
}

impl<T: tokio::io::AsyncRead + std::marker::Unpin> AsyncRead for TokioWrapped<T> {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        _task: &super::TaskManager,
        buf: &mut [u8],
    ) -> Poll<Result<usize, Error>> {
        tokio::io::AsyncRead::poll_read(Pin::new(&mut self.0), cx, buf)
            .map(|r| r.map_err(|e| e.into()))
    }
}

impl<T: tokio::io::AsyncWrite + std::marker::Unpin> AsyncWrite for TokioWrapped<T> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        _task: &super::TaskManager,
        buf: &[u8],
    ) -> Poll<Result<usize, Error>> {
        tokio::io::AsyncWrite::poll_write(Pin::new(&mut self.0), cx, buf)
            .map(|r| r.map_err(|e| e.into()))
    }

    fn poll_flush(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        _task: &super::TaskManager,
    ) -> Poll<Result<(), Error>> {
        tokio::io::AsyncWrite::poll_flush(Pin::new(&mut self.0), cx)
            .map(|r| r.map_err(|e| e.into()))
    }

    fn poll_shutdown(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
        _task: &super::TaskManager,
    ) -> Poll<Result<(), Error>> {
        tokio::io::AsyncWrite::poll_shutdown(Pin::new(&mut self.0), cx)
            .map(|r| r.map_err(|e| e.into()))
    }
}

pub trait AsyncReadExt {
    fn read_exact<'a>(
        &'a mut self,
        task: &'a super::TaskManager,
        buf: &'a mut [u8],
    ) -> ReadExact<'a, Self>
    where
        Self: AsyncRead + std::marker::Unpin,
    {
        ReadExact(self, task, buf, 0)
    }

    fn read_to_end<'a>(
        &'a mut self,
        task: &'a super::TaskManager,
        buf: &'a mut Vec<u8>,
    ) -> ReadToEnd<'a, Self>
    where
        Self: AsyncRead + std::marker::Unpin,
    {
        ReadToEnd(self, task, buf)
    }

    fn read_to_string<'a>(
        &'a mut self,
        task: &'a super::TaskManager,
        s: &'a mut String,
    ) -> ReadToString<'a, Self>
    where
        Self: AsyncRead + std::marker::Unpin,
    {
        ReadToString(self, task, s)
    }

    fn take(self, limit: u64) -> Take<Self>
    where
        Self: AsyncRead + Sized,
    {
        Take(self, limit)
    }

    fn once(self) -> Once<Self>
    where
        Self: AsyncRead + Sized,
    {
        Once(Some(self))
    }
}

impl<T: AsyncRead> AsyncReadExt for T {}

pub struct ReadExact<'a, T: ?Sized>(&'a mut T, &'a super::TaskManager, &'a mut [u8], usize);

impl<'a, T: ?Sized + AsyncRead + std::marker::Unpin> Future for ReadExact<'a, T> {
    type Output = Result<usize, Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        loop {
            let me = &mut *self;
            let bytes = ready!(Pin::new(&mut me.0).poll_read(cx, &me.1, &mut me.2[me.3..]))?;
            if bytes == 0 {
                return Poll::Ready(Ok(me.3));
            }
            me.3 += bytes;
        }
    }
}

pub struct ReadToEnd<'a, T: ?Sized>(&'a mut T, &'a super::TaskManager, &'a mut Vec<u8>);

impl<'a, T: ?Sized + AsyncRead + std::marker::Unpin> Future for ReadToEnd<'a, T> {
    type Output = Result<usize, Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let mut buf: [u8; 2048] = [0; 2048];
        loop {
            let me = &mut *self;
            let bytes = ready!(Pin::new(&mut me.0).poll_read(cx, &me.1, &mut buf))?;
            if bytes == 0 {
                return Poll::Ready(Ok(self.2.len()));
            }
            self.2.extend(&buf[..bytes]);
        }
    }
}

pub struct ReadToString<'a, T: ?Sized>(&'a mut T, &'a super::TaskManager, &'a mut String);

impl<'a, T: ?Sized + AsyncRead + std::marker::Unpin> Future for ReadToString<'a, T> {
    type Output = Result<usize, Error>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let mut buf: [u8; 2048] = [0; 2048];
        loop {
            let me = &mut *self;
            let bytes = ready!(Pin::new(&mut me.0).poll_read(cx, &me.1, &mut buf))?;
            if bytes == 0 {
                return Poll::Ready(Ok(self.2.len()));
            }
            let s = std::str::from_utf8(&buf[..bytes])?;
            self.2.push_str(s);
        }
    }
}

pub struct Take<T>(T, u64);

impl<T: AsyncRead> AsyncRead for Take<T> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
        buf: &mut [u8],
    ) -> Poll<Result<usize, Error>> {
        let size = std::cmp::min(buf.len() as u64, self.1);
        // Safe to get reference from Pin because we own the data and won't be moving it.
        let me = unsafe { self.get_unchecked_mut() };
        let count = ready!(unsafe { Pin::new_unchecked(&mut me.0) }.poll_read(
            cx,
            task,
            &mut buf[..size as usize]
        ))?;
        me.1 -= count as u64;
        Poll::Ready(Ok(count as usize))
    }
}

pub struct Once<T>(Option<T>);

impl<T: AsyncRead> AsyncRead for Once<T> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context,
        task: &super::TaskManager,
        buf: &mut [u8],
    ) -> Poll<Result<usize, Error>> {
        // Safe to get reference from Pin because we own the data and won't be moving it.
        let me = unsafe { self.get_unchecked_mut() };
        Poll::Ready(Ok(match &mut me.0 {
            None => 0,
            Some(v) => {
                let ret = ready!(unsafe { Pin::new_unchecked(v) }.poll_read(cx, task, buf))?;
                me.0 = None;
                ret
            }
        }))
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
    Busy(BoxFuture<'static, Result<(std::io::Result<usize>, Buf, T), Error>>),
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

    pub fn read_from<T: std::io::Read>(&mut self, rd: &mut T) -> std::io::Result<usize> {
        let res = uninterruptibly!(rd.read(&mut self.buf));

        if let Ok(n) = res {
            self.buf.truncate(n);
        } else {
            self.buf.clear();
        }

        assert_eq!(self.pos, 0);

        res
    }

    pub fn write_to<T: std::io::Write>(&mut self, wr: &mut T) -> std::io::Result<()> {
        assert_eq!(self.pos, 0);

        // `write_all` already ignores interrupts
        let res = wr.write_all(&self.buf);
        self.buf.clear();
        res
    }
}

impl<T> AsyncRead for Blocking<T>
where
    T: std::io::Read + std::marker::Unpin + Send + Sync + 'static,
{
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        task: &super::TaskManager,
        dst: &mut [u8],
    ) -> Poll<Result<usize, Error>> {
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
                        task.spawn_blocking(move || {
                            let res = buf.read_from(&mut inner);
                            (res, buf, inner)
                        })
                        .boxed(),
                    );
                }
                State::Busy(ref mut rx) => {
                    let (res, mut buf, inner) = match Pin::new(rx).poll(cx) {
                        Poll::Pending => return Poll::Pending,
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

impl<T> AsyncWrite for Blocking<T>
where
    T: std::io::Write + std::marker::Unpin + Send + Sync + 'static,
{
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        task: &super::TaskManager,
        src: &[u8],
    ) -> Poll<Result<usize, Error>> {
        loop {
            match self.state {
                State::Idle(ref mut buf_cell) => {
                    let mut buf = buf_cell.take().unwrap();

                    assert!(buf.is_empty());

                    let n = buf.copy_from(src);
                    let mut inner = self.inner.take().unwrap();

                    self.state = State::Busy(
                        task.spawn_blocking(move || {
                            let n = buf.len();
                            let res = buf.write_to(&mut inner).map(|_| n);

                            (res, buf, inner)
                        })
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

    fn poll_flush(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        task: &super::TaskManager,
    ) -> Poll<Result<(), Error>> {
        loop {
            let need_flush = self.need_flush;
            match self.state {
                // The buffer is not used here
                State::Idle(ref mut buf_cell) => {
                    if need_flush {
                        let buf = buf_cell.take().unwrap();
                        let mut inner = self.inner.take().unwrap();

                        self.state = State::Busy(
                            task.spawn_blocking(move || {
                                let res = inner.flush().map(|_| 0);
                                (res, buf, inner)
                            })
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

    fn poll_shutdown(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        _task: &super::TaskManager,
    ) -> Poll<Result<(), Error>> {
        Poll::Ready(Ok(()))
    }
}
