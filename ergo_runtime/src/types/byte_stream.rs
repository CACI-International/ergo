//! The ByteStream type.

use crate as ergo_runtime;
use crate::{
    context::{ItemContent, TaskManager},
    io::{self, AsyncReadExt, BoxAsyncRead},
    traits,
    type_system::{ergo_traits_fn, ErgoType},
    Error, Value,
};

use crate::abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RBox, RResult, RVec},
    stream::shared_stream::*,
    type_erase::Erased,
    StableAbi,
};
use std::pin::Pin;
use std::sync::Arc;

/// A stream of bytes.
///
/// Call `read()` to get a value supporting `AsyncRead`.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct ByteStream {
    source: ByteStreamSource_TO<'static, RBox<()>>,
    stream_iter: SharedStreamIter<ByteStreamBlock>,
}

impl From<super::String> for ByteStream {
    fn from(s: super::String) -> Self {
        Self::new(io::wrap(std::io::Cursor::new(s.0.into_bytes())))
    }
}

impl ByteStream {
    pub fn new<Src: io::AsyncRead + Send + 'static>(source: Src) -> Self {
        let (stream, stream_iter) = SharedStream::new();
        let source = ByteStreamSource_TO::from_value(
            ByteStreamSourceImpl {
                guard: ByteStreamSourceGuard::None,
                source: Arc::pin(futures::lock::Mutex::new(ByteStreamSourceState {
                    copy: None,
                    async_read: Some(BoxAsyncRead::new(source)),
                    block: Default::default(),
                    _pinned: std::marker::PhantomPinned,
                })),
                stream: Arc::new(stream),
            },
            TU_Opaque,
        );
        ByteStream {
            source,
            stream_iter,
        }
    }

    /// Get a value supporting `AsyncRead`.
    pub fn read(&self) -> ByteStreamReader {
        ByteStreamReader {
            oblock: None,
            source: self.source.clone(),
            stream_iter: self.stream_iter.clone(),
            bytes_read: 0,
        }
    }
}

ergo_traits_fn! {
    impl traits::IntoTyped<super::String> for ByteStream {
        async fn into_typed(self) -> Value {
            let mut bytes = Vec::new();
            if let Err(e) = self.as_ref().read().read_to_end(&CONTEXT.task, &mut bytes).await {
                return e.into();
            }
            super::String::from(match String::from_utf8(bytes) {
                Ok(s) => s,
                Err(v) => String::from_utf8_lossy(v.as_bytes()).into_owned(),
            }).into()
        }
    }

    traits::IntoTyped::<ByteStream>::add_impl::<super::String>(traits);

    impl traits::ValueByContent for ByteStream {
        async fn value_by_content(self, _deep: bool) -> Value {
            let data = self.clone().to_owned();
            let mut reader = data.read();
            let mut buf: [u8; BYTE_STREAM_BLOCK_LIMIT] =
                unsafe { std::mem::MaybeUninit::uninit().assume_init() };
            use crate::hash::HashFn;
            use std::hash::Hasher;
            let mut h = HashFn::default();
            loop {
                let size = match reader.read(&CONTEXT.task, &mut buf).await {
                    Err(e) => return e.into(),
                    Ok(v) => v
                };
                if size == 0 {
                    break
                } else {
                    h.write(&buf[..size]);
                }
            }
            let deps = crate::depends![h.finish_ext()];
            let mut v = Value::from(self);
            v.set_dependencies(deps);
            v
        }
    }

    crate::ergo_type_name!(traits, ByteStream);

    impl traits::Stored for ByteStream {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: ItemContent) -> crate::RResult<()> {
            io::copy(
                &CONTEXT.task,
                &mut self.read(),
                &mut io::Blocking::new(item),
            )
            .await.map(|_| ()).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: ItemContent) -> crate::RResult<Erased> {
            Ok(Erased::new(ByteStream::new(io::Blocking::new(item)))).into()
        }
    }

    impl traits::Display for ByteStream {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::RResult<()> {
            async move {
                let mut reader = self.read();
                let mut buf: [u8; BYTE_STREAM_BLOCK_LIMIT] =
                    unsafe { std::mem::MaybeUninit::uninit().assume_init() };
                let mut overflow: [u8; 4] =
                    unsafe { std::mem::MaybeUninit::uninit().assume_init() };
                let mut overflow_size = 0;
                loop {
                    let end = reader.read(&CONTEXT.task, &mut buf).await?;
                    if end == 0 {
                        break
                    } else {
                        let mut start = 0;
                        while start < end {
                            if overflow_size > 0 {
                                overflow[overflow_size] = buf[start];
                                start += 1;
                                overflow_size += 1;
                                if let Ok(s) = std::str::from_utf8(&overflow[..overflow_size]) {
                                    f.write_str(s)?;
                                    overflow_size = 0;
                                }
                                continue;
                            }

                            match std::str::from_utf8(&buf[start..end]) {
                                Ok(s) => {
                                    f.write_str(s)?;
                                    break;
                                }
                                Err(e) => {
                                    let ind = e.valid_up_to();
                                    f.write_str(std::str::from_utf8(&buf[start..ind]).unwrap())?;
                                    start = ind;
                                    match e.error_len() {
                                        None => {
                                            assert!(end - ind < 4);
                                            overflow_size = end - ind;
                                            overflow[..overflow_size].copy_from_slice(&buf[ind..end]);
                                            break;
                                        }
                                        Some(n) => {
                                            start += n;
                                            f.write_str(std::char::REPLACEMENT_CHARACTER.encode_utf8(&mut overflow))?;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Ok(())
            }.await.into()
        }
    }
}

/// A type supporting `std::io::Read` for a `ByteStream`.
///
/// Cloning the reader will result in another reader that gets an identical byte stream
/// starting from where this reader left off.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct ByteStreamReader {
    // Safety: oblock must be dropped prior to stream_iter
    // SharedStreamIter guarantees that the referenced ByteStreamBlock remains valid across clones.
    oblock: Option<&'static ByteStreamBlock>,
    source: ByteStreamSource_TO<'static, RBox<()>>,
    stream_iter: SharedStreamIter<ByteStreamBlock>,
    bytes_read: usize,
}

const BYTE_STREAM_BLOCK_LIMIT: usize = 2048;

// Easier to use an RVec than try to use a fixed size array.
//
// abi_stable doesn't seem to support boxed slices (without making a wrapper trait), and it
// can't support arbitrary-sized arrays due to rust generic limitations (const size values).
type ByteStreamBlock = RVec<u8>;

struct CopyState {
    task: TaskManager,
    reader: io::Once<io::Take<&'static mut BoxAsyncRead<'static>>>,
    writer: io::TokioWrapped<&'static mut Vec<u8>>,
    copy: Option<
        io::Copier<
            'static,
            io::Once<io::Take<&'static mut BoxAsyncRead<'static>>>,
            io::TokioWrapped<&'static mut Vec<u8>>,
        >,
    >,
    _pinned: std::marker::PhantomPinned,
}

impl CopyState {
    pub fn new(
        task: TaskManager,
        r: &'static mut BoxAsyncRead<'static>,
        w: &'static mut Vec<u8>,
    ) -> Pin<Box<Self>> {
        let mut ret = Box::new(CopyState {
            task,
            reader: r.take(BYTE_STREAM_BLOCK_LIMIT as u64).once(),
            writer: io::wrap(w),
            copy: None,
            _pinned: std::marker::PhantomPinned,
        });

        let cpy = io::copy(&ret.task, &mut ret.reader, &mut ret.writer);
        ret.copy = Some(unsafe {
            std::mem::transmute::<io::Copier<'_, _, _>, io::Copier<'static, _, _>>(cpy)
        });
        unsafe { Pin::new_unchecked(ret) }
    }
}

impl Drop for CopyState {
    fn drop(&mut self) {
        self.copy = None;
    }
}

struct ByteStreamSourceState {
    // copy must be first, dropped before async_read and block
    copy: Option<Pin<Box<CopyState>>>,
    async_read: Option<BoxAsyncRead<'static>>,
    block: Vec<u8>,
    _pinned: std::marker::PhantomPinned,
}

enum ByteStreamSourceGuard<'a> {
    None,
    Fut(futures::lock::MutexLockFuture<'a, ByteStreamSourceState>),
    Active(futures::lock::MutexGuard<'a, ByteStreamSourceState>),
}

impl<'a> Default for ByteStreamSourceGuard<'a> {
    fn default() -> Self {
        ByteStreamSourceGuard::None
    }
}

impl<'a> Clone for ByteStreamSourceGuard<'a> {
    fn clone(&self) -> Self {
        Self::default()
    }
}

// ByteStreamSource needs to be Sync to be Eraseable. The inner MutexGuard is only Sync when T is Sync, but it's not due to the inner BoxAsyncRead.
// However, we know that the guard itself will only be accessed from a single thread at a time.
unsafe impl<'a> Sync for ByteStreamSourceGuard<'a> {}

#[derive(Clone)]
struct ByteStreamSourceImpl {
    guard: ByteStreamSourceGuard<'static>,
    source: Pin<Arc<futures::lock::Mutex<ByteStreamSourceState>>>,
    stream: Arc<SharedStream<ByteStreamBlock>>,
}

#[sabi_trait]
trait ByteStreamSource: Clone + Send + Sync {
    #[sabi(last_prefix_field)]
    fn poll_data<'a>(
        &'a mut self,
        cx: crate::abi_stable::future::Context,
        task: &'a TaskManager,
    ) -> crate::abi_stable::future::Poll<RResult<bool, Error>>;
}

impl ByteStreamSource for ByteStreamSourceImpl {
    fn poll_data<'a>(
        &'a mut self,
        cx: crate::abi_stable::future::Context,
        task: &'a TaskManager,
    ) -> crate::abi_stable::future::Poll<RResult<bool, Error>> {
        use std::task::Poll::*;
        let waker = cx.into_waker();
        let mut cx = std::task::Context::from_waker(&waker);

        loop {
            match &mut self.guard {
                ByteStreamSourceGuard::None => {
                    self.guard = ByteStreamSourceGuard::Fut(unsafe {
                        // Safety: guard will always be dropped prior to mutex.
                        std::mem::transmute::<
                            futures::lock::MutexLockFuture<'_, ByteStreamSourceState>,
                            futures::lock::MutexLockFuture<'static, ByteStreamSourceState>,
                        >(self.source.lock())
                    });
                }
                ByteStreamSourceGuard::Fut(f) => {
                    match std::future::Future::poll(Pin::new(f), &mut cx) {
                        Pending => return crate::abi_stable::future::Poll::Pending,
                        Ready(g) => {
                            self.guard = ByteStreamSourceGuard::Active(g);
                        }
                    }
                }
                ByteStreamSourceGuard::Active(g) => {
                    let ByteStreamSourceState {
                        copy,
                        async_read,
                        block,
                        ..
                    } = &mut **g;
                    if let Some(reader) = async_read {
                        if copy.is_none() {
                            block.reserve_exact(BYTE_STREAM_BLOCK_LIMIT);
                            *copy = Some(CopyState::new(
                                task.clone(),
                                // Safety: copy is dropped prior to async_read.
                                unsafe { std::mem::transmute::<&'_ mut _, &'static mut _>(reader) },
                                // Safety: copy is dropped prior to block.
                                unsafe { std::mem::transmute::<&'_ mut _, &'static mut _>(block) },
                            ));
                        }
                        let copy_pin = unsafe {
                            copy.as_mut()
                                .unwrap()
                                .as_mut()
                                .map_unchecked_mut(|cs| cs.copy.as_mut().unwrap())
                        };
                        let bytes = match std::future::Future::poll(copy_pin, &mut cx) {
                            Pending => return crate::abi_stable::future::Poll::Pending,
                            Ready(r) => {
                                *copy = None;
                                match r {
                                    Ok(v) => v,
                                    Err(e) => {
                                        return crate::abi_stable::future::Poll::Ready(
                                            RResult::RErr(e),
                                        )
                                    }
                                }
                            }
                        };
                        if bytes == 0 {
                            *async_read = None;
                        }
                    }

                    let ret = if g.block.is_empty() {
                        false
                    } else {
                        self.stream.push(std::mem::take(&mut g.block).into());
                        true
                    };

                    self.guard = ByteStreamSourceGuard::None;
                    return crate::abi_stable::future::Poll::Ready(RResult::ROk(ret));
                }
            }
        }
    }
}

impl io::AsyncRead for ByteStreamReader {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<crate::Result<usize>> {
        let mut buf_offset = 0;
        let me = &mut *self;
        while buf_offset < buf.len() {
            if let Some(block) = me.oblock {
                // Read data if there's any remaining.
                if me.bytes_read < block.len() {
                    let to_read = std::cmp::min(block.len() - me.bytes_read, buf.len());
                    buf[buf_offset..(buf_offset + to_read)]
                        .copy_from_slice(&block[me.bytes_read..(me.bytes_read + to_read)]);
                    buf_offset += to_read;
                    me.bytes_read += to_read;
                }
                // Get next item if we've read all the data.
                if me.bytes_read == block.len() {
                    // Safety: oblock is dropped prior to stream_iter.
                    me.oblock = unsafe {
                        std::mem::transmute::<
                            Option<&'_ ByteStreamBlock>,
                            Option<&'static ByteStreamBlock>,
                        >(me.stream_iter.next())
                    };
                    me.bytes_read = 0;
                }

                // If we've read some data, return it immediately.
                if buf_offset > 0 {
                    break;
                }
            } else {
                // Try to read a block from the byte stream source.
                let got_data = match me
                    .source
                    .poll_data(crate::abi_stable::future::Context::new(cx), task)
                {
                    crate::abi_stable::future::Poll::Pending => {
                        debug_assert!(buf_offset == 0);
                        return std::task::Poll::Pending;
                    }
                    crate::abi_stable::future::Poll::Ready(v) => match v.into_result() {
                        Err(e) => return std::task::Poll::Ready(Err(e)),
                        Ok(v) => v,
                    },
                };
                // Re-check stream_iter in case the poll_data blocked (and another consumer loaded data)
                // TODO: improve behavior with tokio::sync::watch or something similar, to not queue up multiple poll_data
                // (though typically all that data will be used).
                if !got_data && me.stream_iter.current().is_none() {
                    break;
                } else {
                    // Safety: oblock is dropped prior to stream_iter.
                    me.oblock = unsafe {
                        std::mem::transmute::<
                            Option<&'_ ByteStreamBlock>,
                            Option<&'static ByteStreamBlock>,
                        >(me.stream_iter.current())
                    };
                    debug_assert!(me.oblock.is_some());
                }
            }
        }

        std::task::Poll::Ready(Ok(buf_offset))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::io::Blocking;

    #[test]
    fn byte_stream() {
        let mut buf: [u8; 10000] = [0; 10000];
        for i in 0..10000 {
            buf[i] = i as u8;
        }
        let stream = ByteStream::new(Blocking::new(std::io::Cursor::new(buf.to_vec())));
        let mut reader1 = stream.read();
        let mut reader2 = reader1.clone();

        let ctx = crate::Context::builder().build().unwrap();

        let mut buf: [u8; 10001] = [0; 10001];
        match ctx.task.block_on(reader1.read_exact(&ctx.task, &mut buf)) {
            Ok(s) => assert!(s == 10000),
            Err(_) => panic!("buffer read failure"),
        }
        for i in 0..10000 {
            assert!(buf[i] == i as u8);
        }

        match ctx
            .task
            .block_on(reader2.read_exact(&ctx.task, &mut buf[..500]))
        {
            Ok(s) => assert!(s == 500),
            Err(_) => panic!("buffer read failure"),
        }
        for i in 0..500 {
            assert!(buf[i] == i as u8);
        }

        drop(reader1);
        let mut reader3 = reader2.clone();
        drop(reader2);
        let mut reader4 = reader3.clone();

        match ctx
            .task
            .block_on(reader3.read_exact(&ctx.task, &mut buf[..500]))
        {
            Ok(s) => assert!(s == 500),
            Err(_) => panic!("buffer read failure"),
        }
        for i in 0..500 {
            assert!(buf[i] == (i + 500) as u8);
        }

        match ctx
            .task
            .block_on(reader4.read_exact(&ctx.task, &mut buf[..500]))
        {
            Ok(s) => assert!(s == 500),
            Err(_) => panic!("buffer read failure"),
        }
        for i in 0..500 {
            assert!(buf[i] == (i + 500) as u8);
        }
    }
}
