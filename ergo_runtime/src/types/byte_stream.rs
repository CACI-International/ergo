//! Byte stream type, supporting on-demand streaming.

use super::shared_stream::*;
use crate::traits as trts;
use abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RBox, RResult, RString, RVec},
    StableAbi,
};
use grease::{
    future::BoxAsyncRead,
    grease_traits_fn, make_value,
    runtime::{io::AsyncReadExt, ItemContent},
    types::GreaseType,
    Erased, Error, Value,
};
use std::pin::Pin;
use std::sync::Arc;

const BYTE_STREAM_BLOCK_LIMIT: usize = 2048;

// Easier to use an RVec than try to use a fixed size array.
//
// abi_stable doesn't seem to support boxed slices (without making a wrapper trait), and it
// can't support arbitrary-sized arrays due to rust generic limitations (const size values).
type ByteStreamBlock = RVec<u8>;

struct CopyState {
    task: grease::runtime::TaskManager,
    reader:
        grease::runtime::io::Once<grease::runtime::io::Take<&'static mut BoxAsyncRead<'static>>>,
    writer: grease::runtime::io::TokioWrapped<&'static mut Vec<u8>>,
    copy: Option<
        grease::runtime::io::Copy<
            'static,
            grease::runtime::io::Once<
                grease::runtime::io::Take<&'static mut BoxAsyncRead<'static>>,
            >,
            grease::runtime::io::TokioWrapped<&'static mut Vec<u8>>,
        >,
    >,
    _pinned: std::marker::PhantomPinned,
}

impl CopyState {
    pub fn new(
        task: grease::runtime::TaskManager,
        r: &'static mut BoxAsyncRead<'static>,
        w: &'static mut Vec<u8>,
    ) -> Pin<Box<Self>> {
        let mut ret = Box::new(CopyState {
            task,
            reader: r.take(BYTE_STREAM_BLOCK_LIMIT as u64).once(),
            writer: grease::runtime::io::wrap(w),
            copy: None,
            _pinned: std::marker::PhantomPinned,
        });

        let cpy = grease::runtime::io::copy(&ret.task, &mut ret.reader, &mut ret.writer);
        ret.copy = Some(unsafe {
            std::mem::transmute::<
                grease::runtime::io::Copy<'_, _, _>,
                grease::runtime::io::Copy<'static, _, _>,
            >(cpy)
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
        cx: grease::future::Context,
        task: &'a grease::runtime::TaskManager,
    ) -> grease::future::Poll<RResult<bool, Error>>;
}

impl ByteStreamSource for ByteStreamSourceImpl {
    fn poll_data<'a>(
        &'a mut self,
        cx: grease::future::Context,
        task: &'a grease::runtime::TaskManager,
    ) -> grease::future::Poll<RResult<bool, Error>> {
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
                        Pending => return grease::future::Poll::Pending,
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
                            Pending => return grease::future::Poll::Pending,
                            Ready(r) => {
                                *copy = None;
                                match r {
                                    Ok(v) => v,
                                    Err(e) => return grease::future::Poll::Ready(RResult::RErr(e)),
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
                    return grease::future::Poll::Ready(RResult::ROk(ret));
                }
            }
        }
    }
}

/// A stream of bytes.
///
/// Call `read()` to get a value supporting `AsyncRead`.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct ByteStream {
    source: ByteStreamSource_TO<'static, RBox<()>>,
    stream_iter: SharedStreamIter<ByteStreamBlock>,
}

impl From<crate::types::String> for ByteStream {
    fn from(s: crate::types::String) -> Self {
        Self::new(grease::runtime::io::wrap(std::io::Cursor::new(
            s.0.into_bytes(),
        )))
    }
}

impl ByteStream {
    pub fn new<Src: grease::runtime::io::AsyncRead + Send + 'static>(source: Src) -> Self {
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

impl grease::runtime::io::AsyncRead for ByteStreamReader {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &grease::runtime::TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<grease::Result<usize>> {
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
                let got_data = match me.source.poll_data(grease::future::Context::new(cx), task) {
                    grease::future::Poll::Pending => {
                        debug_assert!(buf_offset == 0);
                        return std::task::Poll::Pending;
                    }
                    grease::future::Poll::Ready(v) => match v.into_result() {
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

grease_traits_fn! {
    impl trts::IntoTyped<crate::types::String> for ByteStream {
        async fn into_typed(self) -> Value {
            let v = self;
            let task = CONTEXT.task.clone();
            make_value!([v] {
                let mut bytes = Vec::new();
                v.await?.read().read_to_end(&task, &mut bytes).await?;

                Ok(crate::types::String::from(match String::from_utf8(bytes) {
                    Ok(s) => s,
                    Err(v) => String::from_utf8_lossy(v.as_bytes()).into_owned(),
                }))
            }).into()
        }
    }

    trts::IntoTyped::<ByteStream>::add_impl::<crate::types::String>(traits);

    impl trts::ValueByContent for ByteStream {
        async fn value_by_content(self, _deep: bool) -> Value {
            let data = self.clone().await?;
            let mut reader = data.read();
            let mut buf: [u8; BYTE_STREAM_BLOCK_LIMIT] =
                unsafe { std::mem::MaybeUninit::uninit().assume_init() };
            use grease::hash::HashFn;
            use std::hash::Hasher;
            let mut h = HashFn::default();
            loop {
                let size = reader.read(&CONTEXT.task, &mut buf).await?;
                if size == 0 {
                    break
                } else {
                    h.write(&buf[..size]);
                }
            }
            let deps = grease::depends![h.finish_ext()];
            Value::from(self).set_dependencies(deps)
        }
    }

    impl trts::TypeName for ByteStream {
        async fn type_name() -> RString {
            "ByteStream".into()
        }
    }

    impl trts::Stored for ByteStream {
        async fn put(&self, _stored_ctx: &trts::StoredContext, item: ItemContent) {
            grease::runtime::io::copy(
                &CONTEXT.task,
                &mut self.read(),
                &mut grease::runtime::io::Blocking::new(item),
            )
            .await?;
        }

        async fn get(_stored_ctx: &trts::StoredContext, item: ItemContent) -> Erased {
            Erased::new(ByteStream::new(grease::runtime::io::Blocking::new(item)))
        }
    }

    impl trts::Display for ByteStream {
        async fn fmt(&self) -> RString {
            let mut bytes = Vec::new();
            self.read().read_to_end(&CONTEXT.task, &mut bytes).await?;

            match String::from_utf8(bytes) {
                Ok(s) => s.into(),
                Err(v) => String::from_utf8_lossy(v.as_bytes()).into()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use grease::runtime::io::Blocking;

    #[test]
    fn byte_stream() {
        let mut buf: [u8; 10000] = [0; 10000];
        for i in 0..10000 {
            buf[i] = i as u8;
        }
        let stream = ByteStream::new(Blocking::new(std::io::Cursor::new(buf.to_vec())));
        let mut reader1 = stream.read();
        let mut reader2 = reader1.clone();

        let ctx = grease::runtime::Context::builder().build().unwrap();

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
