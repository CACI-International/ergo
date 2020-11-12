//! Byte stream type, supporting on-demand streaming.

use crate::traits as trts;
use abi_stable::{
    external_types::RRwLock,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RResult, RString, RVec},
    StableAbi,
};
use grease::{
    future::BoxAsyncRead,
    grease_traits_fn, make_value,
    runtime::{io::AsyncReadExt, ItemContent},
    types::GreaseType,
    Erased, Error, Value,
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
struct SharedStreamIter<T> {
    stream: SharedStream<T>,
    current: NonNull<SharedStreamItem<T>>,
    has_read_current: bool,
}

unsafe impl<T> Send for SharedStreamIter<T> {}
unsafe impl<T> Sync for SharedStreamIter<T> {}

impl<T> SharedStreamIter<T> {
    pub fn new(stream: SharedStream<T>, current: NonNull<SharedStreamItem<T>>) -> Self {
        unsafe { current.as_ref() }
            .count
            .fetch_add(1, Ordering::Relaxed);
        SharedStreamIter {
            stream,
            current,
            has_read_current: false,
        }
    }

    /// Get the current value, if any.
    pub fn current(&mut self) -> Option<&T> {
        if let ROption::RSome(v) = &unsafe { self.current.as_ref() }.item {
            self.has_read_current = true;
            Some(v)
        } else {
            None
        }
    }

    /// Get the next value, if any.
    ///
    /// We cannot implement std::iter::Iterator because this is a streaming iterator, which
    /// must retain the reference lifetime correctly.
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
struct SharedStream<T> {
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

const BYTE_STREAM_BLOCK_LIMIT: usize = 2048;

// Easier to use an RVec than try to use a fixed size array.
//
// abi_stable doesn't seem to support boxed slices (without making a wrapper trait), and it
// can't support arbitrary-sized arrays due to rust generic limitations (const size values).
type ByteStreamBlock = RVec<u8>;

struct ByteStreamSourceImpl {
    source: futures::lock::Mutex<Option<BoxAsyncRead<'static>>>,
    stream: SharedStream<ByteStreamBlock>,
}

#[sabi_trait]
trait ByteStreamSource: Send + Sync {
    #[sabi(last_prefix_field)]
    fn need_data<'a>(
        &'a self,
        task: &'a grease::runtime::TaskManager,
    ) -> grease::future::BoxFuture<'a, RResult<bool, Error>>;
}

impl ByteStreamSourceImpl {
    pub async fn need_data_impl(&self, task: &grease::runtime::TaskManager) -> Result<bool, Error> {
        let mut block = Vec::new();
        {
            let mut guard = self.source.lock().await;
            if let Some(reader) = &mut *guard {
                block.reserve_exact(BYTE_STREAM_BLOCK_LIMIT);
                let bytes = grease::runtime::io::copy(
                    task,
                    &mut reader.take(BYTE_STREAM_BLOCK_LIMIT as u64).once(),
                    &mut grease::runtime::io::wrap(&mut block),
                )
                .await?;
                if bytes == 0 {
                    *guard = None;
                }
            }
        }

        if block.is_empty() {
            Ok(false)
        } else {
            self.stream.push(block.into());
            Ok(true)
        }
    }
}

impl ByteStreamSource for ByteStreamSourceImpl {
    fn need_data<'a>(
        &'a self,
        task: &'a grease::runtime::TaskManager,
    ) -> grease::future::BoxFuture<'a, RResult<bool, Error>> {
        grease::future::BoxFuture::new(async move { self.need_data_impl(task).await.into() })
    }
}

/// A stream of bytes.
///
/// Call `read()` to get a value supporting `std::io::Read`.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct ByteStream {
    source: RArc<ByteStreamSource_TO<'static, RBox<()>>>,
    stream_iter: SharedStreamIter<ByteStreamBlock>,
}

impl From<crate::types::String> for ByteStream {
    fn from(s: crate::types::String) -> Self {
        Self::new(grease::runtime::io::wrap(std::io::Cursor::new(
            s.into_bytes(),
        )))
    }
}

impl ByteStream {
    pub fn new<Src: grease::runtime::io::AsyncRead + Send + 'static>(source: Src) -> Self {
        let (stream, stream_iter) = SharedStream::new();
        let source = RArc::new(ByteStreamSource_TO::from_value(
            ByteStreamSourceImpl {
                source: futures::lock::Mutex::new(Some(BoxAsyncRead::new(source))),
                stream,
            },
            TU_Opaque,
        ));
        ByteStream {
            source,
            stream_iter,
        }
    }

    /// Get a value supporting `std::io::Read`.
    pub fn read(&self) -> ByteStreamReader {
        ByteStreamReader {
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
    source: RArc<ByteStreamSource_TO<'static, RBox<()>>>,
    stream_iter: SharedStreamIter<ByteStreamBlock>,
    bytes_read: usize,
}

impl ByteStreamReader {
    async fn read(
        &mut self,
        task: &grease::runtime::TaskManager,
        buf: &mut [u8],
    ) -> grease::Result<usize> {
        let mut buf_offset = 0;
        let mut oblock = self.stream_iter.current();

        while buf_offset < buf.len() {
            if let Some(block) = oblock {
                // Read data if there's any remaining.
                if self.bytes_read < block.len() {
                    let to_read = std::cmp::min(block.len() - self.bytes_read, buf.len());
                    buf[buf_offset..(buf_offset + to_read)]
                        .copy_from_slice(&block[self.bytes_read..(self.bytes_read + to_read)]);
                    buf_offset += to_read;
                    self.bytes_read += to_read;
                }
                // Get next item if we've read all the data.
                if self.bytes_read == block.len() {
                    oblock = self.stream_iter.next();
                    self.bytes_read = 0;
                }

                // If we've read some data, return it immediately.
                if buf_offset > 0 {
                    break;
                }
            } else {
                // Try to read a block from the byte stream source.
                if !self.source.need_data(task).await.into_result()? {
                    break;
                } else {
                    oblock = self.stream_iter.current();
                    debug_assert!(oblock.is_some());
                }
            }
        }

        return Ok(buf_offset);
    }
}

impl grease::runtime::io::AsyncRead for ByteStreamReader {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &grease::runtime::TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<Result<usize, Error>> {
        let mut fut = self.read(task, buf);
        std::future::Future::poll(unsafe { std::pin::Pin::new_unchecked(&mut fut) }, cx)
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
}

#[cfg(test)]
mod test {
    use super::*;
    use grease::runtime::io::Blocking;

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
