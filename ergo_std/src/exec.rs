//! Execute (possibly with path-lookup) external programs.

use abi_stable::{StableAbi, std_types::ROption};
use futures::channel::oneshot::channel;
use grease::{
    depends,
    ffi::OsString,
    bst::BstMap,
    make_value, match_value,
    path::PathBuf,
    runtime::{ItemContent,Traits},
    types::GreaseType,
    value::{Dependencies, Value},
};
use ergo_runtime::{ergo_function, traits, traits::IntoTyped, types};
use std::process::{Command, Stdio};

/// Strings used for commands and arguments.
#[derive(Clone, Debug, GreaseType, Hash, StableAbi)]
#[repr(C)]
pub struct CommandString(OsString);

impl From<types::String> for CommandString {
    fn from(s: types::String) -> Self {
        CommandString(std::ffi::OsString::from(s.into_string()).into())
    }
}

impl From<PathBuf> for CommandString {
    fn from(p: PathBuf) -> Self {
        CommandString(p.into())
    }
}

/// The exit status of a command.
#[derive(Clone, Debug, Eq, PartialEq, Hash, GreaseType, StableAbi)]
#[repr(C)]
pub struct ExitStatus(pub ROption<i32>);

impl ExitStatus {
    pub fn success(&self) -> bool {
        self.0.map(|s| s == 0).unwrap_or(false)
    }
}

impl std::fmt::Display for ExitStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "exit status: ")?;
        match self.0 {
            ROption::RNone => write!(f, "signal"),
            ROption::RSome(i) => write!(f, "{}", i)
        }
    }
}

impl traits::GreaseStored for ExitStatus {
    fn put(&self, _ctx: &traits::StoredContext, mut into: ItemContent) -> ergo_runtime::Result<()> {
        bincode::serialize_into(&mut into, &self.0.clone().into_option()).map_err(|e| e.into())
    }

    fn get(_ctx: &traits::StoredContext, mut from: ItemContent) -> ergo_runtime::Result<Self> {
        let p: Option<i32> = bincode::deserialize_from(&mut from)?;
        Ok(ExitStatus(p.into()))
    }
}

ergo_runtime::grease_display_basic!(ExitStatus);
ergo_runtime::grease_type_name!(ExitStatus);

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(s: std::process::ExitStatus) -> Self {
        ExitStatus(s.code().into())
    }
}

impl From<ExitStatus> for bool {
    fn from(e: ExitStatus) -> Self {
        e.success()
    }
}

mod bytestream {
    use abi_stable::{
        erased_types::DynTrait,
        external_types::{RMutex, RRwLock},
        std_types::{RArc, RBox, ROption, RVec},
        StableAbi,
    };
    use grease::types::GreaseType;
    use std::ptr::NonNull;
    use std::sync::atomic::{AtomicU64, Ordering};
    use super::{traits, ItemContent};

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

    #[derive(StableAbi)]
    #[sabi(impl_InterfaceType(IoRead,Send))]
    #[repr(C)]
    struct ByteStreamInterface;

    const BYTE_STREAM_BLOCK_LIMIT: usize = 2048;

    // Easier to use an RVec than try to use a fixed size array.
    //
    // abi_stable doesn't seem to support boxed slices (without making a wrapper trait), and it
    // can't support arbitrary-sized arrays due to rust generic limitations (const size values).
    type ByteStreamBlock = RVec<u8>;

    #[derive(StableAbi)]
    #[repr(C)]
    struct ByteStreamSource {
        source: RMutex<DynTrait<'static, RBox<()>, ByteStreamInterface>>,
        stream: SharedStream<ByteStreamBlock>,
    }

    impl ByteStreamSource {
        pub fn need_data(&self) -> std::io::Result<bool> {
            let mut block = ByteStreamBlock::with_capacity(BYTE_STREAM_BLOCK_LIMIT);
            {
                use std::io::Read;
                let mut guard = self.source.lock();
                std::io::copy(&mut guard.by_ref().take(BYTE_STREAM_BLOCK_LIMIT as u64), &mut block)?;
            }

            if block.is_empty() {
                Ok(false)
            } else {
                self.stream.push(block);
                Ok(true)
            }
        }
    }

    /// A stream of bytes.
    ///
    /// Call `read()` to get a value supporting `std::io::Read`.
    #[derive(Clone, GreaseType, StableAbi)]
    #[repr(C)]
    pub struct ByteStream {
        source: RArc<ByteStreamSource>,
        stream_iter: SharedStreamIter<ByteStreamBlock>,
    }

    impl std::hash::Hash for ByteStream {
        fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
            use std::io::Read;
            let mut reader = self.read();
            let mut buf: [u8; BYTE_STREAM_BLOCK_LIMIT] = unsafe { std::mem::MaybeUninit::uninit().assume_init() };
            loop {
                match reader.read(&mut buf) {
                    Err(_) | Ok(0) => return,
                    Ok(size) => h.write(&buf[..size])
                }
            }
        }
    }

    impl From<ergo_runtime::types::String> for ByteStream {
        fn from(s: ergo_runtime::types::String) -> Self {
            Self::new(std::io::Cursor::new(s.into_bytes()))
        }
    }

    ergo_runtime::grease_type_name!(ByteStream);

    impl From<ByteStream> for ergo_runtime::types::String {
        fn from(bs: ByteStream) -> Self {
            use std::io::Read;
            let mut bytes = Vec::new();
            if let Err(_e) = bs.read().read_to_end(&mut bytes) {
                // FIXME handle error
            }
            match String::from_utf8(bytes) {
                Ok(s) => s.into(),
                Err(v) => String::from_utf8_lossy(v.as_bytes()).into_owned().into()
            }
        }
    }
    
    impl traits::GreaseStored for ByteStream {
        fn put(&self, _ctx: &traits::StoredContext, mut into: ItemContent) -> ergo_runtime::Result<()> {
            std::io::copy(&mut self.read(), &mut into)?;
            Ok(())
        }

        fn get(_ctx: &traits::StoredContext, from: ItemContent) -> ergo_runtime::Result<Self> {
            Ok(ByteStream::new(from))
        }
    }

    impl ByteStream {
        pub fn new<Src: std::io::Read + Send + 'static>(source: Src) -> Self {
            let (stream, stream_iter) = SharedStream::new();
            let source = RArc::new(ByteStreamSource {
                source: RMutex::new(DynTrait::from_any_value(source, ByteStreamInterface)),
                stream,
            });
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
        source: RArc<ByteStreamSource>,
        stream_iter: SharedStreamIter<ByteStreamBlock>,
        bytes_read: usize,
    }

    impl std::io::Read for ByteStreamReader {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
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
                } else {
                    // Try to read a block from the byte stream source.
                    if !self.source.need_data()? {
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

    #[cfg(test)]
    mod test {
        use super::*;

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
            use std::io::Read;

            let mut buf: [u8; 10000] = [0; 10000];
            for i in 0..10000 {
                buf[i] = i as u8;
            }
            let stream = ByteStream::new(std::io::Cursor::new(buf.to_vec()));
            let mut reader1 = stream.read();
            let mut reader2 = reader1.clone();

            let mut buf: [u8; 10001] = [0; 10001];
            match reader1.read(&mut buf) {
                Ok(s) => assert!(s == 10000),
                Err(_) => panic!("buffer read failure"),
            }
            for i in 0..10000 {
                assert!(buf[i] == i as u8);
            }

            match reader2.read(&mut buf[..500]) {
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

            match reader3.read(&mut buf[..500]) {
                Ok(s) => assert!(s == 500),
                Err(_) => panic!("buffer read failure"),
            }
            for i in 0..500 {
                assert!(buf[i] == (i + 500) as u8);
            }

            match reader4.read(&mut buf[..500]) {
                Ok(s) => assert!(s == 500),
                Err(_) => panic!("buffer read failure"),
            }
            for i in 0..500 {
                assert!(buf[i] == (i + 500) as u8);
            }
        }
    }
}

pub use bytestream::ByteStream;

pub fn function() -> Value {
    ergo_function!(std::exec, |ctx| {
        let cmd = ctx.args.next().ok_or("no command provided")?;

        let (cmdsource, cmd) = cmd.take();

        let cmd = ctx
            .traits
            .get::<IntoTyped<CommandString>>(&cmd)
            .ok_or(
                cmdsource
                    .with("cannot convert value into command string")
                    .into_grease_error(),
            )?
            .into_typed(cmd);

        let mut args = Vec::default();
        while let Some(arg) = ctx.args.next() {
            args.push(
                arg.map(|v| {
                    ctx.traits
                        .get::<IntoTyped<CommandString>>(&v)
                        .ok_or(format!("cannot convert {} into command string", traits::type_name(&ctx.traits, &*v.grease_type())))
                        .map(|t| t.into_typed(v))
                })
                .transpose_err()
                .map_err(|e| e.into_grease_error())?,
            );
        }

        let env = ctx
            .args
            .kw("env")
            .map(|v| {
                v.map(|v| v.typed::<types::Map>().map_err(|_| "env must be a map"))
                    .transpose_err()
                    .map_err(|e| e.into_grease_error())
            })
            .transpose()?;

        let dir = ctx
            .args
            .kw("pwd")
            .map(|v| {
                v.map(|v| {
                    ctx.traits
                        .get::<IntoTyped<PathBuf>>(&v)
                        .ok_or(format!("cannot convert pwd (typed {}) into path", traits::type_name(&ctx.traits, &*v.grease_type())))
                        .map(|t| t.into_typed(v))
                })
                .transpose_err()
                .map_err(|e| e.into_grease_error())
            })
            .transpose()?;
        // TODO consistently set dir?

        let stdin = ctx
            .args
            .kw("stdin")
            .map(|v| {
                v.map(|v| {
                    ctx.traits
                        .get::<IntoTyped<ByteStream>>(&v)
                        .ok_or("cannot convert stdin into byte stream")
                        .map(|t| t.into_typed(v))
                })
                .transpose_err()
                .map_err(|e| e.into_grease_error())
            })
            .transpose()?;

        ctx.unused_arguments()?;

        let mut deps = depends![cmd, ^@args];
        let mut unordered_deps = Vec::new();
        if let Some(v) = &env {
            unordered_deps.push(v.into());
        }
        if let Some(v) = &dir {
            unordered_deps.push(v.into());
        }
        if let Some(v) = &stdin {
            unordered_deps.push(v.into());
        }
        deps += Dependencies::unordered(unordered_deps);

        let task = ctx.task.clone();
        let traits = ctx.traits.clone();
        let log = ctx.log.sublog("exec");

        // Create channels for outputs
        let (send_stdout, rcv_stdout) = channel();
        let (send_stderr, rcv_stderr) = channel();
        let (send_status, rcv_status) = channel();
        
        let run_command = make_value!([^deps] {
            let mut vals: Vec<Value> = Vec::new();
            vals.push(cmd.clone().into());
            for v in &args {
                vals.push(v.clone().into());
            }
            if let Some(v) = &env {
                vals.push(v.clone().into());
            }
            if let Some(v) = &dir {
                vals.push(v.clone().into());
            }
            if let Some(v) = &stdin {
                vals.push(v.clone().into());
            }
            task.join_all(vals).await?;

            let mut command = Command::new(cmd.forced_value().0.as_ref());
            let arg_vals: Vec<_> = args.iter().map(|a| a.forced_value()).collect();
            command.args(arg_vals.iter().map(|v| v.0.as_ref()));
            command.env_clear();
            if let Some(v) = env {
                let types::Map(env) = v.forced_value().owned();
                for (k,v) in env {
                    let k = k.into_string();
                    match_value!(v => {
                        () => |_| {
                            if let Some(v) = std::env::var_os(&k) {
                                command.env(k, v);
                            }
                        },
                        => |v| {
                            let t =
                                traits.get::<IntoTyped<CommandString>>(&v).ok_or(format!("cannot convert env value with key {} into command string", k))?;
                            command.env(k, t.into_typed(v).await?.0.as_ref());
                        }
                    });
                }
            }
            if let Some(v) = dir {
                command.current_dir(v.forced_value().as_ref().as_ref());
            }

            if stdin.is_some() {
                command.stdin(Stdio::piped());
            }
            command.stdout(Stdio::piped());
            command.stderr(Stdio::piped());

            log.debug(format!("spawning child process: {:?}", command));

            let mut child = command.spawn()?;
            if let (Some(input),Some(v)) = (&mut child.stdin, &stdin) {
                std::io::copy(&mut v.forced_value().read(), input)?;
            }

            //TODO make stdin/stdout/stderr concurrently streamed instead of done all at once
            let output = child.wait_with_output()?;

            macro_rules! fetch_named {
                ( $channel:ident, $output:expr, $name:expr ) => {
                    if $channel.is_canceled() {
                        if let Ok(s) = String::from_utf8($output) {
                            if s.is_empty() {
                                "".into()
                            } else {
                                format!("\n{}:\n{}", $name, s)
                            }
                        } else {
                            "".into()
                        }
                    } else {
                        "".into()
                    }
                }
            }
            
            if send_status.is_canceled() {
                if !output.status.success() {
                    let stdout = fetch_named!(send_stdout, output.stdout, "stdout");
                    let stderr = fetch_named!(send_stderr, output.stderr, "stderr");
                    return Err(format!("command returned failure exit status{}{}", stdout, stderr).into());
                }
            } else {
                drop(send_status.send(output.status));
            }
            drop(send_stdout.send(output.stdout));
            drop(send_stderr.send(output.stderr));
            Ok(())
        });

        let mut ret_map = BstMap::default();
        let stdout = make_value!((run_command) ["stdout"] { run_command.await?; Ok(ByteStream::new(std::io::Cursor::new(rcv_stdout.await?))) });
        let stderr = make_value!((run_command) ["stderr"] { run_command.await?; Ok(ByteStream::new(std::io::Cursor::new(rcv_stderr.await?))) });
        let exit_status = make_value!((run_command) ["exit_status"] { run_command.await?; Ok(ExitStatus::from(rcv_status.await?)) });
        
        ret_map.insert("stdout".into(), ctx.imbue_error_context(stdout.into(), "while evaluating stdout of exec command"));
        ret_map.insert("stderr".into(), ctx.imbue_error_context(stderr.into(), "while evaluating stderr of exec command"));
        ret_map.insert("exit-status".into(), ctx.imbue_error_context(exit_status.into(), "while evaluating exit_status of exec command"));
        ret_map.insert("complete".into(), ctx.imbue_error_context(run_command.into(), "while evaluating result of exec command"));

        types::Map(ret_map).into()
    })
    .into()
}

/// Traits for CommandString, ExitStatus, and ByteStream types.
pub fn traits(traits: &mut Traits) {
    // CommandString traits
    IntoTyped::<CommandString>::add_impl::<types::String>(traits);
    IntoTyped::<CommandString>::add_impl::<PathBuf>(traits);

    // ExitStatus traits
    IntoTyped::<bool>::add_impl::<ExitStatus>(traits);
    traits::ValueByContent::add_impl::<ExitStatus>(traits);
    traits::Display::add_impl::<ExitStatus>(traits);
    traits::TypeName::add_impl::<ExitStatus>(traits);
    traits::Stored::add_impl::<ExitStatus>(traits);

    // ByteStream traits
    IntoTyped::<types::String>::add_impl::<ByteStream>(traits);
    IntoTyped::<ByteStream>::add_impl::<types::String>(traits);
    traits::ValueByContent::add_impl::<ByteStream>(traits);
    traits::TypeName::add_impl::<ByteStream>(traits);
    traits::Stored::add_impl::<ByteStream>(traits);
}
