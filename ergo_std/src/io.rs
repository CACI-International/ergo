/// I/O utilities.
use ergo_runtime::{depends, io, nsid, traits, try_result, types, Value};
use futures::lock::{Mutex, MutexGuard};

pub fn module() -> Value {
    crate::make_string_map! {
        "stdin" = stdin(),
        "stdout" = stdout(),
        "stderr" = stderr()
    }
}

struct Stdin<'a> {
    inner: io::Blocking<std::io::Stdin>,
    _guard: MutexGuard<'a, ()>,
}

impl<'a> Stdin<'a> {
    pub fn new(guard: MutexGuard<'a, ()>) -> Self {
        Stdin {
            inner: io::Blocking::new(std::io::stdin()),
            _guard: guard,
        }
    }
}

impl<'a> io::AsyncRead for Stdin<'a> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &ergo_runtime::context::TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<ergo_runtime::Result<usize>> {
        io::AsyncRead::poll_read(
            unsafe { self.map_unchecked_mut(|v| &mut v.inner) },
            cx,
            task,
            buf,
        )
    }
}

lazy_static::lazy_static! {
    static ref STDIN_MUTEX: Mutex<()> = Mutex::new(());
    static ref STDOUT_MUTEX: Mutex<()> = Mutex::new(());
}

static STDIN_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[types::ergo_fn]
/// Get the standard input ByteStream of the process.
///
/// Arguments: (none)
///
/// Returns a ByteStream that takes exclusive access of the process standard input.
async fn stdin() -> Value {
    let id = STDIN_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let guard = STDIN_MUTEX.lock().await;
    Value::constant_deps(
        types::ByteStream::new(Stdin::new(guard)),
        depends![nsid!(io::stdin), id],
    )
}

#[types::ergo_fn]
/// Write a ByteStream to the process standard output.
///
/// Arguments: `(Into<ByteStream> :bytes)`
///
/// Takes exclusive access of the process standard output (pausing logging) and writes the ByteStream to it.
async fn stdout(bytes: _) -> Value {
    let bytes =
        try_result!(traits::into_sourced::<types::ByteStream>(CONTEXT, bytes).await).unwrap();
    {
        let _guard = STDOUT_MUTEX.lock().await;
        let _paused = CONTEXT.log.pause();
        try_result!(
            io::copy_interactive(
                &CONTEXT.task,
                &mut bytes.as_ref().read(),
                &mut io::Blocking::new(std::io::stdout())
            )
            .await
        );
    }
    types::Unit.into()
}

#[types::ergo_fn]
/// Write a ByteStream to the process standard error.
///
/// Arguments: `(ByteStream :bytes)`
///
/// Writes the ByteStream to the process standard error. This does not have any exclusive access
/// guarantees like `stdin` and `stdout`.
async fn stderr(bytes: _) -> Value {
    let bytes =
        try_result!(traits::into_sourced::<types::ByteStream>(CONTEXT, bytes).await).unwrap();
    try_result!(
        io::copy_interactive(
            &CONTEXT.task,
            &mut bytes.as_ref().read(),
            &mut io::Blocking::new(std::io::stderr())
        )
        .await
    );
    types::Unit.into()
}
