/// I/O utilities.
use ergo_runtime::{depends, io, nsid, traits, types, Context, Value};
use futures::lock::{Mutex, MutexGuard};

pub fn module() -> Value {
    crate::make_string_map! {
        "stdin" = stdin(),
        "stdout" = stdout(),
        "stderr" = stderr(),
        "is-terminal" = is_terminal()
    }
}

// FIXME use `std::io::StdinLock` instead (it isn't Send so that complicates things)?
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
        buf: &mut [u8],
    ) -> std::task::Poll<io::Result<usize>> {
        io::AsyncRead::poll_read(unsafe { self.map_unchecked_mut(|v| &mut v.inner) }, cx, buf)
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
/// Arguments: `()`
///
/// Returns a ByteStream that takes exclusive access of the process standard input.
async fn stdin(_: types::Unit) -> Value {
    let id = STDIN_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let guard = STDIN_MUTEX.lock().await;
    Value::with_id(
        types::ByteStream::new(Stdin::new(guard)),
        depends![const nsid!(io::stdin), id],
    )
}

#[types::ergo_fn]
/// Write a ByteStream to the process standard output.
///
/// Arguments: `(Into<ByteStream> :bytes)`
///
/// Takes exclusive access of the process standard output (pausing logging) and writes the ByteStream to it.
async fn stdout(bytes: _) -> Value {
    let bytes = traits::into::<types::ByteStream>(bytes).await?;
    {
        let _guard = STDOUT_MUTEX.lock().await;
        let _paused = Context::global().log.pause();
        io::copy_interactive(
            &mut bytes.as_ref().read(),
            &mut io::Blocking::new(std::io::stdout()),
        )
        .await?;
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
    let bytes = traits::into::<types::ByteStream>(bytes).await?;
    io::copy_interactive(
        &mut bytes.as_ref().read(),
        &mut io::Blocking::new(std::io::stderr()),
    )
    .await?;
    types::Unit.into()
}

#[types::ergo_fn]
/// Return whether an io stream is connected to a terminal.
///
/// Arguments: `()`
///
/// Keyed arguments:
/// * `:stream` (optional) - the stream to check. May be `stdout` (default), `stdin`, or `stderr`.
///
/// Returns a Bool indicating whether the stream is connected to a terminal.
async fn is_terminal(_: types::Unit, (stream): [types::String]) -> Value {
    let stream = match stream {
        None => atty::Stream::Stdout,
        Some(s) => match s.as_ref().as_str() {
            "stdout" => atty::Stream::Stdout,
            "stderr" => atty::Stream::Stderr,
            "stdin" => atty::Stream::Stdin,
            other => Err(ergo_runtime::metadata::Source::get(&s)
                .with(format!("invalid stream: {}", other))
                .into_error())?,
        },
    };

    types::Bool(atty::is(stream)).into()
}
