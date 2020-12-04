/// I/O utilities.
use ergo_runtime::{ergo_function, types, ContextExt};
use futures::lock::{Mutex, MutexGuard};
use grease::runtime::io;
use grease::{make_value, value::Value};

pub fn module() -> Value {
    crate::grease_string_map! {
        r"A map of program IO functions.

Each of these functions grants exlusive access to the byte stream."
        "stdin": "A byte stream of the program's stdin handle." = stdin_fn(),
        "stdout": "Write a byte stream to the program's stdout handle." = stdout_fn(),
        "stderr": "Write a byte stream to the program's stderr handle." = stderr_fn()
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
        task: &grease::runtime::TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<grease::Result<usize>> {
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

fn stdin_fn() -> Value {
    ergo_function!(std::io::stdin, |ctx| {
        ctx.unused_arguments()?;

        // Derive identity from random integer; stdin may contain anything.
        make_value!([rand::random::<u64>()] {
            let guard = STDIN_MUTEX.lock().await;
            Ok(types::ByteStream::new(Stdin::new(guard)))
        })
        .into()
    })
    .into()
}

fn stdout_fn() -> Value {
    ergo_function!(std::io::stdout, |ctx| {
        let data = ctx.args.next().ok_or("'data' missing")?;

        ctx.unused_arguments()?;

        let data = ctx.into_sourced::<types::ByteStream>(data);
        let data = data.await?.unwrap();

        let log = ctx.log.clone();
        let task = ctx.task.clone();

        make_value!([data] {
            let data = data.await?;
            let _guard = STDOUT_MUTEX.lock().await;
            let paused = log.pause();
            io::copy_interactive(&task, &mut data.read(), &mut io::Blocking::new(std::io::stdout())).await?;
            drop(paused);
            Ok(())
        })
        .into()
    })
    .into()
}

fn stderr_fn() -> Value {
    ergo_function!(std::io::stderr, |ctx| {
        let data = ctx.args.next().ok_or("'data' missing")?;

        ctx.unused_arguments()?;

        let data = ctx.into_sourced::<types::ByteStream>(data);
        let data = data.await?.unwrap();

        let task = ctx.task.clone();

        make_value!([data] {
            let data = data.await?;
            io::copy_interactive(&task, &mut data.read(), &mut io::Blocking::new(std::io::stderr())).await?;
            Ok(())
        })
        .into()
    })
    .into()
}
