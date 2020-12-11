//! Execute (possibly with path-lookup) external programs.

use abi_stable::{std_types::ROption, StableAbi};
use ergo_runtime::{
    context_ext::AsContext, ergo_function, namespace_id, traits, traits::IntoTyped, types,
    ContextExt,
};
use futures::channel::oneshot::channel;
use futures::future::FutureExt;
use futures::sink::SinkExt;
use futures::stream::StreamExt;
use grease::{
    depends,
    ffi::OsString,
    make_value,
    path::PathBuf,
    runtime::{
        io::{wrap, Blocking, TokioWrapped},
        ItemContent,
    },
    types::GreaseType,
    value::{Dependencies, Value},
    Error,
};
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
        match self.0 {
            ROption::RNone => write!(f, "signal"),
            ROption::RSome(i) => write!(f, "{}", i),
        }
    }
}

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

type PipeBuf = std::io::Cursor<Box<[u8]>>;

struct PipeSend {
    send: futures::channel::mpsc::UnboundedSender<tokio::io::Result<PipeBuf>>,
}

type PipeRecv = tokio_util::io::StreamReader<
    futures::stream::Fuse<futures::channel::mpsc::UnboundedReceiver<tokio::io::Result<PipeBuf>>>,
    PipeBuf,
>;

fn pipe() -> (TokioWrapped<PipeSend>, TokioWrapped<PipeRecv>) {
    let (send, recv) = futures::channel::mpsc::unbounded::<tokio::io::Result<PipeBuf>>();
    (
        wrap(PipeSend { send }),
        wrap(tokio_util::io::StreamReader::new(recv.fuse())),
    )
}

impl tokio::io::AsyncWrite for PipeSend {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        buf: &[u8],
    ) -> std::task::Poll<tokio::io::Result<usize>> {
        let len = buf.len();
        futures::future::Future::poll(
            std::pin::Pin::new(
                &mut self
                    .get_mut()
                    .send
                    .send(Ok(std::io::Cursor::new(buf.to_vec().into_boxed_slice()))),
            ),
            cx,
        )
        .map(move |v| {
            v.map_err(|_| tokio::io::ErrorKind::BrokenPipe.into())
                .map(move |()| len)
        })
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context,
    ) -> std::task::Poll<tokio::io::Result<()>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<tokio::io::Result<()>> {
        futures::future::Future::poll(std::pin::Pin::new(&mut self.get_mut().send.close()), cx)
            .map(|v| v.map_err(|_| tokio::io::ErrorKind::BrokenPipe.into()))
    }
}

enum ChannelRead<R> {
    Waiting(futures::channel::oneshot::Receiver<R>),
    Read(R),
}

impl<R: grease::runtime::io::AsyncRead + std::marker::Unpin> ChannelRead<R> {
    pub fn new(channel: futures::channel::oneshot::Receiver<R>) -> Self {
        ChannelRead::Waiting(channel)
    }
}

impl<R> grease::runtime::io::AsyncRead for ChannelRead<R>
where
    R: grease::runtime::io::AsyncRead + std::marker::Unpin,
{
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &grease::runtime::TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<Result<usize, Error>> {
        let me = &mut *self;
        use futures::future::Future;
        use std::task::Poll::*;
        loop {
            let new_me = match me {
                ChannelRead::Waiting(r) => match std::pin::Pin::new(r).poll(cx) {
                    Pending => return Pending,
                    Ready(Err(_)) => return Ready(Ok(0)),
                    Ready(Ok(v)) => ChannelRead::Read(v),
                },
                ChannelRead::Read(r) => return std::pin::Pin::new(r).poll_read(cx, task, buf),
            };
            *me = new_me;
        }
    }
}

struct ReadWhile<R, Fut> {
    read: R,
    read_done: bool,
    fut: Fut,
    fut_done: bool,
}

impl<R, Fut> ReadWhile<R, Fut> {
    pub fn new(read: R, fut: Fut) -> Self {
        ReadWhile {
            read,
            read_done: false,
            fut,
            fut_done: false,
        }
    }
}

impl<T, R, Fut> grease::runtime::io::AsyncRead for ReadWhile<R, Fut>
where
    R: grease::runtime::io::AsyncRead + std::marker::Unpin,
    Fut: futures::future::Future<Output = Result<T, Error>> + std::marker::Unpin,
{
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &grease::runtime::TaskManager,
        buf: &mut [u8],
    ) -> std::task::Poll<Result<usize, Error>> {
        use std::task::Poll::*;
        let result = if !self.read_done {
            let me = &mut *self;
            match std::pin::Pin::new(&mut me.read).poll_read(cx, task, buf) {
                Ready(Err(e)) => return Ready(Err(e)),
                Ready(Ok(0)) => {
                    me.read_done = true;
                    Ready(Ok(0))
                }
                otherwise => otherwise,
            }
        } else {
            Pending
        };
        if !self.fut_done {
            let me = &mut *self;
            match std::pin::Pin::new(&mut me.fut).poll(cx) {
                Ready(Err(e)) => return Ready(Err(e)),
                Ready(Ok(_)) => {
                    me.fut_done = true;
                }
                _ => (),
            }
        };

        if self.fut_done && self.read_done {
            Ready(Ok(0))
        } else if let Ready(Ok(0)) = result {
            Pending
        } else {
            result
        }
    }
}

struct BoundTo<MasterFut, Fut> {
    master: MasterFut,
    other: Option<Fut>,
}

impl<M, Fut> BoundTo<M, Fut> {
    pub fn new(master: M, other: Fut) -> Self {
        BoundTo {
            master,
            other: Some(other),
        }
    }
}

impl<MasterFut, Fut, MT, FT, E> futures::future::Future for BoundTo<MasterFut, Fut>
where
    MasterFut: futures::future::Future<Output = Result<MT, E>> + std::marker::Unpin,
    Fut: futures::future::Future<Output = Result<FT, E>> + std::marker::Unpin,
{
    type Output = MasterFut::Output;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        let me = &mut *self;
        use std::task::Poll::*;
        if let Some(o) = &mut me.other {
            match std::pin::Pin::new(o).poll(cx) {
                Ready(Err(e)) => return Ready(Err(e)),
                Ready(Ok(_)) => {
                    me.other = None;
                }
                Pending => (),
            }
        }

        std::pin::Pin::new(&mut me.master).poll(cx)
    }
}

pub fn function() -> Value {
    ergo_function!(independent std::exec,
        "Execute an external program.

Arguments: <program> [arguments...]
Both `program` and `arguments` must be convertible to CommandString. By default String, Path, and ByteStream satisfy
this. 

Keyword Arguments:
* <env: Map>: A map of Strings to Strings where key-value pairs define the environment variables to
  set while executing the program.
* <pwd: Into<Path>>: The working directory to set while executing the program.
* <stdin: Into<ByteStream>>: The stdin to pipe into the program.

Programs are by default run without any working directory or environment variables.

This returns a map with the following keys:
* <stdout: ByteStream>: The standard output stream from the program.
* <stderr: ByteStream>: The standard output stream from the program.
* <exit-status: ExitStatus>: The exit status of the program.
* <complete: ()>: A value which returns successfully when the program does.

Note that `complete` _won't_ cause an error to occur if `exit-status` is used in the script.
Also note that if `complete` throws an error, it will include the stdout or stderr values if they are unused in the
script.
",
    |ctx| {
        let cmd = ctx.args.next().ok_or("no command provided")?;

        let cmd = ctx.into_sourced::<CommandString>(cmd);
        let cmd = cmd.await?.unwrap();

        let mut args = Vec::default();
        while let Some(arg) = ctx.args.next() {
            args.push(
                {
                    let arg = ctx.into_sourced::<CommandString>(arg);
                    arg.await?.unwrap()
                }
            );
        }

        let env = match ctx.args.kw("env") {
            Some(v) => {
                let env = ctx.source_value_as::<types::Map>(v);
                Some(env.await?)
            }
            None => None
        };

        let dir = match ctx.args.kw("pwd") {
            Some(v) => {
                let pwd = ctx.into_sourced::<PathBuf>(v);
                Some(pwd.await?.unwrap())
            }
            // TODO set dir if not specified?
            None => None
        };

        let stdin = match ctx.args.kw("stdin") {
            Some(v) => {
                let stdin = ctx.into_sourced::<types::ByteStream>(v);
                Some(stdin.await?.unwrap())
            }
            None => None
        };

        ctx.unused_arguments()?;

        let mut deps = depends![cmd, ^@args];
        let mut unordered_deps = Vec::new();
        if let Some(v) = &env {
            unordered_deps.push((&**v).into());
        }
        if let Some(v) = &dir {
            unordered_deps.push(v.into());
        }
        if let Some(v) = &stdin {
            unordered_deps.push(v.into());
        }
        deps += Dependencies::unordered(unordered_deps);

        let rt = ctx;
        let ctx: grease::runtime::Context = rt.as_context().clone();
        let log = ctx.log.sublog("exec");

        // Create channels for outputs
        let (send_stdout, rcv_stdout) = channel();
        let (send_stderr, rcv_stderr) = channel();
        let (send_status, rcv_status) = channel();

        let run_command = make_value!([namespace_id!(std::exec::complete), ^deps] {
            let mut vals: Vec<Value> = Vec::new();
            vals.push(cmd.clone().into());
            for v in &args {
                vals.push(v.clone().into());
            }
            if let Some(v) = &env {
                vals.push((**v).clone().into());
            }
            if let Some(v) = &dir {
                vals.push(v.clone().into());
            }
            if let Some(v) = &stdin {
                vals.push(v.clone().into());
            }
            ctx.task.join_all(vals).await?;

            let mut command = Command::new(cmd.forced_value().0.as_ref());
            let arg_vals: Vec<_> = args.iter().map(|a| a.forced_value()).collect();
            command.args(arg_vals.iter().map(|v| v.0.as_ref()));
            command.env_clear();
            if let Some(v) = env {
                let (source, v) = v.take();
                let types::Map(env) = v.forced_value().owned();
                for (k,v) in env {
                    let k = ctx.source_value_as::<types::String>(source.clone().with(k));
                    let k = k.await?.unwrap().await?.owned().into_string();
                    let v = ctx.into_typed::<CommandString>(v).await
                        .map_err(|e| format!("env key {}: {}", k, e))?;
                    command.env(k, v.await?.0.as_ref());
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

            // Disown process group so signals don't go to children.
            disown_pgroup(&mut command);

            let mut child = command.spawn()?;

            let stdin = stdin.map(|v| v.forced_value().read());

            let input = if let (Some(input),Some(mut v)) = (child.stdin.take(), stdin) {
                let mut b = Blocking::new(input);
                let task = ctx.task.clone();
                async move {
                    grease::runtime::io::copy_interactive(&task, &mut v, &mut b).await
                }.boxed()
            } else {
                futures::future::ok(0).boxed()
            };

            let (mut out_pipe_send, out_pipe_recv) = pipe();
            let mut cstdout = Blocking::new(child.stdout.take().unwrap());
            let output = grease::runtime::io::copy_interactive(&ctx.task, &mut cstdout, &mut out_pipe_send);
            let out_pipe_recv = if !send_stdout.is_canceled() {
                drop(send_stdout.send(out_pipe_recv));
                None
            } else {
                Some(out_pipe_recv)
            };

            let (mut err_pipe_send, err_pipe_recv) = pipe();
            let mut cstderr = Blocking::new(child.stderr.take().unwrap());
            let error = grease::runtime::io::copy_interactive(&ctx.task, &mut cstderr, &mut err_pipe_send);
            let err_pipe_recv = if !send_stderr.is_canceled() {
                drop(send_stderr.send(err_pipe_recv));
                None
            } else {
                Some(err_pipe_recv)
            };

            let exit_status = ctx.task.spawn_blocking(move || {
                child.wait().map_err(Error::from)
            }).map(|r| r.and_then(|v| v));

            // Only run stdin while waiting for exit status
            let exit_status = BoundTo::new(exit_status, input);

            let (exit_status, _) = ctx.task.join(exit_status, ctx.task.join(output, error)).await?;

            log.debug(format!("child process exited: {:?}", command));

            // These pipes should be dropped here so that, if an error occurred, the recv pipes
            // terminate correctly if gathering stdout/stderr.
            drop(out_pipe_send);
            drop(err_pipe_send);

            macro_rules! fetch_named {
                ( $output:expr, $name:expr ) => {
                    if let Some(mut recv_pipe) = $output {
                        let mut s = String::new();
                        use grease::runtime::io::AsyncReadExt;
                        if recv_pipe.read_to_string(&ctx.task, &mut s).await.is_ok() {
                            if !s.is_empty() {
                                Some(format!("\n{}:\n{}", $name, s))
                            }
                            else { None }
                        } else {
                            None
                        }
                    } else {
                        None
                    }.unwrap_or_default()
                }
            }

            if send_status.is_canceled() {
                if !exit_status.success() {
                    let stdout = fetch_named!(out_pipe_recv, "stdout");
                    let stderr = fetch_named!(err_pipe_recv, "stderr");
                    return Err(format!("command returned failure exit status{}{}", stdout, stderr).into());
                }
            } else {
                drop(send_status.send(exit_status));
            }
            Ok(types::Unit)
        });

        let stdout = make_value!((run_command) [namespace_id!(std::exec::stdout)] {
            Ok(types::ByteStream::new(ReadWhile::new(ChannelRead::new(rcv_stdout), run_command)))
        });
        let stderr = make_value!((run_command) [namespace_id!(std::exec::stderr)] {
            Ok(types::ByteStream::new(ReadWhile::new(ChannelRead::new(rcv_stderr), run_command)))
        });
        let exit_status = make_value!((run_command) [namespace_id!(std::exec::exit_status)] { run_command.await?; Ok(ExitStatus::from(rcv_status.await?)) });

        crate::grease_string_map! {
            "Outputs from an `exec` call."
            "stdout": "The standard output byte stream." = rt.imbue_error_context(stdout.into(), "while evaluating stdout of exec command"),
            "stderr": "The standard error byte stream." = rt.imbue_error_context(stderr.into(), "while evaluating stderr of exec command"),
            "exit-status": "The exit status, which may be used in boolean contexts to check for successful exit." = rt.imbue_error_context(exit_status.into(), "while evaluating exit_status of exec command"),
            "complete": "A unit value for the successful completion of the launched program." = rt.imbue_error_context(run_command.into(), "while evaluating result of exec command")
        }
    })
    .into()
}

#[cfg(unix)]
fn disown_pgroup(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    unsafe {
        cmd.pre_exec(|| {
            if libc::setsid() == -1 {
                Err(errno::errno().into())
            } else {
                Ok(())
            }
        });
    }
}

#[cfg(windows)]
fn disown_pgroup(cmd: &mut Command) {}

grease::grease_traits_fn! {
    // CommandString traits
    IntoTyped::<CommandString>::add_impl::<types::String>(traits);
    IntoTyped::<CommandString>::add_impl::<PathBuf>(traits);
    ergo_runtime::grease_type_name!(traits, CommandString);

    // ExitStatus traits
    IntoTyped::<bool>::add_impl::<ExitStatus>(traits);
    ergo_runtime::grease_display_basic!(traits, ExitStatus);
    ergo_runtime::grease_type_name!(traits, ExitStatus);
    traits::ValueByContent::add_impl::<ExitStatus>(traits);

    impl traits::Stored for ExitStatus {
        async fn put(&self, _ctx: &traits::StoredContext, mut into: ItemContent) {
            bincode::serialize_into(&mut into, &self.0.clone().into_option())?
        }

        async fn get(_ctx: &traits::StoredContext, mut from: ItemContent) -> grease::type_erase::Erased {
            let p: Option<i32> = bincode::deserialize_from(&mut from)?;
            grease::type_erase::Erased::new(ExitStatus(p.into()))
        }
    }
}
