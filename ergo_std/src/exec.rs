//! Execute (possibly with path-lookup) external programs.

use ergo_runtime::abi_stable::{ffi::OsString, std_types::ROption, type_erase::Erased, StableAbi};
use ergo_runtime::{
    context::ItemContent,
    depends,
    io::{self, wrap, Blocking, TokioWrapped},
    nsid, traits, try_result,
    type_system::ErgoType,
    types, Dependencies, Error, Source, Value,
};
use futures::channel::oneshot::channel;
use futures::future::FutureExt;
use futures::stream::StreamExt;
use std::process::{Command, Stdio};

/// Strings used for commands and arguments.
#[derive(Clone, Debug, ErgoType, Hash, StableAbi)]
#[repr(C)]
pub struct CommandString(pub OsString);

impl From<types::String> for CommandString {
    fn from(s: types::String) -> Self {
        CommandString(std::ffi::OsString::from(s.into_string()).into())
    }
}

impl From<types::Path> for CommandString {
    fn from(p: types::Path) -> Self {
        CommandString(p.0.into())
    }
}

ergo_runtime::HashAsDependency!(CommandString);

/// The exit status of a command.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ErgoType, StableAbi)]
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

impl From<ExitStatus> for types::Bool {
    fn from(e: ExitStatus) -> Self {
        types::Bool(e.success())
    }
}

impl From<ExitStatus> for ergo_runtime::TypedValue<ExitStatus> {
    fn from(e: ExitStatus) -> Self {
        ergo_runtime::TypedValue::constant(e)
    }
}

impl From<&'_ ExitStatus> for Dependencies {
    fn from(e: &'_ ExitStatus) -> Self {
        depends![ExitStatus::ergo_type(), e]
    }
}

ergo_runtime::HashAsDependency!(ExitStatus);

#[types::ergo_fn]
/// Execute an external program.
///
/// Arguments: `:program ^:arguments`
///
/// Both `program` and `arguments` must be convertible to `CommandString`. By default `String`, `Path`, and `ByteStream` satisfy
/// this.
///
/// Keyed Arguments:
/// * `Map :env`: A map of Strings to Strings where key-value pairs define the environment
/// variables to set while executing the program (default empty).
/// * `Into<Path> :pwd`: The working directory to set while executing the program (default none).
/// * `Into<ByteStream> :stdin`: The stdin to pipe into the program (default none).
/// * `:retain-terminal`: If set, the spawned child is kept in the same terminal session.
///
/// This returns a Map with the following indices:
/// * `:stdout`: The standard output stream from the program, as a `ByteStream`.
/// * `:stderr`: The standard error stream from the program, as a `ByteStream`.
/// * `:exit-status`: The exit status of the program, as an `ExitStatus`.
/// * `:complete`: Waits for the program to complete successfully, evaluating to `Unit` or an
/// `Error`.
///
/// Note that the `complete` value will not evaluate to an error if `exit-status` is used in the
/// script. It also won't include each of stdout or stderr in the error string if they are used in
/// the script.
///
/// The `ExitStatus` type can be converted to `Bool` to check whether the program returned a successful status.
pub async fn function(
    cmd: _,
    (env): [types::Map],
    (pwd): [_],
    (stdin): [_],
    (retain_terminal): [_],
    ...
) -> Value {
    let mut args = Vec::default();
    args.push(traits::into_sourced::<CommandString>(CONTEXT, cmd));
    while let Some(arg) = REST.next() {
        args.push(traits::into_sourced::<CommandString>(CONTEXT, arg));
    }

    let args: Vec<_> = try_result!(CONTEXT.task.join_all(args).await)
        .into_iter()
        .map(Source::unwrap)
        .collect();

    let pwd = match pwd {
        Some(v) => {
            Some(try_result!(traits::into_sourced::<types::Path>(CONTEXT, v).await).unwrap())
        }
        // TODO set dir if not specified?
        None => None,
    };

    let stdin = match stdin {
        Some(v) => {
            Some(try_result!(traits::into_sourced::<types::ByteStream>(CONTEXT, v).await).unwrap())
        }
        None => None,
    };

    let retain_terminal = match retain_terminal {
        Some(v) => {
            try_result!(traits::into_sourced::<types::Bool>(CONTEXT, v).await)
                .unwrap()
                .as_ref()
                .0
        }
        None => false,
    };

    try_result!(REST.unused_arguments());

    let log = CONTEXT.log.sublog("exec");

    let mut args = args.iter();
    let cmd = args.next().unwrap();

    let mut command = Command::new(cmd.as_ref().0.as_ref());
    command.args(args.map(|v| v.as_ref().0.as_ref()));
    command.env_clear();
    if let Some(v) = env {
        let v = v.unwrap();
        let types::Map(env) = v.to_owned();
        for (k, v) in env {
            let k = try_result!(CONTEXT.eval_as::<types::String>(k).await)
                .unwrap()
                .to_owned()
                .into_string();
            let v = try_result!(traits::into_sourced::<CommandString>(CONTEXT, v).await);
            command.env(k, v.value().as_ref().0.as_ref());
        }
    }
    if let Some(v) = pwd {
        command.current_dir(v.as_ref().as_ref());
    }

    if stdin.is_some() {
        command.stdin(Stdio::piped());
    }
    command.stdout(Stdio::piped());
    command.stderr(Stdio::piped());

    if !retain_terminal {
        // Disown process group so signals don't go to children.
        disown_pgroup(&mut command);
    }

    // Create channels for outputs
    let (send_stdout, rcv_stdout) = channel();
    let (send_stderr, rcv_stderr) = channel();
    let (send_status, rcv_status) = channel();

    let task = CONTEXT.task.clone();
    let run_command = async move {
        log.debug(format!("spawning child process: {:?}", command));

        let mut child = command.spawn()?;

        // Handle stdin
        let stdin = stdin.map(|v| v.as_ref().read());
        let input = if let (Some(input), Some(mut v)) = (child.stdin.take(), stdin) {
            let mut b = Blocking::new(input);
            let task = task.clone();
            async move { ergo_runtime::io::copy_interactive(&task, &mut v, &mut b).await }.boxed()
        } else {
            futures::future::ok(0).boxed()
        };

        // Handle stdout
        let (mut out_pipe_send, out_pipe_recv) = pipe();
        let mut cstdout = Blocking::new(child.stdout.take().unwrap());
        let output = ergo_runtime::io::copy_interactive(&task, &mut cstdout, &mut out_pipe_send);
        let out_pipe_recv = if !send_stdout.is_canceled() {
            drop(send_stdout.send(out_pipe_recv));
            None
        } else {
            Some(out_pipe_recv)
        };

        // Handle stderr
        let (mut err_pipe_send, err_pipe_recv) = pipe();
        let mut cstderr = Blocking::new(child.stderr.take().unwrap());
        let error = io::copy_interactive(&task, &mut cstderr, &mut err_pipe_send);
        let err_pipe_recv = if !send_stderr.is_canceled() {
            drop(send_stderr.send(err_pipe_recv));
            None
        } else {
            Some(err_pipe_recv)
        };

        // Handle running the child and getting the exit status
        let exit_status = task
            .spawn_blocking(move || child.wait().map_err(Error::from))
            .map(|r| r.and_then(|v| v));

        // Only run stdin while waiting for exit status
        let exit_status = BoundTo::new(exit_status, input);

        // Finally await everything
        let exit_status = {
            // TODO let _guard = super::task::ParentTask::remain_active();
            futures::future::try_join3(exit_status, output, error)
                .await?
                .0
        };

        log.debug(format!("child process exited: {:?}", command));

        // These pipes should be dropped here so that, if an error occurred, the recv pipes
        // terminate correctly if gathering stdout/stderr.
        drop(out_pipe_send);
        drop(err_pipe_send);

        // Produce the final result
        if send_status.is_canceled() {
            if !exit_status.success() {
                macro_rules! fetch_named {
                    ( $output:expr, $name:expr ) => {
                        if let Some(mut recv_pipe) = $output {
                            let mut s = String::new();
                            use io::AsyncReadExt;
                            if recv_pipe.read_to_string(&task, &mut s).await.is_ok() {
                                if !s.is_empty() {
                                    Some(format!("\n{}:\n{}", $name, s))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                        .unwrap_or_default()
                    };
                }
                let stdout = fetch_named!(out_pipe_recv, "stdout");
                let stderr = fetch_named!(err_pipe_recv, "stderr");
                return Err(
                    format!("command returned failure exit status{}{}", stdout, stderr).into(),
                );
            }
        } else {
            drop(send_status.send(exit_status));
        }

        ergo_runtime::Result::Ok(())
    }
    .shared();

    // Create output values
    let complete = {
        let run_command = run_command.clone();
        Value::dyn_new(
            |_| async move {
                try_result!(run_command.await);
                types::Unit.into()
            },
            depends![^CALL_DEPENDS.clone(), nsid!(exec::complete)],
        )
    };

    let stdout = Value::constant_deps(
        types::ByteStream::new(ReadWhile::new(
            ChannelRead::new(rcv_stdout),
            run_command.clone(),
        )),
        depends![^CALL_DEPENDS.clone(), nsid!(exec::stdout)],
    );

    let stderr = Value::constant_deps(
        types::ByteStream::new(ReadWhile::new(
            ChannelRead::new(rcv_stderr),
            run_command.clone(),
        )),
        depends![^CALL_DEPENDS.clone(), nsid!(exec::stderr)],
    );

    let exit_status = Value::dyn_new(
        |_| async move {
            try_result!(run_command.await);
            ExitStatus::from(try_result!(rcv_status.await)).into()
        },
        depends![^CALL_DEPENDS, nsid!(exec::exit_status)],
    );

    crate::make_string_map! { source ARGS_SOURCE,
        "stdout" = stdout,
        "stderr" = stderr,
        "exit-status" = exit_status,
        "complete" = complete
    }
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

ergo_runtime::type_system::ergo_traits_fn! {
    // CommandString traits
    traits::IntoTyped::<CommandString>::add_impl::<types::String>(traits);
    traits::IntoTyped::<CommandString>::add_impl::<types::String>(traits);
    ergo_runtime::ergo_type_name!(traits, CommandString);

    // ExitStatus traits
    traits::IntoTyped::<types::Bool>::add_impl::<ExitStatus>(traits);
    ergo_runtime::ergo_display_basic!(traits, ExitStatus);
    ergo_runtime::ergo_type_name!(traits, ExitStatus);
    traits::ValueByContent::add_impl::<ExitStatus>(traits);

    impl traits::Stored for ExitStatus {
        async fn put(&self, _ctx: &traits::StoredContext, mut into: ItemContent) -> ergo_runtime::RResult<()> {
            bincode::serialize_into(&mut into, &self.0.clone().into_option()).map_err(|e| e.into()).into()
        }

        async fn get(_ctx: &traits::StoredContext, mut from: ItemContent) -> ergo_runtime::RResult<Erased>
        {
            bincode::deserialize_from(&mut from)
                .map(|status: Option<i32>| Erased::new(ExitStatus(status.into())))
                .map_err(|e| e.into())
                .into()
        }
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
        use futures::sink::Sink;
        use std::task::Poll::*;
        let mut me = std::pin::Pin::new(&mut self.get_mut().send);
        match me.as_mut().poll_ready(cx) {
            Pending => return Pending,
            Ready(Err(_)) => return Ready(Err(tokio::io::ErrorKind::BrokenPipe.into())),
            Ready(Ok(())) => (),
        }

        Ready(
            if me
                .start_send(Ok(std::io::Cursor::new(buf.to_vec().into_boxed_slice())))
                .is_err()
            {
                Err(tokio::io::ErrorKind::BrokenPipe.into())
            } else {
                Ok(buf.len())
            },
        )
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<tokio::io::Result<()>> {
        use futures::sink::Sink;
        let me = std::pin::Pin::new(&mut self.get_mut().send);
        me.poll_flush(cx)
            .map(|r| r.map_err(|_| tokio::io::ErrorKind::BrokenPipe.into()))
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<tokio::io::Result<()>> {
        use futures::sink::Sink;
        let me = std::pin::Pin::new(&mut self.get_mut().send);
        me.poll_close(cx)
            .map(|r| r.map_err(|_| tokio::io::ErrorKind::BrokenPipe.into()))
    }
}

enum ChannelRead<R> {
    Waiting(futures::channel::oneshot::Receiver<R>),
    Read(R),
}

impl<R: ergo_runtime::io::AsyncRead + std::marker::Unpin> ChannelRead<R> {
    pub fn new(channel: futures::channel::oneshot::Receiver<R>) -> Self {
        ChannelRead::Waiting(channel)
    }
}

impl<R> ergo_runtime::io::AsyncRead for ChannelRead<R>
where
    R: ergo_runtime::io::AsyncRead + std::marker::Unpin,
{
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &ergo_runtime::context::TaskManager,
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

impl<T, R, Fut> ergo_runtime::io::AsyncRead for ReadWhile<R, Fut>
where
    R: ergo_runtime::io::AsyncRead + std::marker::Unpin,
    Fut: futures::future::Future<Output = Result<T, Error>> + std::marker::Unpin,
{
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
        task: &ergo_runtime::context::TaskManager,
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

// Only run these tests on unix, where we assume the existence of some programs.
#[cfg(all(test, unix))]
mod test {
    ergo_script::test! {
        fn exec(t) {
            t.assert_success("self:exec echo hello |>:complete")
        }
    }

    ergo_script::test! {
        fn exec_fail(t) {
            t.assert_fail("self:exec false |>:complete")
        }
    }
}
