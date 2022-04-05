//! Execute (possibly with path-lookup) external programs.

use ergo_runtime::abi_stable::{
    ffi::OsString,
    rvec,
    std_types::{ROption, RString, RVec},
    type_erase::Erased,
    StableAbi,
};
use ergo_runtime::{
    context::ItemContent,
    depends,
    error::DiagnosticInfo,
    io::{self, Blocking},
    metadata::Source,
    nsid, traits, try_result,
    type_system::ErgoType,
    types, Context, Value,
};
use futures::future::FutureExt;
use futures::io::AsyncWriteExt;
use futures::lock::Mutex;
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

ergo_runtime::ConstantDependency!(CommandString);

impl ergo_runtime::GetDependenciesConstant for CommandString {
    fn get_depends(&self) -> ergo_runtime::DependenciesConstant {
        depends![CommandString::ergo_type(), self]
    }
}

/// A spawned child process.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Child {
    command_string: RString,
    stdin: Value,
    stdout: Value,
    stderr: Value,
    exit_status: Value,
}

const CHILD_SUCCESS_INDEX: &'static str = "success";

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

impl From<ExitStatus> for types::Number {
    fn from(e: ExitStatus) -> Self {
        types::Number::from(match e.0 {
            ROption::RNone => -1,
            ROption::RSome(i) => i,
        })
    }
}

ergo_runtime::ConstantDependency!(ExitStatus);

impl ergo_runtime::GetDependenciesConstant for ExitStatus {
    fn get_depends(&self) -> ergo_runtime::DependenciesConstant {
        depends![ExitStatus::ergo_type(), self]
    }
}

impl From<ExitStatus> for ergo_runtime::TypedValue<ExitStatus> {
    fn from(b: ExitStatus) -> Self {
        Self::constant(b)
    }
}

#[types::ergo_fn]
/// Execute an external program.
///
/// Arguments: `:program ^:arguments`
///
/// Both `program` and `arguments` must be convertible to `CommandString`. `String`, `Path`, and
/// `ByteStream` satisfy this. `Unset` and `Unit` values passed in `arguments` will be discarded.
///
/// Keyed Arguments:
/// * `Map :env`: A map of Strings to Strings where key-value pairs define the environment
/// variables to set while executing the program (default empty).
/// * `Into<Path> :pwd`: The working directory to set while executing the program (default none).
/// * `:retain-terminal`: If set, the spawned child is kept in the same terminal session.
///
/// When called, the child process is spawned and a Child-typed value is returned, which supports:
/// * Indices:
///   * `:stdin`: A function which may be passed an `Into ByteStream` argument to write to the
///   child's stdin, or a `Unit` argument to close the child's stdin.
///   * `:stdout`: The standard output `ByteStream` of the child.
///   * `:stderr`: The standard error `ByteStream` of the child.
///   * `:exit`: The exit status of the child (waiting for the child to terminate), as an `ExitStatus`.
///   * `:success`: A convenience index to wait for the child to complete, evaluating to `Unit` on
///   successful exit or an `Error` indicating the child's exit status, stderr, stdout, and command
///   line. __This is the nested value that is evaluated if a `Child` type is evaluated in sequence
///   in a block.__
/// * Conversion to `Bool` and `Number`, which is the same as converting `exit` to the type (exit
/// success/failure and exit code, respectively).
/// * Conversion to `String` and `ByteStream`, which is the same as converting `stdout` to the type
/// _after_ evaluating the `success` index to ensure no error occurred.
///
/// The `ExitStatus` type can be converted to `Bool` to check whether the program returned a
/// successful status. It can also be converted to `Number`; if the process exited as the result of
/// a signal (on unix OSes), it converts to `-1`.
pub async fn function(
    cmd: _,
    (env): [types::Map],
    (pwd): [_],
    (retain_terminal): [_],
    ...
) -> Value {
    let mut args = Vec::default();
    args.push(async move { traits::into::<CommandString>(cmd).await.map(Some) }.boxed());
    while let Some(arg) = REST.next() {
        args.push(
            async move {
                let mut arg = arg;
                drop(Context::eval(&mut arg).await);
                if arg.is_type::<types::Unset>() || arg.is_type::<types::Unit>() {
                    Ok(None)
                } else {
                    traits::into::<CommandString>(arg).await.map(Some)
                }
            }
            .boxed(),
        );
    }

    let args: Vec<_> = Context::global()
        .task
        .join_all(args)
        .await?
        .into_iter()
        .filter_map(|v| v)
        .collect();

    let pwd = match pwd {
        Some(v) => Some(traits::into::<types::Path>(v).await?),
        // TODO set dir if not specified?
        None => None,
    };

    let retain_terminal = match retain_terminal {
        Some(v) => traits::into::<types::Bool>(v).await?.as_ref().0,
        None => false,
    };

    REST.unused_arguments()?;

    let log = Context::global().log.sublog("exec");

    let mut args = args.iter();
    let cmd = args.next().unwrap();

    let mut command = Command::new(cmd.as_ref().0.as_ref());
    command.args(args.map(|v| v.as_ref().0.as_ref()));
    command.env_clear();
    if let Some(v) = env {
        let types::Map(env) = v.to_owned();
        for (k, v) in env {
            let k = Context::eval_as::<types::String>(k.into())
                .await?
                .to_owned()
                .into_string();
            let v = traits::into::<CommandString>(v).await?;
            command.env(k, v.as_ref().0.as_ref());
        }
    }
    if let Some(v) = pwd {
        command.current_dir(v.as_ref().as_ref());
    }

    let command_string = {
        fn string_lit(s: &mut String, v: &std::ffi::OsStr) {
            let v = v.to_string_lossy();
            s.reserve(2 + v.len());
            s.push('"');
            s.extend(v.escape_default());
            s.push('"');
        }
        let mut s = String::new();
        s += "{pwd=";
        match command.get_current_dir() {
            None => s += "<none>",
            Some(p) => s += &p.display().to_string(),
        }
        s += ",env={";
        let mut first = true;
        for (k, v) in command.get_envs() {
            if !first {
                s += ",";
            }
            string_lit(&mut s, k);
            if let Some(v) = v {
                s += "=";
                string_lit(&mut s, v);
            }
            first = false;
        }
        s += "} ";
        string_lit(&mut s, command.get_program());
        for a in command.get_args() {
            s += " ";
            string_lit(&mut s, a);
        }
        s
    };

    command.stdin(Stdio::piped());
    command.stdout(Stdio::piped());
    command.stderr(Stdio::piped());

    if !retain_terminal {
        // Disown process group so signals don't go to children.
        disown_pgroup(&mut command);
    }

    log.debug(format_args!("spawning child process: {}", &command_string));

    let mut child = command
        .spawn()
        .add_primary_label(ARGS_SOURCE.with("while spawning this process"))?;

    // Handle stdin
    let stdin = std::sync::Arc::new(Mutex::new(Some(Blocking::new(child.stdin.take().unwrap()))));
    let stdin = types::ergo_fn_value! {
        #[depends(^CALL_DEPENDS.clone())]
        #[cloning(stdin)]
        /// Send data to stdin of the child process.
        ///
        /// Arguments: `:value`
        ///
        /// If `value` is `Into ByteStream`, converts the value and copies the ByteStream to the
        /// child's stdin stream.
        ///
        /// If `value` is `Unit`, closes the stdin stream. Subsequent attempts to call this
        /// function will produce an error.
        async fn stdin(mut value: _) -> Value {
            let mut stdin = stdin.lock().await;
            if stdin.is_none() {
                Err(ergo_runtime::error! {
                    labels: [primary(ARGS_SOURCE.with(""))],
                    error: "called `stdin` after closing"
                })?;
            }

            Context::eval(&mut value).await?;
            if value.is_type::<types::Unit>() {
                stdin.take().unwrap().close().await?;
            } else {
                let stream = traits::into::<types::ByteStream>(value).await?;
                io::copy_interactive(&mut stream.as_ref().read(), stdin.as_mut().unwrap()).await.into_diagnostic()?;
            }
            types::Unit.into()
        }
    };

    // Handle stdout
    let stdout = Value::with_id(
        types::ByteStream::new(Blocking::new(child.stdout.take().unwrap())),
        depends![dyn ^CALL_DEPENDS.clone(), nsid!(exec::stdout)],
    );

    // Handle stderr
    let stderr = Value::with_id(
        types::ByteStream::new(Blocking::new(child.stderr.take().unwrap())),
        depends![dyn ^CALL_DEPENDS.clone(), nsid!(exec::stderr)],
    );

    // Handle running the child and getting the exit status
    let cs = command_string.clone();
    let exit_status = Context::global()
        .task
        .spawn_blocking(move || {
            let ret = child.wait().map_err(|e| {
                ergo_runtime::error! {
                    labels: [ primary(ARGS_SOURCE.with("while waiting for process to exit")) ],
                    error: e
                }
            });
            log.debug(format_args!("child process exited ({:?}): {}", ret, &cs));
            ret
        })
        .map(|r| r.and_then(|v| v))
        .shared();
    let exit_status = Value::dynamic(
        move || async move { ExitStatus::from(try_result!(exit_status.await)).into() },
        depends![dyn ^CALL_DEPENDS.clone(), nsid!(exec::exit_status)],
    );

    Value::with_id(
        Child {
            command_string: command_string.into(),
            stdin,
            stdout,
            stderr,
            exit_status,
        },
        depends![dyn ^CALL_DEPENDS, nsid!(exec::child)],
    )
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
    traits::IntoTyped::<CommandString>::add_impl::<types::Path>(traits);
    ergo_runtime::ergo_type_name!(traits, CommandString);

    // Child traits
    ergo_runtime::ergo_type_name!(traits, Child);
    impl traits::IntoTyped<types::Bool> for Child {
        async fn into_typed(self) -> Value {
            traits::into::<types::Bool>(self.as_ref().exit_status.clone()).await.into()
        }
    }
    impl traits::IntoTyped<types::Number> for Child {
        async fn into_typed(self) -> Value {
            traits::into::<types::Number>(self.as_ref().exit_status.clone()).await.into()
        }
    }
    impl traits::IntoTyped<types::String> for Child {
        async fn into_typed(self) -> Value {
            try_result!(traits::bind_no_error(self.clone().into(), types::Index(types::String::from(CHILD_SUCCESS_INDEX).into()).into()).await);
            traits::into::<types::String>(self.as_ref().stdout.clone()).await.into()
        }
    }
    impl traits::IntoTyped<types::ByteStream> for Child {
        async fn into_typed(self) -> Value {
            try_result!(traits::bind_no_error(self.clone().into(), types::Index(types::String::from(CHILD_SUCCESS_INDEX).into()).into()).await);
            traits::into::<types::ByteStream>(self.as_ref().stdout.clone()).await.into()
        }
    }
    impl traits::Nested for Child {
        async fn nested(&self) -> RVec<Value> {
            rvec![traits::bind(SELF_VALUE.clone(), types::Index(types::String::from(CHILD_SUCCESS_INDEX).into()).into()).await]
        }
    }
    impl traits::Bind for Child {
        async fn bind(&self, arg: Value) -> Value {
            let src = Source::get(&arg);
            let ind = try_result!(Context::eval_as::<types::Index>(arg).await).to_owned().0;
            let ind = try_result!(Context::eval_as::<types::String>(ind).await);
            let s = ind.as_ref().as_str();
            match s {
                "stdin" => self.stdin.clone(),
                "stdout" => self.stdout.clone(),
                "stderr" => self.stderr.clone(),
                "exit" => self.exit_status.clone(),
                "success" => {
                    let command_string = self.command_string.clone();
                    let stdout = self.stdout.clone();
                    let stderr = self.stderr.clone();
                    let exit_status = self.exit_status.clone();
                    Value::dynamic(move || async move {
                            let exit_status = try_result!(Context::eval_as::<ExitStatus>(exit_status).await);
                            if exit_status.as_ref().success() {
                                types::Unit.into()
                            } else {
                                let stdout = try_result!(traits::into::<types::String>(stdout).await);
                                let stderr = try_result!(traits::into::<types::String>(stderr).await);

                                ergo_runtime::error! {
                                    labels: [primary(src.with(""))],
                                    notes: [
                                        format_args!("command was: {}", command_string),
                                        format_args!("exit status was: {}", exit_status.as_ref()),
                                        format_args!("stdout was: {}", stdout.as_ref()),
                                        format_args!("stderr was: {}", stderr.as_ref())
                                    ],
                                    error: "command returned failure exit status"
                                }.into()
                            }
                        },
                        depends![dyn SELF_VALUE, nsid!(exec::success)]
                    )
                }
                _ => {
                    ergo_runtime::error! {
                        labels: [ primary(src.with("")) ],
                        notes: [ "supported indices: `stdin`, `stdout`, `stderr`, `exit`, `success`" ],
                        error: "unrecognized Child index"
                    }.into()
                }
            }
        }
    }

    // ExitStatus traits
    traits::IntoTyped::<types::Bool>::add_impl::<ExitStatus>(traits);
    traits::IntoTyped::<types::Number>::add_impl::<ExitStatus>(traits);
    ergo_runtime::ergo_display_basic!(traits, ExitStatus);
    ergo_runtime::ergo_type_name!(traits, ExitStatus);

    impl traits::Stored for ExitStatus {
        async fn put(&self, _ctx: &traits::StoredContext, mut into: ItemContent) -> ergo_runtime::RResult<()> {
            bincode::serialize_into(&mut into, &self.0.clone().into_option())
                .add_primary_label(Source::get(SELF_VALUE).with("while storing this value"))
                .map_err(|e| e.into())
                .into()
        }

        async fn get(_ctx: &traits::StoredContext, mut from: ItemContent) -> ergo_runtime::RResult<Erased>
        {
            bincode::deserialize_from(&mut from)
                .map(|status: Option<i32>| Erased::new(ExitStatus(status.into())))
                .into_diagnostic()
                .map_err(|e| e.into())
                .into()
        }
    }
}

// Only run these tests on unix, where we assume the existence of some programs.
#[cfg(all(test, unix))]
mod test {
    ergo_script::tests! {
        fn exec(t) {
            t.assert_success("self:exec echo hello; ()");
            t.assert_success("self:exec echo hello |>:success");
        }

        fn exec_fail(t) {
            t.assert_fail("self:exec false; ()");
            t.assert_fail("self:exec false |>:success");
        }

        fn exit_status_number(t) {
            t.assert_eq("self:Number:from <| self:exec false |>:exit", "self:Number:new 1");
            t.assert_eq("self:Number:from <| self:exec true |>:exit", "self:Number:new 0");
            t.assert_eq("self:Number:from <| self:exec false", "self:Number:new 1");
            t.assert_eq("self:Number:from <| self:exec true", "self:Number:new 0");
        }

        fn exit_bool(t) {
            t.assert_eq("self:Bool:from <| self:exec false |>:exit", "self:Bool:false");
            t.assert_eq("self:Bool:from <| self:exec true |>:exit", "self:Bool:true");
            t.assert_eq("self:Bool:from <| self:exec false", "self:Bool:false");
            t.assert_eq("self:Bool:from <| self:exec true", "self:Bool:true");
        }

        fn exec_unset_args(t) {
            t.assert_eq("self:String:from <| self:exec echo $unset hello $unset world |>:stdout", "\"hello world\\n\"");
            t.assert_fail("self:exec $unset echo |>:success");
        }

        fn exec_unit_args(t) {
            t.assert_eq("self:String:from <| self:exec echo () hello () world |>:stdout", "\"hello world\\n\"");
            t.assert_fail("self:exec () echo |>:success");
        }

        fn stdin(t) {
            t.assert_eq("child = self:exec cat; child:stdin hello; child:stdin (); self:String:from child:stdout", "\"hello\"");
        }

        fn to_string(t) {
            t.assert_eq("self:String:from <| self:exec echo abc", "\"abc\\n\"");
        }
    }
}
