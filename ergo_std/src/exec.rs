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
use ergo_runtime::{ergo_function, namespace_id, traits, traits::IntoTyped, types};
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
}

pub fn function() -> Value {
    ergo_function!(independent std::exec, |ctx| {
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
                        .get::<IntoTyped<types::ByteStream>>(&v)
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
        
        let run_command = make_value!([namespace_id!(std::exec::complete), ^deps] {
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
        let stdout = make_value!((run_command) [namespace_id!(std::exec::stdout)] { run_command.await?; Ok(types::ByteStream::new(std::io::Cursor::new(rcv_stdout.await?))) });
        let stderr = make_value!((run_command) [namespace_id!(std::exec::stderr)] { run_command.await?; Ok(types::ByteStream::new(std::io::Cursor::new(rcv_stderr.await?))) });
        let exit_status = make_value!((run_command) [namespace_id!(std::exec::exit_status)] { run_command.await?; Ok(ExitStatus::from(rcv_status.await?)) });
        
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
}
