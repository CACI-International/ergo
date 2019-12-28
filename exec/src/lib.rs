use grease::{
    channel, depends, future, item_name, make_value, Context, Dependencies, GetValueType, ItemName,
    Plan, TraitImpl, TypedValue, ValueType,
};
use os_str_bytes::OsStringBytes;
use std::collections::BTreeMap;
use std::convert::TryInto;
use std::ffi::{OsStr, OsString};
use std::hash::Hash;
use std::io::{Read, Write};
use std::path::PathBuf;

/// Strings used for commands and arguments.
#[derive(Clone, Debug, GetValueType, Hash)]
pub struct CommandString(OsString);

impl<T: Into<OsString>> From<T> for CommandString {
    fn from(v: T) -> Self {
        CommandString(v.into())
    }
}

impl AsRef<OsStr> for CommandString {
    fn as_ref(&self) -> &OsStr {
        &self.0
    }
}

/// Strings used for stdin to commands.
#[derive(Clone, Debug, GetValueType, Hash)]
pub struct StdinString(OsString);

impl<T: Into<OsString>> From<T> for StdinString {
    fn from(v: T) -> Self {
        StdinString(v.into())
    }
}

impl AsRef<OsStr> for StdinString {
    fn as_ref(&self) -> &OsStr {
        &self.0
    }
}

fn vec_to_os_string_impl<T: From<OsString> + GetValueType>() -> TraitImpl {
    TraitImpl::for_trait::<grease::IntoTyped<T>>(|v| {
        v.typed::<Vec<u8>>()
            .unwrap()
            .map(|vec| {
                OsString::from_bytes(vec)
                    .map(T::from)
                    .map_err(|e| e.to_string())
            })
            .into()
    })
}

/// Traits for types in this crate.
pub fn trait_generator(v: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    if *v == String::value_type() {
        vec![
            grease::impl_into::<String, CommandString>().into(),
            grease::impl_into::<String, StdinString>().into(),
        ]
    } else if *v == PathBuf::value_type() {
        vec![
            grease::impl_into::<PathBuf, CommandString>(),
            TraitImpl::for_trait::<grease::IntoTyped<StdinString>>(|v| {
                v.typed::<PathBuf>()
                    .unwrap()
                    .map(|path| {
                        let mut v = Vec::new();
                        std::fs::File::open(path)
                            .map_err(|e| e.to_string())?
                            .read_to_end(&mut v)
                            .map_err(|e| e.to_string())?;
                        OsString::from_vec(v)
                            .map(StdinString::from)
                            .map_err(|e| e.to_string())
                    })
                    .into()
            }),
        ]
    } else if *v == Vec::<u8>::value_type() {
        vec![
            vec_to_os_string_impl::<CommandString>(),
            vec_to_os_string_impl::<StdinString>(),
        ]
    } else {
        vec![]
    }
}

#[derive(Clone, Debug)]
pub enum SomeValue<T> {
    Immediate(T),
    Delayed(TypedValue<T>),
}

impl<T> From<T> for SomeValue<T> {
    fn from(v: T) -> Self {
        Self::Immediate(v)
    }
}

impl<T> From<TypedValue<T>> for SomeValue<T> {
    fn from(v: TypedValue<T>) -> Self {
        Self::Delayed(v)
    }
}

impl<T: Clone> From<&'_ TypedValue<T>> for SomeValue<T> {
    fn from(v: &TypedValue<T>) -> Self {
        Self::Delayed(v.clone())
    }
}

impl<T> SomeValue<T> {
    pub fn dependency<'a>(&'a self) -> grease::Dependency
    where
        &'a T: Into<grease::Dependency>,
    {
        match self {
            SomeValue::Immediate(v) => v.into(),
            SomeValue::Delayed(v) => v.into(),
        }
    }

    pub fn from<U: Into<T>>(v: U) -> Self {
        Self::Immediate(v.into())
    }
}

#[derive(Clone, Debug)]
pub enum Argument {
    Arg(SomeValue<CommandString>),
    ProducedPath { id: usize },
}

impl From<TypedValue<CommandString>> for Argument {
    fn from(v: TypedValue<CommandString>) -> Self {
        Argument::Arg(SomeValue::Delayed(v))
    }
}

impl From<CommandString> for Argument {
    fn from(v: CommandString) -> Self {
        Argument::Arg(SomeValue::Immediate(v))
    }
}

impl From<String> for Argument {
    fn from(v: String) -> Self {
        Self::from(CommandString::from(v))
    }
}

impl From<&str> for Argument {
    fn from(v: &str) -> Self {
        Self::from(CommandString::from(v))
    }
}

impl From<&OsStr> for Argument {
    fn from(v: &OsStr) -> Self {
        Self::from(CommandString::from(v))
    }
}

impl From<&Argument> for grease::Dependency {
    fn from(a: &Argument) -> Self {
        use Argument::*;
        match a {
            Arg(v) => v.dependency(),
            ProducedPath { .. } => (&1238479745234347u128).into(),
        }
    }
}

impl From<&'_ str> for SomeValue<CommandString> {
    fn from(v: &str) -> Self {
        Self::from(CommandString::from(v))
    }
}

#[derive(Debug)]
pub struct Config {
    command: SomeValue<CommandString>,
    pub arguments: Vec<Argument>,
    pub env: BTreeMap<String, Option<SomeValue<CommandString>>>,
    pub stdin: Option<SomeValue<StdinString>>,
    pub dir: Option<SomeValue<PathBuf>>,
    produced_paths: usize,
}

pub struct ExecResult {
    pub output_paths: Vec<TypedValue<PathBuf>>,
    pub stdout: TypedValue<Vec<u8>>,
    pub stderr: TypedValue<Vec<u8>>,
    pub exit_status: TypedValue<std::process::ExitStatus>,
    pub complete: TypedValue<()>,
}

impl Config {
    pub fn new<T: Into<SomeValue<CommandString>>>(command: T) -> Self {
        Config {
            arguments: Default::default(),
            env: Default::default(),
            stdin: None,
            dir: None,
            command: command.into(),
            produced_paths: 0,
        }
    }

    pub fn push_arg<T: Into<Argument>>(&mut self, v: T) {
        self.arguments.push(v.into())
    }

    pub fn path(&mut self) -> Argument {
        let ret = Argument::ProducedPath {
            id: self.produced_paths,
        };
        self.produced_paths += 1;
        ret
    }

    pub fn push_path(&mut self) {
        let p = self.path();
        self.arguments.push(p);
    }
}

impl Plan for Config {
    type Output = Result<ExecResult, String>;

    fn plan(self, ctx: &mut Context) -> Self::Output {
        // Move and rebind for convenience
        let args = self.arguments;
        let command = self.command;
        let env: Vec<_> = self.env.into_iter().collect();

        // Create values from the context for use later
        let command_dep = command.dependency();
        let cmd = match command {
            SomeValue::Immediate(v) => Ok((v.0.to_owned(), ctx.cmd.create(&v.0))),
            SomeValue::Delayed(v) => Err(v),
        };
        let log = ctx.log.sublog("exec");
        let tsk = ctx.task.clone();
        let store = ctx.store.item(item_name!("exec"));
        let mut cmd_untracked = ctx.cmd.untracked();

        // Get Values out of arguments to access later
        let mut arg_values = Vec::new();
        for a in &args {
            match a {
                Argument::Arg(SomeValue::Delayed(v)) => arg_values.push(v.clone()),
                _ => (),
            }
        }
        let input = self.stdin;
        let dir = self.dir;

        // Get Values out of the environment to access later
        let env_values: Vec<_> = env
            .iter()
            .filter_map(|(_, v)| {
                if let Some(SomeValue::Delayed(v)) = v {
                    Some(v.clone())
                } else {
                    None
                }
            })
            .collect();

        let env_deps: Vec<grease::Dependency> = env
            .iter()
            .map(|(k, v)| {
                vec![
                    k.into(),
                    match v {
                        // TODO pull from env here for dependency? if so, env changes from other
                        // commands will not be visible
                        None => (&57023u128).into(),
                        Some(v) => v.dependency(),
                    },
                ]
            })
            .flatten()
            .collect();

        // Create dependencies
        let mut deps = Dependencies::ordered(&args)
            + Dependencies::unordered(depends![join vec![command_dep],&env_deps]);
        if let Some(SomeValue::Delayed(v)) = &input {
            deps = deps + Dependencies::unordered(vec![v]);
        }
        if let Some(SomeValue::Delayed(v)) = &dir {
            deps = deps + Dependencies::unordered(vec![v]);
        }

        // Get value id from dependencies and use it as part of the item output path
        let value_id = deps.value_id::<()>();
        let store = store.item::<&ItemName>(value_id.to_string().as_str().try_into().unwrap());

        // Create map of produced file id to item
        let mut files: BTreeMap<usize, grease::Item> = Default::default();
        for a in &args {
            match a {
                Argument::Arg(_) => (),
                Argument::ProducedPath { id } => {
                    let s = format!("{}", id);
                    let item_name: &ItemName =
                        s.as_str().try_into().map_err(|e: &str| e.to_owned())?;
                    files.entry(*id).or_insert_with(|| store.item(item_name));
                }
            }
        }
        let filesmap = files.clone();

        // Create channels for outputs
        let (send_stdout, rcv_stdout) = channel::oneshot::channel();
        let (send_stderr, rcv_stderr) = channel::oneshot::channel();
        let (send_status, rcv_status) = channel::oneshot::channel();

        let run_command = make_value!([^deps] {
            let name = match &cmd {
                Ok((n,_)) => (*n).clone(),
                Err(v) => format!("cmd{}", v.id()).into()
            };
            let mut work = log.work(name.to_string_lossy());
            tsk.spawn(async move {
                let (name, mut cmd) = match cmd {
                    Ok(v) => v,
                    Err(v) => {
                        let v = v.await?;
                        (v.0.to_owned(),cmd_untracked.create(&v.0))
                    }
                };

                // Force futures
                // TODO combine with prior await
                let (arg_vs,envs) = future::try_join(future::try_join_all(arg_values),future::try_join_all(env_values)).await?;

                // Set arguments
                let mut arg_iter = arg_vs.into_iter();
                for a in args {
                    match a {
                        Argument::Arg(SomeValue::Immediate(s)) => {
                            cmd.arg(s);
                        },
                        Argument::Arg(SomeValue::Delayed(_)) => {
                            let v = arg_iter.next().unwrap();
                            cmd.arg(v.as_ref());
                        },
                        Argument::ProducedPath { id } => {
                            cmd.arg(filesmap.get(&id).unwrap().path());
                        },
                    }
                }

                // Set working directory
                if let Some(dir) = dir {
                    match dir {
                        SomeValue::Immediate(path) => {
                            cmd.current_dir(path);
                        },
                        SomeValue::Delayed(o) => {
                            let path = o.await?;
                            cmd.current_dir(&*path);
                        }
                    }
                }

                // Set stdin
                use std::process::Stdio;
                let input_str = if let Some(input_arg) = input {
                    match input_arg {
                        SomeValue::Immediate(s) => {
                            cmd.stdin(Stdio::piped());
                            Some(s)
                        },
                        SomeValue::Delayed(o) => {
                            let s = o.await?;
                            cmd.stdin(Stdio::piped());
                            Some(s.clone())
                        }
                    }
                } else { None };
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::piped());

                // Set environment
                let mut env_iter = envs.into_iter();
                for (k,v) in env {
                    match v {
                        None => {
                            if let Some(v) = std::env::var_os(&k) {
                                cmd.env(k,v);
                            }
                        }
                        Some(SomeValue::Immediate(s)) => {
                            cmd.env(k,s);
                        },
                        Some(SomeValue::Delayed(_)) => {
                            cmd.env(k,&*env_iter.next().unwrap());
                        },
                        _ => ()
                    }
                }

                log.info(format!("Running: {}", name.to_string_lossy()));
                log.debug(format!("Arguments: {:?}", cmd));

                let output = {
                    let _record = work.start();
                    let mut child = cmd.spawn().map_err(|e| format!("io error: {}", e))?;
                    // Write stdin
                    if let (Some(cin),Some(s)) = (&mut child.stdin,&input_str) {
                        write!(cin, "{}", s.0.to_string_lossy()).map_err(|e| format!("io error: {}", e))?;
                    }
                    //TODO make stdout/stderr streamed out instead of collected at the end
                    child.wait_with_output().map_err(|e| format!("io error: {}", e))?
                };
                if send_status.is_canceled() {
                    if !output.status.success() {
                        let rest = if send_stderr.is_canceled() {
                            if let Ok(s) = String::from_utf8(output.stderr) {
                                if s.is_empty() {
                                    "".into()
                                } else {
                                    format!(":\n{}", s)
                                }
                            } else {
                                "".into()
                            }
                        } else {
                            "".into()
                        };
                        return Err(format!("command returned failure exit status{}", rest));
                    }
                } else {
                    drop(send_status.send(output.status));
                }
                drop(send_stdout.send(output.stdout));
                drop(send_stderr.send(output.stderr));
                Ok(())
            }).await
        });

        let output_paths: Vec<_> = files
            .into_iter()
            .map(|(_, item)| {
                make_value!((run_command) {
                    if !item.exists() {
                        run_command.await?;
                    }
                    Ok(item.path())
                })
            })
            .collect();
        let stdout = make_value!((run_command) { run_command.await?; rcv_stdout.await.map_err(|e| format!("{}", e)) });
        let stderr = make_value!((run_command) { run_command.await?; rcv_stderr.await.map_err(|e| format!("{}", e)) });
        let exit_status = make_value!((run_command) { run_command.await?; rcv_status.await.map_err(|e| format!("{}", e)) });

        Ok(ExecResult {
            output_paths,
            stdout,
            stderr,
            exit_status,
            complete: run_command,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn run_command() -> Result<(), String> {
        let mut cfg = Config::new("echo");
        cfg.push_arg("hello");

        let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;
        let status = ctx.plan(cfg)?.exit_status.get()?;
        assert!(status.success());
        Ok(())
    }

    #[test]
    fn fail_command() -> Result<(), String> {
        let cfg = Config::new("false");

        let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;
        let status = ctx.plan(cfg)?.exit_status.get()?;
        assert!(!status.success());
        Ok(())
    }

    #[test]
    fn output_as_argument() -> Result<(), String> {
        let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

        let mut cfg = Config::new("echo");
        cfg.push_arg("hello");
        let result = cfg.plan(&mut ctx)?;

        let mut cfg2 = Config::new("echo");
        cfg2.push_arg(result.stdout.map(|v| {
            String::from_utf8(v.clone())
                .map_err(|e| format!("{}", e))
                .map(OsString::from)
                .map(CommandString::from)
        }));
        let result = cfg2.plan(&mut ctx)?;

        assert!(result.exit_status.get()?.success());
        assert!(&*(result.stdout.get()?) == b"hello\n\n");
        Ok(())
    }

    #[test]
    fn output_as_input() -> Result<(), String> {
        let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

        let mut cfg = Config::new("echo");
        cfg.push_arg("hello");
        let result = cfg.plan(&mut ctx)?;

        let mut cfg2 = Config::new("cat");
        cfg2.stdin = Some(
            result
                .stdout
                .map(|v| {
                    String::from_utf8(v.clone())
                        .map_err(|e| format!("{}", e))
                        .map(OsString::from)
                        .map(StdinString::from)
                        .into()
                })
                .into(),
        );
        let result = cfg2.plan(&mut ctx)?;

        assert!(result.exit_status.get()?.success());
        assert!(&*(result.stdout.get()?) == b"hello\n");
        Ok(())
    }

    #[test]
    fn env() -> Result<(), String> {
        let mut ctx = Context::builder().build().map_err(|e| e.to_string())?;

        let mut cfg = Config::new("env");
        cfg.env
            .insert("VALUE".into(), Some(SomeValue::from(OsString::from("42"))));
        let result = ctx.plan(cfg)?;

        assert!(&*(result.stdout.get()?) == b"VALUE=42\n");
        Ok(())
    }

    #[test]
    fn env_inherit() -> Result<(), String> {
        let mut ctx = Context::builder().build().map_err(|e| e.to_string())?;

        let mut cfg = Config::new("env");
        cfg.env.insert("CARGO_PKG_NAME".into(), None);
        let result = ctx.plan(cfg)?;

        assert!(&*(result.stdout.get()?) == b"CARGO_PKG_NAME=exec\n");
        Ok(())
    }
}
