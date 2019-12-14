use grease::{
    channel, depends, future, item_name, make_value, Context, Dependencies, ItemName, Plan,
    TypedValue,
};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub enum Argument {
    String(String),
    OutputString(TypedValue<String>),
    File(PathBuf),
    OutputFile(TypedValue<PathBuf>),
    ProducedFile { id: usize },
}

impl Argument {
    pub fn as_value(&self) -> Option<grease::Value> {
        match self {
            Self::OutputString(s) => Some(s.clone().into()),
            Self::OutputFile(s) => Some(s.clone().into()),
            _ => None,
        }
    }
}

impl From<&Argument> for grease::Dependency {
    fn from(a: &Argument) -> Self {
        use Argument::*;
        match a {
            String(s) => s.into(),
            OutputString(v) => v.into(),
            File(p) => p.into(),
            OutputFile(v) => v.into(),
            ProducedFile { .. } => (&1238479745234347u128).into(),
        }
    }
}

#[derive(Debug)]
pub struct Config {
    command: String,
    pub arguments: Vec<Argument>,
    pub env: HashMap<String, Option<Argument>>,
    pub stdin: Option<Argument>,
    produced_files: usize,
}

pub struct ExecResult {
    pub output_files: Vec<TypedValue<PathBuf>>,
    pub stdout: TypedValue<Vec<u8>>,
    pub stderr: TypedValue<Vec<u8>>,
    pub exit_status: TypedValue<std::process::ExitStatus>,
}

impl Config {
    pub fn new<T: Into<String>>(command: T) -> Self {
        Config {
            arguments: Default::default(),
            env: Default::default(),
            stdin: None,
            command: command.into(),
            produced_files: 0,
        }
    }

    pub fn file(&mut self) -> Argument {
        let ret = Argument::ProducedFile {
            id: self.produced_files,
        };
        self.produced_files += 1;
        ret
    }
}

impl Plan for Config {
    type Output = Result<ExecResult, String>;

    fn plan(self, ctx: &mut Context) -> Self::Output {
        // Move and rebind for convenience
        let args = self.arguments;
        let name = self.command;
        let env: Vec<_> = self.env.into_iter().collect();

        // Create values from the context for use later
        let mut cmd = ctx.cmd.create(&name);
        let log = ctx.log.sublog("exec");
        let tsk = ctx.task.clone();
        let store = ctx.store.item(item_name!("exec"));

        // Get Values out of arguments to access later
        let mut str_values = Vec::new();
        let mut file_values = Vec::new();
        for a in &args {
            match a {
                Argument::OutputString(v) => str_values.push(v.clone()),
                Argument::OutputFile(v) => file_values.push(v.clone()),
                _ => (),
            }
        }
        let input = self.stdin.clone();

        // Get Values out of the environment to access later
        let env_values: Vec<_> = env
            .iter()
            .filter_map(|(_, v)| {
                if let Some(Argument::OutputString(v)) = v {
                    Some(v.clone())
                } else {
                    None
                }
            })
            .collect();

        // Create dependencies
        let mut deps = Dependencies::ordered(depends![join & args])
            + Dependencies::unordered(depends![join depends![name],&env_values]);
        if let Some(v) = &input {
            if let Some(v) = v.as_value() {
                deps = deps + Dependencies::unordered(depends![v]);
            }
        }

        // Get value id from dependencies and use it as part of the item output path
        let value_id = deps.value_id::<()>();
        let store = store.item::<&ItemName>(value_id.to_string().as_str().try_into().unwrap());

        // Create map of produced file id to item
        let mut files: BTreeMap<usize, grease::Item> = Default::default();
        for a in &args {
            match a {
                Argument::ProducedFile { id } => {
                    let s = format!("{}", id);
                    let item_name: &ItemName =
                        s.as_str().try_into().map_err(|e: &str| e.to_owned())?;
                    files.entry(*id).or_insert_with(|| store.item(item_name));
                }
                _ => (),
            }
        }
        let filesmap = files.clone();

        // Create channels for outputs
        let (send_stdout, rcv_stdout) = channel::oneshot::channel();
        let (send_stderr, rcv_stderr) = channel::oneshot::channel();
        let (send_status, rcv_status) = channel::oneshot::channel();

        let run_command = make_value!([^deps] {
            let mut work = log.work(&name);
            tsk.spawn(async move {
                // Force futures
                let (strs,files,envs) = future::try_join3(future::try_join_all(str_values),future::try_join_all(file_values),future::try_join_all(env_values)).await?;

                // Set arguments
                let mut str_iter = strs.iter();
                let mut file_iter = files.iter();
                for a in args {
                    match a {
                        Argument::String(s) => {
                            cmd.arg(s);
                        }
                        Argument::OutputString(_) => {
                            let s = str_iter.next().unwrap();
                            let p: &str = s.as_ref();
                            cmd.arg(p);
                        }
                        Argument::File(p) => {
                            cmd.arg(p);
                        }
                        Argument::OutputFile(_) => {
                            let s = file_iter.next().unwrap();
                            let p: &std::path::Path = s.as_ref();
                            cmd.arg(p);
                        }
                        Argument::ProducedFile { id } => {
                            cmd.arg(filesmap.get(&id).unwrap().path());
                        }
                    }
                }

                // Set stdin
                use std::process::Stdio;
                let input_str = if let Some(input_arg) = input {
                    match input_arg {
                        Argument::String(s) => {
                            cmd.stdin(Stdio::piped());
                            Some(s)
                        },
                        Argument::OutputString(o) => {
                            let s = o.await?;
                            cmd.stdin(Stdio::piped());
                            Some(s.clone())
                        },
                        Argument::File(p) => {
                            cmd.stdin(Stdio::from(File::open(p).map_err(|e| format!("failed to open file: {}", e))?));
                            None
                        },
                        Argument::OutputFile(o) => {
                            let p = o.await?;
                            cmd.stdin(Stdio::from(File::open(p.clone()).map_err(|e| format!("failed to open file: {}", e))?));
                            None
                        }
                        Argument::ProducedFile {id} => {
                            return Err("cannot use an output file as process input".to_owned());
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
                        Some(Argument::String(s)) => {
                            cmd.env(k,s);
                        },
                        Some(Argument::OutputString(_)) => {
                            cmd.env(k,&*env_iter.next().unwrap());
                        },
                        _ => ()
                    }
                }

                log.info(format!("Running '{}'", &name));
                log.debug(format!("Arguments: {:?}", cmd));

                let output = {
                    let _record = work.start();
                    let mut child = cmd.spawn().map_err(|e| format!("io error: {}", e))?;
                    // Write stdin if we couldn't use it directly as a file
                    if let (Some(cin),Some(s)) = (&mut child.stdin,&input_str) {
                        write!(cin, "{}", s).map_err(|e| format!("io error: {}", e))?;
                    }
                    //TODO make stdout/stderr streamed out instead of collected at the end
                    child.wait_with_output().map_err(|e| format!("io error: {}", e))?
                };
                if send_status.is_canceled() {
                    if !output.status.success() {
                        let rest = if send_stderr.is_canceled() {
                            if let Ok(s) = String::from_utf8(output.stderr) {
                                format!(":\n{}", s)
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

        let output_files: Vec<_> = files
            .into_iter()
            .map(|(_, item)| make_value!((run_command) { run_command.await?; Ok(item.path()) }))
            .collect();
        let stdout = make_value!((run_command) { run_command.await?; rcv_stdout.await.map_err(|e| format!("{}", e)) });
        let stderr = make_value!((run_command) { run_command.await?; rcv_stderr.await.map_err(|e| format!("{}", e)) });
        let exit_status = make_value!((run_command) { run_command.await?; rcv_status.await.map_err(|e| format!("{}", e)) });

        Ok(ExecResult {
            output_files,
            stdout,
            stderr,
            exit_status,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn run_command() -> Result<(), String> {
        let mut cfg = Config::new("echo");
        cfg.arguments.push(Argument::String("hello".to_owned()));

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
        cfg.arguments.push(Argument::String("hello".to_owned()));
        let result = cfg.plan(&mut ctx)?;

        let mut cfg2 = Config::new("echo");
        cfg2.arguments.push(Argument::OutputString(
            result
                .stdout
                .map(|v| String::from_utf8(v.clone()).map_err(|e| format!("{}", e))),
        ));
        let result = cfg2.plan(&mut ctx)?;

        assert!(result.exit_status.get()?.success());
        assert!(&*(result.stdout.get()?) == b"hello\n\n");
        Ok(())
    }

    #[test]
    fn output_as_input() -> Result<(), String> {
        let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

        let mut cfg = Config::new("echo");
        cfg.arguments.push(Argument::String("hello".to_owned()));
        let result = cfg.plan(&mut ctx)?;

        let mut cfg2 = Config::new("cat");
        cfg2.stdin =
            Some(Argument::OutputString(result.stdout.map(|v| {
                String::from_utf8(v.clone()).map_err(|e| format!("{}", e))
            })));
        let result = cfg2.plan(&mut ctx)?;

        assert!(result.exit_status.get()?.success());
        assert!(&*(result.stdout.get()?) == b"hello\n");
        Ok(())
    }
}
