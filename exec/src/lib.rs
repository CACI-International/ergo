use grease::{channel, future, item_name, make_value, Context, ItemName, Plan, TypedValue};
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

    fn plan(&self, ctx: &mut Context) -> Self::Output {
        if !self.env.is_empty() {
            unimplemented!();
        }

        let mut cmd = ctx.cmd.create(&self.command);
        let args = self.arguments.clone();

        let log = ctx.log.sublog("exec");
        let name = self.command.clone();
        let tsk = ctx.task.clone();
        let store = ctx.store.item(item_name!("exec"));

        let mut files: BTreeMap<usize, grease::Item> = Default::default();
        let mut str_values = Vec::new();
        let mut file_values = Vec::new();
        for a in &args {
            match a {
                Argument::OutputString(v) => str_values.push(v.clone()),
                Argument::OutputFile(v) => file_values.push(v.clone()),
                Argument::ProducedFile { id } => {
                    let s = format!("{}", id);
                    let item_name: &ItemName =
                        s.as_str().try_into().map_err(|e: &str| e.to_owned())?;
                    files.entry(*id).or_insert_with(|| store.item(item_name));
                }
                _ => (),
            }
        }
        let files_copy = files.clone();
        let input = self.stdin.clone();

        let (send_stdout, rcv_stdout) = channel::oneshot::channel();
        let (send_stderr, rcv_stderr) = channel::oneshot::channel();
        let (send_status, rcv_status) = channel::oneshot::channel();

        let run_command = make_value!([^str_values,^file_values,name] {
            let mut work = log.work(&name);
            tsk.spawn(async move {
                let (strs,files) = future::try_join(future::try_join_all(str_values),future::try_join_all(file_values)).await?;
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
                            cmd.arg(files_copy.get(&id).unwrap().path());
                        }
                    }
                }
                log.info(format!("Running '{}'", &name));
                log.debug(format!("Arguments: {:?}", cmd));

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
                drop(send_stdout.send(output.stdout));
                drop(send_stderr.send(output.stderr));
                if send_status.is_canceled() {
                    if !output.status.success() {
                        return Err("command returned failure exit status".to_owned());
                    }
                } else {
                    drop(send_status.send(output.status));
                }
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
        let status = cfg.plan(&mut ctx)?.exit_status.get()?;
        assert!(status.success());
        Ok(())
    }

    #[test]
    fn fail_command() -> Result<(), String> {
        let cfg = Config::new("false");

        let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;
        let status = cfg.plan(&mut ctx)?.exit_status.get()?;
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
