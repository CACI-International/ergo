use grease::{
    channel, future, item_name, make_value, type_uuid, Context, ItemName, Plan, Value, ValueType,
};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryInto;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub enum Argument {
    String(String),
    OutputString(Value),
    File(PathBuf),
    OutputFile(Value),
    ProducedFile { id: usize },
}

#[derive(Debug)]
pub struct Config {
    command: String,
    pub arguments: Vec<Argument>,
    pub env: HashMap<String, Option<Argument>>,
    pub stdin: Option<Argument>,
    pub stdout: bool,
    pub stderr: bool,
    produced_files: usize,
}

pub struct ExecResult {
    pub output_files: Vec<Value>,
    pub stdout: Value,
    pub stderr: Value,
    pub exit_status: Value,
}

impl Config {
    pub fn new<T: Into<String>>(command: T) -> Self {
        Config {
            arguments: Default::default(),
            env: Default::default(),
            stdin: None,
            stdout: false,
            stderr: false,
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
        if !self.env.is_empty() || self.stdout || self.stderr || self.stdin.is_some() {
            unimplemented!();
        }

        let mut cmd = ctx.cmd.create(&self.command);
        let args = self.arguments.clone();

        let log = ctx.log.sublog("exec");
        let name = self.command.clone();
        let tsk = ctx.task.clone();
        let store = ctx.store.item(item_name!("exec"));

        let mut files: BTreeMap<usize,grease::Item> = Default::default();
        let mut arg_values = Vec::new();
        for a in &args {
            match a {
                Argument::OutputString(v) => arg_values.push(v.clone()),
                Argument::OutputFile(v) => arg_values.push(v.clone()),
                Argument::ProducedFile { id } => {
                    let s = format!("{}", id);
                    let item_name : &ItemName = s.as_str().try_into().map_err(|e : &str| e.to_owned())?;
                    files
                        .entry(*id)
                        .or_insert_with(|| store.item(item_name));
                }
                _ => (),
            }
        }
        let files_copy = files.clone();

        let (send_stdout, rcv_stdout) = channel::oneshot::channel();
        let (send_stderr, rcv_stderr) = channel::oneshot::channel();

        let exit_status = make_value!(ValueType::new(type_uuid(b"process")), [^arg_values,name] {
            let mut work = log.work(&name);
            tsk.spawn(async move {
                let vs = future::try_join_all(arg_values).await?;
                let mut i = 0;
                for a in args {
                    match a {
                        Argument::String(s) => {
                            cmd.arg(s);
                        }
                        Argument::OutputString(_) => {
                            let s = vs.get(i).unwrap();
                            cmd.arg(String::from_utf8_lossy(s).as_ref());
                            i += 1;
                        }
                        Argument::File(p) => {
                            cmd.arg(p);
                        }
                        Argument::OutputFile(_) => {
                            let s = vs.get(i).unwrap();
                            cmd.arg(String::from_utf8_lossy(s).as_ref());
                            i += 1;
                        }
                        Argument::ProducedFile { id } => {
                            cmd.arg(files_copy.get(&id).unwrap().path());
                        }
                    }
                }
                log.info(format!("Running '{}'", &name));
                log.debug(format!("Arguments: {:?}", cmd));

                use std::process::Stdio;
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::piped());

                let output = {
                    let _record = work.start();
                    let mut child = cmd.spawn().map_err(|e| format!("io error: {}", e))?;
                    //TODO make stdout/stderr streamed out instead of collected at the end
                    child.wait_with_output().map_err(|e| format!("io error: {}", e))?
                };
                drop(send_stdout.send(output.stdout));
                drop(send_stderr.send(output.stderr));
                Ok(vec![if output.status.success() { 1 } else { 0 }])
            }).await
        });

        let stdout = make_value!(ValueType::new(type_uuid(b"bytes")), rcv_stdout.await.map_err(|e| format!("{}", e)));
        let stderr = make_value!(ValueType::new(type_uuid(b"bytes")), rcv_stderr.await.map_err(|e| format!("{}", e)));
        let output_files: Vec<_> = files.into_iter().map(|(_,item)| make_value!(ValueType::new(type_uuid(b"file")), {
            Ok(Vec::from(item.path().to_string_lossy().as_bytes()))
        })).collect();

        Ok(ExecResult {
            output_files,
            stdout,
            stderr,
            exit_status
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
        assert!(*status == vec![1]);
        Ok(())
    }
}
