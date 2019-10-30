use grease::{future, Context, Plan, Value, ValueType, type_uuid};
use std::collections::HashMap;
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
    type Output = Value;

    fn plan(&self, ctx: &mut Context) -> Self::Output {
        if !self.env.is_empty() || self.stdout || self.stderr || self.stdin.is_some() {
            unimplemented!();
        }

        let mut cmd = ctx.cmd.create(&self.command);
        let args = self.arguments.clone();

        let mut arg_values = Vec::new();
        for a in &args {
            match a {
                Argument::OutputString(v) => arg_values.push(v.clone()),
                Argument::OutputFile(v) => arg_values.push(v.clone()),
                _ => (),
            }
        }

        let arg_values_c = arg_values.clone();
        let log = ctx.log.sublog("exec");
        let name = self.command.clone();
        let tsk = ctx.task.clone();
        Value::new(
            ValueType::new(type_uuid(b"process")),
            async move {
                let mut work = log.work(&name);
                tsk.spawn(async move {
                    let vs = future::try_join_all(arg_values_c).await?;
                    let mut i = 0;
                    let mut files = HashMap::new();
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
                                let p = files
                                    .entry(id)
                                    .or_insert_with(|| "output_file".to_owned());
                                cmd.arg(p);
                            }
                        }
                    }
                    log.info(format!("Running '{}'", &name));
                    log.debug(format!("Arguments: {:?}", cmd));
                    {
                        let _record = work.start();
                        cmd.output().map_err(|e| format!("io error: {}", e))?;
                    }
                    Ok(vec![])
                }).await
            },
            arg_values.as_slice(),
        )
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
        let _res = cfg.plan(&mut ctx).get()?;
        Ok(())
    }
}
