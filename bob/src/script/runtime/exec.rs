//! Wrapper around exec calls.

use super::{Data, DataFunction, FunctionContext};
use exec::{Argument, Config};
use grease::Plan;
use std::collections::HashMap;

pub fn exec_builtin() -> Data {
    Data::Function(DataFunction::BuiltinFunction(std::rc::Rc::new(exec)))
}

fn exec(ctx: &mut grease::Context<FunctionContext>) -> Result<Data, String> {
    // FunctionContext is only used once, so swap out args.
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut arg_iter = args.into_iter();

    let cmd = arg_iter.next().ok_or("no command provided".to_owned())?;
    let cmd = if let Data::String(s) = cmd {
        s
    } else {
        return Err("first exec argument must be a string".to_owned());
    };

    let mut cfg = Config::new(cmd);
    let mut output_file_bindings = Vec::new();
    to_cfg(arg_iter, &mut cfg, &mut output_file_bindings)?;

    let result = cfg.plan_split(ctx)?;

    let mut ret_map = HashMap::new();
    ret_map.insert("stdout".to_owned(), Data::Value(result.stdout.into()));
    ret_map.insert("stderr".to_owned(), Data::Value(result.stderr.into()));
    ret_map.insert(
        "exit_status".to_owned(),
        Data::Value(result.exit_status.into()),
    );
    for (binding, file) in output_file_bindings.into_iter().zip(result.output_files) {
        ctx.inner
            .current_env()
            .insert(binding, Data::Value(file.into()));
    }

    Ok(Data::Map(ret_map))
}

fn to_cfg<I: Iterator<Item = Data>>(
    data: I,
    cfg: &mut Config,
    output_file_bindings: &mut Vec<String>,
) -> Result<(), String> {
    for a in data {
        match a {
            Data::String(s) => cfg.arguments.push(Argument::String(s)),
            Data::Value(v) => {
                let arg = v
                    .typed::<String>()
                    .map(Argument::OutputString)
                    .or_else(|v| v.typed::<std::path::PathBuf>().map(Argument::OutputFile))
                    .map_err(|_| "only string and path types are supported")?;
                cfg.arguments.push(arg)
            }
            Data::Array(arr) => to_cfg(arr.into_iter(), cfg, output_file_bindings)?,
            Data::Map(mut m) => {
                if m.len() == 1 {
                    if let Some(v) = m.remove("output") {
                        match v {
                            Data::String(s) => {
                                output_file_bindings.push(s);
                                let f = cfg.file();
                                cfg.arguments.push(f);
                            }
                            Data::Value(v) => {
                                let arg = v
                                    .typed::<String>()
                                    .map(Argument::OutputString)
                                    .map_err(|_| "only string types are supported")?;
                                cfg.arguments.push(arg);
                            }
                            _ => {
                                return Err("output must be a string to use for the binding name"
                                    .to_owned())
                            }
                        }
                    } else if let Some(v) = m.remove("env") {
                        match v {
                            Data::Map(env) => {
                                for (k, v) in env.into_iter() {
                                    match v {
                                        Data::String(s) => {
                                            cfg.env.insert(
                                                k,
                                                if s.is_empty() {
                                                    None
                                                } else {
                                                    Some(Argument::String(s))
                                                },
                                            );
                                        }
                                        Data::Value(v) => {
                                            let arg = v
                                                .typed::<String>()
                                                .map(Argument::OutputString)
                                                .map_err(|_| "only string types are supported")?;
                                            cfg.arguments.push(arg);
                                        }
                                        _ => return Err("env values must be strings".to_owned()),
                                    }
                                }
                            }
                            _ => return Err("env must be a map".to_owned()),
                        }
                    } else if let Some(_v) = m.remove("stdin") {
                        unimplemented!();
                    }
                }
            }
            Data::Function(_) => return Err("cannot pass function to exec".to_owned()),
        }
    }

    Ok(())
}
