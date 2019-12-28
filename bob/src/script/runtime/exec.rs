//! Wrapper around exec calls.

use super::{Data, DataFunction, FunctionContext, FunctionError};
use exec::{CommandString, Config, StdinString};
use grease::{make_value, Plan};
use log::trace;
use std::collections::HashMap;

pub fn exec_builtin() -> Data {
    Data::Function(DataFunction::BuiltinFunction(Box::new(exec)).into())
}

fn exec(ctx: &mut grease::Context<FunctionContext>) -> Result<Data, FunctionError> {
    // FunctionContext is only used once, so swap out args.
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut arg_iter = args.into_iter();

    let cmd = arg_iter
        .next()
        .ok_or(FunctionError::from("no command provided"))?;
    let cmd: exec::SomeValue<CommandString> = match cmd {
        Data::String(s) => CommandString::from(s).into(),
        Data::Value(v) => {
            let t: grease::IntoTyped<CommandString> = ctx
                .traits
                .get(&v)
                .ok_or("cannot convert value into command string".to_owned())?;
            t.into_typed(v).into()
        }
        _ => return Err("first exec argument must be convertible to a CommandString".into()),
    };

    let mut cfg = Config::new(cmd);
    let mut output_path_bindings = Vec::new();
    let mut creates = Vec::new();
    to_cfg(
        arg_iter,
        ctx,
        &mut cfg,
        &mut output_path_bindings,
        &mut creates,
    )?;

    trace!("exec plan configuration: {:?}", &cfg);
    let result = cfg.plan_split(ctx)?;

    let mut ret_map = HashMap::new();
    let complete = result.complete.clone();
    ret_map.insert("stdout".into(), Data::Value(result.stdout.into()));
    ret_map.insert("stderr".into(), Data::Value(result.stderr.into()));
    ret_map.insert("exit_status".into(), Data::Value(result.exit_status.into()));
    ret_map.insert("*".into(), Data::Value(result.complete.into()));
    for ((binding, is_dir), path) in output_path_bindings.into_iter().zip(result.output_paths) {
        ctx.inner.env_insert(
            binding,
            if is_dir {
                make_dir_function(path)
            } else {
                Data::Value(path.into())
            },
        );
    }
    for (binding, value) in creates {
        ctx.inner.env_insert(
            binding,
            Data::Value(
                make_value!((complete) [value] {
                    let path = value.await?;
                    if !path.exists() {
                        complete.await?;
                    }
                    Ok(path.clone())
                })
                .into(),
            ),
        );
    }

    Ok(Data::Map(ret_map))
}

fn make_dir_function(v: grease::TypedValue<std::path::PathBuf>) -> Data {
    Data::Function(
        DataFunction::BuiltinFunction(Box::new(move |ctx| {
            let mut args = Vec::new();
            std::mem::swap(&mut args, &mut ctx.inner.args);

            let mut v2 = v.clone();
            for a in args {
                match a {
                    Data::String(s) if s != "." => {
                        v2 = v2.map(move |path| {
                            let mut p = path.clone();
                            p.push(s);
                            Ok(p)
                        });
                    }
                    Data::String(_) | Data::Unit => return Ok(Data::Value(v2.into())),
                    _ => return Err("only strings and terminating unit values may be passed to directory functions".into()),
                }
            }
            Ok(make_dir_function(v2))
        }))
        .into(),
    )
}

fn to_cfg<I: Iterator<Item = Data>>(
    data: I,
    ctx: &mut grease::Context<FunctionContext>,
    cfg: &mut Config,
    output_path_bindings: &mut Vec<(String, bool)>,
    creates: &mut Vec<(String, grease::TypedValue<std::path::PathBuf>)>,
) -> Result<(), String> {
    for a in data {
        match a {
            Data::Unit => (),
            Data::String(s) => cfg.push_arg(s),
            Data::Value(v) => {
                let t: grease::IntoTyped<CommandString> = ctx
                    .traits
                    .get(&v)
                    .ok_or("cannot convert value into command string".to_owned())?;
                cfg.push_arg(t.into_typed(v));
            }
            Data::Array(arr) => to_cfg(arr.into_iter(), ctx, cfg, output_path_bindings, creates)?,
            Data::Map(m) => {
                for (k, v) in m.into_iter() {
                    if k == "file" || k == "dir" {
                        match v {
                            Data::String(s) => {
                                output_path_bindings.push((s, k == "dir"));
                                cfg.push_path();
                            }
                            _ => {
                                return Err("file/dir must be a string to use for the binding name"
                                    .to_owned())
                            }
                        }
                    } else if k == "env" {
                        match v {
                            Data::Map(env) => {
                                for (k, v) in env.into_iter() {
                                    match v {
                                        Data::Unit => {
                                            cfg.env.insert(k, None);
                                        }
                                        Data::String(s) => {
                                            cfg.env.insert(k, Some(CommandString::from(s).into()));
                                        }
                                        Data::Value(v) => {
                                            let t: grease::IntoTyped<CommandString> =
                                                ctx.traits.get(&v).ok_or(
                                                    "cannot convert env value into command string"
                                                        .to_owned(),
                                                )?;
                                            cfg.env.insert(k, Some(t.into_typed(v).into()));
                                        }
                                        _ => {
                                            return Err("env values must be strings or unit-values"
                                                .to_owned())
                                        }
                                    }
                                }
                            }
                            _ => return Err("env must be a map".to_owned()),
                        }
                    } else if k == "pwd" {
                        cfg.dir = Some(match v {
                            Data::Value(v) => {
                                let t: grease::IntoTyped<std::path::PathBuf> =
                                    ctx.traits
                                        .get(&v)
                                        .ok_or("cannot convert value into path".to_owned())?;
                                t.into_typed(v).into()
                            }
                            _ => return Err("pwd values must be paths".to_owned()),
                        });
                    } else if k == "creates" {
                        match v {
                            Data::Map(bindings) => {
                                for (k, v) in bindings.into_iter() {
                                    match v {
                                        Data::Value(v) => {
                                            let t: grease::IntoTyped<std::path::PathBuf> =
                                                ctx.traits.get(&v).ok_or(
                                                    "cannot convert creates value into path"
                                                        .to_owned(),
                                                )?;
                                            creates.push((k, t.into_typed(v)));
                                        }
                                        _ => return Err("creates bindings must be values".into()),
                                    }
                                }
                            }
                            _ => return Err("creates must be a map".to_owned()),
                        }
                    } else if k == "stdin" {
                        cfg.stdin = Some(match v {
                            Data::String(s) => StdinString::from(s).into(),
                            Data::Value(v) => {
                                let t: grease::IntoTyped<StdinString> = ctx
                                    .traits
                                    .get(&v)
                                    .ok_or("cannot convert value into stdin string".to_owned())?;
                                t.into_typed(v).into()
                            }
                            _ => return Err("stdin values must be strings".into()),
                        });
                    } else {
                        return Err(format!("unrecognized map directive: {}", k));
                    }
                }
            }
            Data::Function(_) => return Err("cannot pass function to exec".to_owned()),
        }
    }

    Ok(())
}
