//! Wrapper around exec calls.

use super::builtin_function_prelude::*;
use super::{Error, Source};
use exec::{CommandString, Config, StdinString};
use grease::{make_value, match_value, Plan};
use log::trace;
use std::collections::BTreeMap;

def_builtin!(ctx => {
    // FunctionContext is only used once, so swap out args.
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut arg_iter = args.into_iter();

    let cmd = arg_iter.next().ok_or("no command provided")?;

    let (cmdsource, cmd) = cmd.take();

    let into_cmd_str: grease::IntoTyped<CommandString> =
        ctx.traits.get(&cmd).ok_or(EvalError::from(
            cmdsource
                .clone()
                .with("cannot convert value into command string"),
        ))?;
    // TODO remove SomeValue, add another way to indicate whether exec should record the command
    let cmd: exec::SomeValue<CommandString> = into_cmd_str.into_typed(cmd).into();

    let mut cfg = Config::new(cmd);
    let mut output_path_bindings = Vec::new();
    let mut creates = Vec::new();
    for a in arg_iter {
        to_cfg(a, ctx, &mut cfg, &mut output_path_bindings, &mut creates)?;
    }

    trace!("exec plan configuration: {:?}", &cfg);
    let result = cfg.plan_split(ctx)?;

    let mut ret_map = BTreeMap::new();
    let complete = result.complete.clone();
    ret_map.insert(
        "stdout".into(),
        cmdsource.clone().with(result.stdout.into()),
    );
    ret_map.insert(
        "stderr".into(),
        cmdsource.clone().with(result.stderr.into()),
    );
    ret_map.insert(
        "exit_status".into(),
        cmdsource.clone().with(result.exit_status.into()),
    );
    ret_map.insert(
        "once".into(),
        cmdsource.clone().with(result.complete_once.into()),
    );
    ret_map.insert("*".into(), cmdsource.with(result.complete.into()));
    for ((binding, is_dir), path) in output_path_bindings.into_iter().zip(result.output_paths) {
        let (source, binding) = binding.take();
        ctx.inner.env_insert(
            binding,
            source.with(if is_dir {
                make_dir_function(path)
            } else {
                path.into()
            }),
        );
    }
    for (binding, value) in creates {
        ctx.inner.env_insert(
            binding,
            value.map(|v| {
                make_value!((complete) [v] {
                    let path = v.await?;
                    if !path.exists() {
                        complete.await?;
                    }
                    Ok(path.clone())
                })
                .into()
            }),
        );
    }

    Ok(ScriptMap(ret_map).into())
});

fn make_dir_function(v: grease::TypedValue<std::path::PathBuf>) -> Value {
    ScriptFunction::BuiltinFunction(Box::new(move |ctx| {
        let mut args = Vec::new();
        std::mem::swap(&mut args, &mut ctx.inner.args);

        let mut v = v.clone();
        for a in args {
            let s = a
                .map(|v| {
                    v.typed::<ScriptString>()
                        .map_err(|_| "only strings may be passed to directory functions".into())
                        .and_then(|v| v.get())
                        .map(|v| v.owned())
                })
                .transpose_err()?;
            if s != "." {
                v = v.map(move |path| {
                    let mut p = path.clone();
                    p.push(s);
                    Ok(p)
                });
            } else {
                return Ok(v.into());
            }
        }
        Ok(make_dir_function(v))
    }))
    .into()
}

fn to_cfg(
    value: Source<Value>,
    ctx: &mut grease::Context<FunctionContext>,
    cfg: &mut Config,
    output_path_bindings: &mut Vec<(Source<String>, bool)>,
    creates: &mut Vec<(String, Source<grease::TypedValue<std::path::PathBuf>>)>,
) -> Result<(), Source<Error>> {
    value.map(|value| match_value!(value => {
        ScriptUnit => |_| Ok(()),
        ScriptArray => |val| {
            let ScriptArray(arr) = val.get()?.owned();
            for v in arr {
                to_cfg(v, ctx, cfg, output_path_bindings, creates)?;
            }
            Ok(())
        },
        ScriptMap => |val| {
            let ScriptMap(m) = val.get()?.owned();
            for (k,v) in m.into_iter() {
                if k == "file" || k == "dir" {
                    let s = v.map(|v| v.typed::<ScriptString>()
                        .map_err(|_| "must be a string to use for the binding name".into())
                        .and_then(|v| v.get())
                        .map(|v| v.owned())).transpose()?;
                    output_path_bindings.push((s, k == "dir"));
                    cfg.push_path();
                } else if k == "env" {
                    let ScriptMap(env) = v.map(|v| v.typed::<ScriptMap>()
                        .map_err(|_| "env must be a map".into())
                        .and_then(|v| v.get())
                        .map(|v| v.owned())).transpose_err()?;
                    for (k, v) in env.into_iter() {
                        v.map(|v| match_value!(v => {
                            () => |_| {
                                cfg.env.insert(k, None);
                                Ok(())
                            },
                            => |v| {
                                let t: grease::IntoTyped<CommandString> =
                                    ctx.traits.get(&v).ok_or("cannot convert env value into command string")?;
                                cfg.env.insert(k, Some(t.into_typed(v).into()));
                                Ok(())
                            }
                        })).transpose_err().map_err(|e: Source<EvalError>| e)?;
                    }
                } else if k == "pwd" {
                    cfg.dir = Some(v.map(|v| {
                            let t: grease::IntoTyped<std::path::PathBuf> =
                                ctx.traits
                                    .get(&v)
                                    .ok_or("cannot convert value into path")?;
                            Ok(t.into_typed(v).into())
                        }).transpose_err().map_err(|e: Source<EvalError>| e)?);
                } else if k == "creates" {
                    let ScriptMap(bindings) = v.map(|v| v.typed::<ScriptMap>()
                        .map_err(|_| "creates must be a map".into())
                        .and_then(|v| v.get())
                        .map(|v| v.owned())).transpose_err()?;
                    for (k, v) in bindings.into_iter() {
                        let v = v.map(|v| {
                            let t: grease::IntoTyped<std::path::PathBuf> =
                                ctx.traits.get(&v).ok_or("cannot convert creates value into path")?;
                            Ok(t.into_typed(v))
                        }).transpose().map_err(|e: Source<EvalError>| e)?;
                        creates.push((k, v));
                    }
                } else if k == "stdin" {
                    let v = v.map(|v| {
                        let t: grease::IntoTyped<StdinString> =
                            ctx.traits
                                .get(&v)
                                .ok_or("cannot convert value into stdin string")?;
                        Ok(t.into_typed(v).into())
                    }).transpose_err().map_err(|e: Source<EvalError>| e)?;
                    cfg.stdin = Some(v);
                } else {
                    return Err(format!("unrecognized map directive: {}", k).into());
                }
            }
            Ok(())
        },
        => |v| {
            let t: grease::IntoTyped<CommandString> = ctx
                .traits
                .get(&v)
                .ok_or("cannot convert value into command string")?;
            cfg.push_arg(t.into_typed(v));
            Ok(())
        }
    })).transpose_err().map_err(|e: Source<EvalError>| e.into())
}
