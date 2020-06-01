//! Wrapper around exec calls.

use super::builtin_function_prelude::*;
use super::{Error, Source};
use exec::{CommandString, Config, StdinString};
use grease::{make_value, match_value, Plan};
use log::trace;
use std::collections::BTreeMap;

def_builtin!(ctx => {
    let cmd = ctx.args.next().ok_or("no command provided")?;

    let (cmdsource, cmd) = cmd.take();

    let into_cmd_str: grease::IntoTyped<CommandString> =
        match ctx.traits.get(&cmd) {
            Some(t) => t,
            None => {
                ctx.error(cmdsource.with("cannot convert value into command string"));
                return Ok(Eval::Error);
            }
        };
    // TODO remove SomeValue, add another way to indicate whether exec should record the command
    let cmd: exec::SomeValue<CommandString> = into_cmd_str.into_typed(cmd).into();

    let mut cfg = Config::new(cmd);
    let mut output_path_bindings = Vec::new();
    let mut creates = Vec::new();
    let mut all_success = true;
    while let Some(a) = ctx.args.next() {
        all_success &= to_cfg(a, ctx, &mut cfg, &mut output_path_bindings);
    }

    if let Some(v) = ctx.args.kw("env") {
        match v.map(|v| v.typed::<ScriptMap>()
            .map_err(|_| "env must be a map".into())
            .and_then(|v| v.get())
            .map(|v| v.owned())).transpose_err() {
            Err(e) => {
                ctx.error(e);
                all_success = false;
            }
            Ok(ScriptMap(env)) => {
                for (k, v) in env.into_iter() {
                    eval_error!(ctx, v.map(|v| match_value!(v => {
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
                    })).transpose_err().map_err(|e: Source<Error>| e), all_success = false);
                }
            }
        }
    }
    if let Some(v) = ctx.args.kw("pwd") {
        match v.map(|v| {
                let t: grease::IntoTyped<std::path::PathBuf> =
                    ctx.traits
                        .get(&v)
                        .ok_or("cannot convert value into path")?;
                Ok(t.into_typed(v).into())
            }).transpose_err().map_err(|e: Source<Error>| e) {
            Err(e) => {
                ctx.error(e);
                all_success = false;
            }
            Ok(v) => cfg.dir = Some(v)
        }
    }
    if let Some(v) = ctx.args.kw("creates") {
        match v.map(|v| v.typed::<ScriptMap>()
            .map_err(|_| "creates must be a map".into())
            .and_then(|v| v.get())
            .map(|v| v.owned())).transpose_err() {
            Err(e) => {
                ctx.error(e);
                all_success = false;
            }
            Ok(ScriptMap(bindings)) => {
                for (k, v) in bindings.into_iter() {
                    let v = eval_error!(ctx, v.map(|v| {
                        let t: grease::IntoTyped<std::path::PathBuf> =
                            ctx.traits.get(&v).ok_or("cannot convert creates value into path")?;
                        Ok(t.into_typed(v))
                    }).transpose().map_err(|e: Source<Error>| e), { all_success = false; continue; });
                    creates.push((k, v));
                }
            }
        }
    }
    if let Some(v) = ctx.args.kw("stdin") {
        match v.map(|v| {
            let t: grease::IntoTyped<StdinString> =
                ctx.traits
                    .get(&v)
                    .ok_or("cannot convert value into stdin string")?;
            Ok(t.into_typed(v).into())
        }).transpose_err().map_err(|e: Source<Error>| e) {
            Err(e) => {
                ctx.error(e);
                all_success = false;
            }
            Ok(v) => cfg.stdin = Some(v)
        }
    }

    all_success &= !ctx.unused_arguments();

    if !all_success {
        return Ok(Eval::Error);
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

    let mut paths = BTreeMap::new();
    for ((binding, is_dir), path) in output_path_bindings.into_iter().zip(result.output_paths) {
        let (source, binding) = binding.take();
        paths.insert(binding, source.with(if is_dir { make_dir_function(path) } else { path.into() }));
    }

    for (binding, value) in creates {
        paths.insert(binding,
            value.map(|v| {
                make_value!((complete) [v] {
                    let path = v.await?;
                    if !path.exists() {
                        complete.await?;
                    }
                    Ok(path.clone())
                })
                .into()
            })
        );
    }

    ret_map.insert(
        "paths".into(),
        cmdsource.clone().with(ScriptMap(paths).into())
    );

    ret_map.insert("*".into(), cmdsource.with(result.complete.into()));

    Ok(Eval::Value(ScriptMap(ret_map).into()))
});

fn make_dir_function(v: grease::TypedValue<std::path::PathBuf>) -> Value {
    ScriptFunction::BuiltinFunction(Box::new(move |ctx| {
        let mut v = v.clone();
        let mut any_error = false;
        while let Some(a) = ctx.args.next() {
            let s = eval_error!(
                ctx,
                a.map(|v| {
                    v.typed::<ScriptString>()
                        .map_err(|_| "only strings may be passed to directory functions".into())
                        .and_then(|v| v.get())
                        .map(|v| v.owned())
                })
                .transpose_err(),
                {
                    any_error = true;
                    continue;
                }
            );
            if s != "." {
                v = v.and_then(move |path| {
                    let mut p = path.clone();
                    p.push(s);
                    Ok(p)
                });
            } else {
                return Ok(Eval::Value(v.into()));
            }
        }

        if ctx.unused_arguments() {
            return Ok(Eval::Error);
        }

        if any_error {
            Ok(Eval::Error)
        } else {
            Ok(Eval::Value(make_dir_function(v)))
        }
    }))
    .into()
}

fn to_cfg(
    value: Source<Value>,
    ctx: &mut grease::Context<FunctionContext>,
    cfg: &mut Config,
    output_path_bindings: &mut Vec<(Source<String>, bool)>,
) -> bool {
    let result = value
        .map(|value| {
            match_value!(value => {
                ScriptMap => |val| {
                    let ScriptMap(m) = val.get()?.owned();
                    let mut any_error = false;
                    for (k,v) in m.into_iter() {
                        if k == "file" || k == "dir" {
                            let s = eval_error!(ctx, v.map(|v| v.typed::<ScriptString>()
                                .map_err(|_| "must be a string to use for the binding name".into())
                                .and_then(|v| v.get())
                                .map(|v| v.owned())).transpose(), { any_error = true; continue; });
                            output_path_bindings.push((s, k == "dir"));
                            cfg.push_path();
                        } else {
                            return Err(format!("unrecognized map directive: {}", k).into());
                        }
                    }
                    Ok(!any_error)
                },
                => |v| {
                    let t: grease::IntoTyped<CommandString> = ctx
                        .traits
                        .get(&v)
                        .ok_or("cannot convert value into command string")?;
                    cfg.push_arg(t.into_typed(v));
                    Ok(true)
                }
            })
        })
        .transpose_err()
        .map_err(|e: Source<Error>| e);
    match result {
        Ok(v) => v,
        Err(e) => {
            ctx.error(e);
            false
        }
    }
}
