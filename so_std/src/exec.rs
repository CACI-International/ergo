//! Wrapper around exec calls.

use super::builtin_function_prelude::*;
use super::{Error, Source};
use exec::{CommandString, Config, StdinString};
use grease::{match_value, Plan};
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
    let mut all_success = true;
    while let Some(a) = ctx.args.next() {
        all_success &= to_cfg(a, ctx, &mut cfg);
    }

    if let Some(v) = ctx.args.kw("env") {
        eval_error!(ctx, v.map(|v| v.typed::<ScriptMap>()
            .map_err(|_| "env must be a map".into())
            .and_then(|v| v.get())
            .and_then(|v| {
                let ScriptMap(env) = v.owned();
                for (k,v) in env {
                    match_value!(v => {
                        () => |_| {
                            cfg.env.insert(k, None);
                        },
                        => |v| {
                            let t: grease::IntoTyped<CommandString> =
                                ctx.traits.get(&v).ok_or(format!("cannot convert env value with key {} into command string", k))?;
                            cfg.env.insert(k, Some(t.into_typed(v).into()));
                        }
                    });
                }
                Ok(())
            })).transpose_err(), all_success = false);
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
    if let Some(v) = ctx.args.kw("description") {
        let desc = v.unwrap().typed::<ScriptString>().map_err(|_| "description must be a string")?;
        cfg.description = Some(desc.into());
    }

    all_success &= !ctx.unused_arguments();

    if !all_success {
        return Ok(Eval::Error);
    }

    trace!("exec plan configuration: {:?}", &cfg);
    let result = cfg.plan_split(ctx)?;

    let mut ret_map = BTreeMap::new();
    ret_map.insert("stdout".into(), ctx.imbue_error_context(result.stdout.into(), "while evaluating stdout of exec command"));
    ret_map.insert("stderr".into(), ctx.imbue_error_context(result.stderr.into(), "while evaluating stderr of exec command"));
    ret_map.insert("exit_status".into(), ctx.imbue_error_context(result.exit_status.into(), "while evaluating exit_status of exec command"));
    ret_map.insert("complete".into(), ctx.imbue_error_context(result.complete.into(), "while evaluating result of exec command"));

    Ok(Eval::Value(ScriptMap(ret_map).into()))
});

fn to_cfg(
    value: Source<Value>,
    ctx: &mut grease::Context<FunctionContext>,
    cfg: &mut Config,
) -> bool {
    let result = value
        .map(|value| {
            let t: grease::IntoTyped<CommandString> = ctx
                .traits
                .get(&value)
                .ok_or("cannot convert value into command string")?;
            cfg.push_arg(t.into_typed(value));
            Ok(true)
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
