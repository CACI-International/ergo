//! Logging functions.

use ergo_runtime::{source_value_as, traits, types, Runtime};
use futures::future::FutureExt;
use grease::{make_value, runtime::Log, value::Value};

pub fn function(ctx: &mut Runtime) -> Value {
    logger(ctx.log.clone())
}

fn logger(log: Log) -> Value {
    types::Function::new(move |ctx| {
        let log = log.clone();
        async move {
            let op = ctx.args.next().ok_or("no command given")?;
            let arg = ctx.args.next().ok_or("no argument given")?;

            let op = source_value_as!(op, types::String, ctx)?
                .await
                .transpose_ok()?;

            let arg = ctx
                .traits
                .get::<traits::IntoTyped<types::String>>(&arg)
                .ok_or(
                    arg.source()
                        .with("cannot convert value into string")
                        .into_grease_error(),
                )?
                .into_typed(arg.unwrap());

            Ok(ctx.call_site.clone().with(match op.as_ref().as_str() {
                "sublog" => {
                    let arg = arg.await?;
                    logger(log.sublog(arg.as_ref().as_str()))
                }
                "debug" => make_value!(["log debug", arg] {
                    let arg = arg.await?;
                    log.debug(arg.as_ref().as_str());
                    Ok(())
                })
                .into(),
                "info" => make_value!(["log info", arg] {
                    let arg = arg.await?;
                    log.info(arg.as_ref().as_str());
                    Ok(())
                })
                .into(),
                "warn" => make_value!(["log warn", arg] {
                    let arg = arg.await?;
                    log.warn(arg.as_ref().as_str());
                    Ok(())
                })
                .into(),
                "error" => make_value!(["log error", arg] {
                    let arg = arg.await?;
                    log.error(arg.as_ref().as_str());
                    Ok(())
                })
                .into(),
                _ => return Err(op.source().with("unrecognized command").into_grease_error()),
            }))
        }
        .boxed()
    })
    .into()
}
