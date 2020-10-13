//! Logging functions.

use ergo_runtime::{namespace_id, source_value_as, traits, types, Runtime};
use futures::future::FutureExt;
use grease::{depends, make_value, runtime::Log, value::Value};

pub fn function(ctx: &mut Runtime) -> Value {
    logger(ctx.log.clone())
}

fn logger(log: Log) -> Value {
    types::Function::new(
        move |ctx| {
            let log = log.clone();
            async move {
                let op = ctx.args.next().ok_or("no command given")?;
                let arg = ctx.args.next().ok_or("no argument given")?;

                let op = source_value_as!(op, types::String, ctx)?
                    .await
                    .transpose_ok()?;

                let arg = traits::into_sourced::<types::String>(ctx, arg)?.unwrap();

                Ok(ctx.call_site.clone().with(match op.as_ref().as_str() {
                    "sublog" => {
                        let arg = arg.await?;
                        logger(log.sublog(arg.as_ref().as_str()))
                    }
                    "debug" => make_value!([namespace_id!(std::log::debug), arg] {
                        let arg = arg.await?;
                        log.debug(arg.as_ref().as_str());
                        Ok(())
                    })
                    .into(),
                    "info" => make_value!([namespace_id!(std::log::info), arg] {
                        let arg = arg.await?;
                        log.info(arg.as_ref().as_str());
                        Ok(())
                    })
                    .into(),
                    "warn" => make_value!([namespace_id!(std::log::warn), arg] {
                        let arg = arg.await?;
                        log.warn(arg.as_ref().as_str());
                        Ok(())
                    })
                    .into(),
                    "error" => make_value!([namespace_id!(std::log::error), arg] {
                        let arg = arg.await?;
                        log.error(arg.as_ref().as_str());
                        Ok(())
                    })
                    .into(),
                    _ => return Err(op.source().with("unrecognized command").into_grease_error()),
                }))
            }
            .boxed()
        },
        depends![namespace_id!(std::log)],
    )
    .into()
}
