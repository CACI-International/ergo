//! Logging functions.

use ergo_runtime::{ergo_function, namespace_id, types, ContextExt, Runtime};
use grease::{make_value, runtime::Log, value::Value};

pub fn function(ctx: &mut Runtime) -> Value {
    logger(ctx.log.clone())
}

fn logger(log: Log) -> Value {
    ergo_function!(independent std::log,
"Send strings to the runtime log output.

The first argument is a command, one of: `sublog`, `debug`, `info`, `warn`, or `error`.

## `sublog`
The next argument gives the name of the sublogger to create, and the function returns a new log function.

## `debug`, `info`, `warn`, `error`
The next argument gives the string to log to the appropriate output verbosity level.

Note that like other functions, these logging functions don't occur immediately, but rather return a
unit-typed value that, when evaluated, will log to the output.",
    log |ctx| {
        let op = ctx.args.next().ok_or("no command given")?;
        let arg = ctx.args.next().ok_or("no argument given")?;

        let op = ctx.source_value_as::<types::String>(op);
        let op = op.await?.await.transpose_ok()?;

        let arg = ctx.into_sourced::<types::String>(arg);
        let arg = arg.await?.unwrap();

        match op.as_ref().as_str() {
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
        }
    }).into()
}
