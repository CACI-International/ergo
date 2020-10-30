//! Error module.

use ergo_runtime::{ergo_function, types, ContextExt, FunctionArguments};
use futures::future::TryFutureExt;
use grease::{depends, make_value, value::Value};

pub fn module() -> Value {
    crate::grease_string_map! {
        "catch" = catch_fn(),
        "throw" = throw_fn()
    }
}

fn throw_fn() -> Value {
    ergo_function!(std::error::throw, |ctx| {
        let message = ctx.args.next().ok_or("no message provided")?;

        ctx.unused_arguments()?;

        let message = ctx.source_value_as::<types::String>(message);
        let message = message.await?.unwrap();

        make_value!([message] {
            let message = message.await?;
            let ret: Result<(), _> = Err(message.owned().into_string().into());
            ret
        })
        .into()
    })
    .into()
}

fn catch_fn() -> Value {
    ergo_function!(std::error::catch, |ctx| {
        let handler = ctx.args.next().ok_or("no handler provided")?;
        let (value_source, value) = ctx.args.next().ok_or("no value provided")?.take();

        ctx.unused_arguments()?;

        let handler = ctx.source_value_as::<types::Function>(handler);
        let handler = handler.await?.unwrap();

        let call_site = ctx.call_site.clone();
        let deps = depends![handler, value];
        let handler = types::Portable::portable(handler, ctx);

        Value::dyn_new(
            async move { value.make_any_value().await }.or_else(move |e| {
                let e = e.to_string();
                async move {
                    handler
                        .await?
                        .call(
                            FunctionArguments::positional(vec![
                                value_source.with(types::String::from(e).into())
                            ]),
                            call_site,
                        )
                        .await?
                        .make_any_value()
                        .await
                }
            }),
            deps,
        )
    })
    .into()
}
