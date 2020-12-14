//! Error module.

use ergo_runtime::{ergo_function, types, ContextExt, FunctionArguments};
use futures::future::TryFutureExt;
use grease::{
    depends, make_value,
    value::{Errored, Value},
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of error manipulation functions:"
        "catch": "Catch any error as a result of executing a value." = catch_fn(),
        "throw": "Create a value which will error." = throw_fn()
    }
}

fn throw_fn() -> Value {
    ergo_function!(
        std::error::throw,
        r"Create a value which, when evaluated, will error.

Arguments: <error: String>
The returned value will be unit-typed and will immediately error with the given error message.",
        |ctx| {
            let message = ctx.args.next().ok_or("no message provided")?;

            ctx.unused_arguments()?;

            let message = ctx.source_value_as::<types::String>(message);
            let message = message.await?.unwrap();

            make_value!([message] {
                let message = message.await?;
                let ret: Result<types::Unit, _> = Err(message.owned().into_string().into());
                ret
            })
            .into()
        }
    )
    .into()
}

fn catch_fn() -> Value {
    ergo_function!(std::error::catch,
        r"Catch an error result in the given value.

Arguments: <handler: Function> <value>
The returned value is dynamically-typed, and will return the result of `<value>` if it does not error.
If it _does_ error, `handler` is applied to the error string and whatever it returns will be the result.",
        |ctx| {
        let handler = ctx.args.next().ok_or("no handler provided")?;
        let (value_source, value) = ctx.args.next().ok_or("no value provided")?.take();

        ctx.unused_arguments()?;

        let handler = ctx.source_value_as::<types::Function>(handler);
        let handler = handler.await?.unwrap();

        let call_site = ctx.call_site.clone();
        let deps = depends![handler, value];
        let handler = types::Portable::portable(handler, ctx);

        Value::dyn_new(
            async move {
                Errored::ignore(value.clone()).await?;
                Ok(value.into_any_value())
            }
            .or_else(move |e: grease::Error| {
                let e = e.to_string();
                async move {
                    Ok(handler
                        .await?
                        .call(
                            FunctionArguments::positional(vec![
                                value_source.with(types::String::from(e).into())
                            ]),
                            call_site,
                        )
                        .await?
                        .unwrap()
                        .into_any_value())
                }
            }),
            deps,
        )
    })
    .into()
}
