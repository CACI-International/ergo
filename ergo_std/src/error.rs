//! Error module.

use ergo_runtime::{ergo_function, traits, types, Arguments, ContextExt};
use futures::future::TryFutureExt;
use grease::{
    depends, make_value,
    value::{Errored, Value},
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "catch" = catch_fn(),
        "throw" = throw_fn()
    }
}

fn throw_fn() -> Value {
    ergo_function!(
        std::error::throw,
        r"Create a value which, when evaluated, will error.

Arguments: <error: String>
If the keyword argument `pattern` is present, the error will be marked as a pattern error.
The returned value will be unit-typed and will immediately error with the given error message.",
        |ctx, args| {
            let message = args.next().ok_or("no message provided")?;
            let is_pattern_error = args.kw("pattern").is_some();

            args.unused_arguments()?;

            let message = ctx.source_value_as::<types::String>(message);
            let message = message.await?.unwrap();

            make_value!([message] {
                let message = message.await?;
                let mut e: grease::Error = message.owned().into_string().into();
                if is_pattern_error {
                    e = ergo_runtime::error::PatternError::wrap(e);
                }
                Result::<types::Unit, _>::Err(e)
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
        |ctx,args| {
        let handler = args.next().ok_or("no handler provided")?;
        let (value_source, value) = args.next().ok_or("no value provided")?.take();

        args.unused_arguments()?;

        let handler = traits::delay_bind(ctx, handler).await?;

        let deps = depends![*handler.value, value];

        Value::dyn_new(
            async move {
                Errored::ignore(value.clone()).await?;
                Ok(value.into_any_value())
            }
            .or_else(move |e: grease::Error| {
                let e = e.to_string();
                async move {
                    Ok(handler
                        .bind(
                            value_source.clone().with(types::Args {
                                args: Arguments::positional(vec![
                                    value_source.with(types::String::from(e).into())
                                ]).unchecked()
                            }.into())
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

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn throw(t) {
            t.assert_fail("self:error:throw doh");
        }
    }

    ergo_script::test! {
        fn catch(t) {
            t.assert_success("self:error:catch (fn _ -> caught) (self:error:throw doh)");
        }
    }
}
