//! Error module.

use ergo_runtime::{source_value_as, types, FunctionArguments, FunctionCall};
use futures::future::{FutureExt, TryFutureExt};
use grease::{bst::BstMap, depends, make_value, value::Value};

pub fn module() -> Value {
    let mut map = BstMap::new();
    map.insert("catch".into(), catch_fn());
    map.insert("throw".into(), throw_fn());
    types::Map(map).into()
}

fn throw_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let message = ctx.args.next().ok_or("no message provided")?;

            let message = source_value_as!(message, types::String, ctx)?.unwrap();

            Ok(ctx.call_site.clone().with(
                make_value!(["error throw", message] {
                    let message = message.await?;
                    let ret: Result<(), _> = Err(message.owned().into_string().into());
                    ret
                })
                .into(),
            ))
        }
        .boxed()
    })
    .into()
}

fn catch_fn() -> Value {
    types::Function::new(|ctx| {
        async move {
            let handler = ctx.args.next().ok_or("no handler provided")?;
            let (value_source, value) = ctx.args.next().ok_or("no value provided")?.take();

            let handler = source_value_as!(handler, types::Function, ctx)?.unwrap();

            let error_string = make_value!((value) ["error catch message"] {
                match value.await {
                    Err(e) => Ok(types::String::from(e.to_string())),
                    Ok(_) => Err("expected value to error".into())
                }
            })
            .into();

            let call_site = ctx.call_site.clone();
            let error_value = handler
                .await?
                .call(&mut FunctionCall::new(
                    ctx,
                    FunctionArguments::positional(vec![value_source.with(error_string)]),
                    call_site,
                ))
                .await?
                .unwrap();

            if value.grease_type() != error_value.grease_type() {
                return Err(ctx
                    .call_site
                    .clone()
                    .with("handler return type doesn't match value type")
                    .into_grease_error());
            }

            let deps = depends!["error catch", value];
            Ok(ctx.call_site.clone().with(Value::new(
                value.grease_type(),
                value.or_else(|_| error_value),
                deps,
            )))
        }
        .boxed()
    })
    .into()
}
