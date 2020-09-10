//! If function.

use ergo_runtime::{source::Source, traits::IntoTyped, types};
use futures::future::FutureExt;
use grease::value::{IntoValue, Value};

pub fn function() -> Value {
    types::Function::new(|ctx| {
        async move {
            let cond = ctx.args.next().ok_or("no condition specified")?;
            let if_true = ctx.args.next().ok_or("no truth value specified")?;
            let if_false = ctx.args.next();

            ctx.unused_arguments()?;

            match ctx.traits.get::<IntoTyped<bool>>(&cond) {
                Some(t) => {
                    let cond = t.into_typed(cond.unwrap());
                    let vals = vec![
                        if_false.map(Source::unwrap).unwrap_or(().into_value()),
                        if_true.unwrap(),
                    ];

                    let val = types::Either::new(vals, cond.map(|v| v.owned().into()));

                    Ok(ctx.call_site.clone().with(val.into()))
                }
                None => {
                    // Immediately evaluate to true type
                    // This case should technically never happen because IntoTyped<bool> has a blanket
                    // implementation for all types.
                    Ok(if_true)
                }
            }
        }
        .boxed()
    })
    .into()
}
