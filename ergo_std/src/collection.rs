//! Functions over collections.

use ergo_runtime::{types, FunctionArguments};
use futures::future::{ok, FutureExt, TryFutureExt};
use grease::{bst::BstMap, depends, make_value, value::Value};

pub fn module() -> Value {
    let mut map = BstMap::default();
    map.insert("fold".into(), fold_fn());
    map.insert("map".into(), map_fn());
    types::Map(map).into()
}

fn fold_fn() -> Value {
    types::Function::new(|ctx| async move {
        let func = ctx.args.next().ok_or("fold function not provided")?;
        let orig = ctx.args.next().ok_or("fold base value not provided")?;
        let vals = ctx.args.next().ok_or("fold values not provided")?;

        ctx.unused_arguments()?;

        let func = func
            .map(|f| {
                f.typed::<types::Function>()
                    .map_err(|_| "expected a function")
            })
            .transpose()
            .map_err(|e| e.into_grease_error())?;

        let vals = vals
            .map(|v| v.typed::<types::Array>().map_err(|_| "expected an array"))
            .transpose()
            .map_err(|e| e.into_grease_error())?;

        let deps = depends!["fold", *func, *orig, *vals];

        let task = ctx.task.clone();
        let func = func.map(|v| types::Portable::portable(v, ctx));
        Ok(ctx.call_site.clone().with(Value::new(
            orig.grease_type(),
            async move {
                let (func_source, func) = func.take();
                let (vals_source, vals) = vals.take();
                let (func, vals) = task.join(func, vals).await?;
                let val = vals.owned().0.into_iter().fold(ok(orig).boxed(), |acc, v| {
                    let f = &func;
                    let src = &vals_source;
                    let fsrc = &func_source;
                    acc.and_then(move |accv| {
                        f.call(
                            FunctionArguments::positional(vec![
                                accv,
                                src.clone().with(v),
                            ]),
                            fsrc.clone(),
                        )
                    })
                    .boxed()
                });

                val.await?.await.unwrap()
            },
            deps,
        )))
    }.boxed())
    .into()
}

fn map_fn() -> Value {
    types::Function::new(|ctx| async move {
        let func = ctx.args.next().ok_or("map function not provided")?;
        let arr = ctx.args.next().ok_or("map array not provided")?;

        ctx.unused_arguments()?;

        let func = func
            .map(|f| {
                f.typed::<types::Function>()
                    .map_err(|_| "expected a function")
            })
            .transpose()
            .map_err(|e| e.into_grease_error())?;

        let arr = arr
            .map(|v| v.typed::<types::Array>()
                .map_err(|_| "expected an array"))
            .transpose()
            .map_err(|e| e.into_grease_error())?;

        let task = ctx.task.clone();
        let func = func.map(|v| types::Portable::portable(v, ctx));
        Ok(ctx.call_site.clone().with(make_value!(["map",*func,*arr] {
            let (func_source, func) = func.take();
            let (arr_source, arr) = arr.take();
            let (func,arr) = task.join(func,arr).await?;
            let arr = task.join_all(arr.owned().0
                .into_iter()
                .map(|d| func.call(FunctionArguments::positional(vec![arr_source.clone().with(d)]), func_source.clone())));

            Ok(types::Array(arr.await?.into_iter().map(|v| v.unwrap()).collect()))
        }).into()))
    }.boxed()).into()
}
