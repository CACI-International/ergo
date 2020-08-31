//! Functions over collections.

use abi_stable::std_types::RVec;
use grease::{bst::BstMap, depends, make_value, value::Value};
use so_runtime::{types, FunctionArguments};

pub fn module() -> Value {
    let mut map = BstMap::default();
    map.insert("fold".into(), fold_fn());
    map.insert("map".into(), map_fn());
    types::Map(map).into()
}

fn fold_fn() -> Value {
    types::Function::new(|ctx| {
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
                let val = vals
                    .owned()
                    .0
                    .into_iter()
                    .fold(Ok(orig), |acc, v| {
                        acc.and_then(|acc| {
                            func.call(
                                FunctionArguments::positional(vec![acc, vals_source.clone().with(v)]),
                                func_source.clone(),
                            )
                            .into()
                        })
                    })?
                    .unwrap();
                val.await
            },
            deps,
        )))
    })
    .into()
}

fn map_fn() -> Value {
    types::Function::new(|ctx| {
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
            arr.owned()
                .0
                .into_iter()
                .map(|d| func.call(FunctionArguments::positional(vec![arr_source.clone().with(d)]), func_source.clone())
                            .map(|src| src.unwrap())
                ).collect::<Result<RVec<_>, _>>()
            .map(types::Array)
        }).into()))
    }).into()
}
