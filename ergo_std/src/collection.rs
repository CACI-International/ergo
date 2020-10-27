//! Functions over collections.

use abi_stable::std_types::RVec;
use ergo_runtime::{ergo_function, types, ContextExt, FunctionArguments};
use futures::future::{ok, FutureExt, TryFutureExt};
use grease::{bst::BstMap, depends, make_value, value::Value};

pub fn module() -> Value {
    let mut map = BstMap::default();
    map.insert("entries".into(), entries_fn());
    map.insert("fold".into(), fold_fn());
    map.insert("map".into(), map_fn());
    types::Map(map).into()
}

fn fold_fn() -> Value {
    ergo_function!(std::collection::fold, |ctx| {
        let func = ctx.args.next().ok_or("fold function not provided")?;
        let orig = ctx.args.next().ok_or("fold base value not provided")?;
        let vals = ctx.args.next().ok_or("fold values not provided")?;

        ctx.unused_arguments()?;

        let func = ctx.source_value_as::<types::Function>(func);
        let func = func.await?;

        let vals = ctx.source_value_as::<types::Array>(vals);
        let vals = vals.await?;

        let deps = depends![*func, *orig, *vals];

        let task = ctx.task.clone();
        let func = func.map(|v| types::Portable::portable(v, ctx));
        Value::dyn_new(
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
                            FunctionArguments::positional(vec![accv, src.clone().with(v)]),
                            fsrc.clone(),
                        )
                    })
                    .boxed()
                });

                val.await?.make_any_value().await
            },
            deps,
        )
    })
    .into()
}

fn map_fn() -> Value {
    ergo_function!(std::collection::map, |ctx| {
        let func = ctx.args.next().ok_or("map function not provided")?;
        let arr = ctx.args.next().ok_or("map array not provided")?;

        ctx.unused_arguments()?;

        let func = ctx.source_value_as::<types::Function>(func);
        let func = func.await?;

        let arr = ctx.source_value_as::<types::Array>(arr);
        let arr = arr.await?;

        let task = ctx.task.clone();
        let func = func.map(|v| types::Portable::portable(v, ctx));
        make_value!([*func,*arr] {
            let (func_source, func) = func.take();
            let (arr_source, arr) = arr.take();
            let (func,arr) = task.join(func,arr).await?;
            let arr = task.join_all(arr.owned().0
                .into_iter()
                .map(|d| func.call(FunctionArguments::positional(vec![arr_source.clone().with(d)]), func_source.clone())));

            Ok(types::Array(arr.await?.into_iter().map(|v| v.unwrap()).collect()))
        }).into()
    }).into()
}

fn entries_fn() -> Value {
    ergo_function!(std::collection::entries, |ctx| {
        let value = ctx.args.next().ok_or("map not provided")?;

        ctx.unused_arguments()?;

        let map = ctx.source_value_as::<types::Map>(value);
        let map = map.await?.unwrap();

        make_value!([map] {
            let mut vals = RVec::new();
            for (k,v) in map.await?.owned().0 {
                let mut entry = BstMap::new();
                entry.insert("key".into(), types::String::from(k).into());
                entry.insert("value".into(), v);

                vals.push(types::Map(entry).into());
            }

            Ok(types::Array(vals))
        })
        .into()
    })
    .into()
}
