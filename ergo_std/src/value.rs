//! Value-related intrinsics.

use ergo_runtime::{
    apply_value,
    traits::{
        force_value_nested, read_from_store, type_name, write_to_store, Stored, ValueByContent,
    },
    types,
};
use futures::future::FutureExt;
use grease::{bst::BstMap, depends, item_name, value::Value};

pub fn module() -> Value {
    let mut map = BstMap::default();
    map.insert("cache".into(), cache_fn());
    map.insert("force".into(), force_fn());
    map.insert("variable".into(), variable_fn());
    types::Map(map).into()
}

fn force_fn() -> Value {
    types::Function::new(|ctx| async move {
        let val = ctx.args.next().ok_or("value not provided")?;

        ctx.unused_arguments()?;

        let (source, val) = val.take();

        match ctx.traits.get::<ValueByContent>(&val) {
            Some(t) => t
                .value_by_content(&ctx.traits, val)
                .map(|v| ctx.call_site.clone().with(v)),
            None => Err(source
                .with(format!(
                    "ValueByContent not implemented for {}",
                    type_name(&ctx.traits, &val.grease_type())
                ))
                .into_grease_error()),
        }
    }.boxed())
    .into()
}

fn cache_fn() -> Value {
    types::Function::new(|ctx| async move {
        let v = ctx.args.next().ok_or("no argument to cache")?;
        let args = std::mem::take(&mut ctx.args);

        let store = ctx.store.item(item_name!("cache"));
        let log = ctx.log.sublog("cache");

        let to_cache = apply_value(ctx, v, args.unchecked(), true).await?.unwrap();

        if let Some(_) = ctx.traits.get::<Stored>(&to_cache) {
            let deps = depends![to_cache];
            let traits = ctx.traits.clone();
            Ok(ctx.call_site.clone().with(Value::new(
                to_cache.grease_type(),
                async move {
                    let err = match read_from_store(&traits, &store, to_cache.id()) {
                        Ok(val) => {
                            log.debug(format!(
                                "successfully read cached value for {}",
                                to_cache.id()
                            ));
                            if val.grease_type() != to_cache.grease_type() {
                                "cached value had different value type".into()
                            } else {
                                return Ok(val
                                    .await
                                    .expect("value should have success value from cache"));
                            }
                        }
                        Err(e) => e,
                    };
                    log.debug(format!("failed to read cache entry, (re)caching: {}", err));
                    force_value_nested(&traits, to_cache.clone()).await?;
                    if let Err(e) =
                        write_to_store(&traits, &store, to_cache.clone()).map_err(|e| e.to_string())
                    {
                        log.warn(format!("failed to cache value: {}", e));
                    }
                    Ok(to_cache
                        .await
                        .expect("error should have been caught previously"))
                },
                deps,
            )))
        } else {
            Err("no stored trait implemented for value".into())
        }
    }.boxed())
    .into()
}

fn variable_fn() -> Value {
    types::Function::new(|ctx| async move {
        let v = ctx.args.next().ok_or("no argument to variable")?;

        let deps = if let Some(v) = ctx.args.kw("depends") {
            depends![*v]
        } else {
            Default::default()
        };

        let args = std::mem::take(&mut ctx.args);
        ctx.unused_arguments()?;

        let val = apply_value(ctx, v, args.unchecked(), true).await?;

        Ok(val.map(|v| v.set_dependencies(deps)))
    }.boxed())
    .into()
}
