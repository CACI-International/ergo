//! Value-related intrinsics.

use abi_stable::std_types::RArc;
use ergo_runtime::{context_ext::AsContext, ergo_function, types, ContextExt};
use futures::future::FutureExt;
use grease::{depends, item_name, value::Value};

pub fn module() -> Value {
    crate::grease_string_map! {
        "cache" = cache_fn(),
        "debug" = debug_fn(),
        "force" = force_fn(),
        "variable" = variable_fn()
    }
}

fn force_fn() -> Value {
    ergo_function!(independent std::value::force, |ctx| {
        let val = ctx.args.next().ok_or("value not provided")?;

        ctx.unused_arguments()?;

        let (source, val) = val.take();
        let val = ctx.value_by_content(val);
        val.await.map_err(move |e| source.with(e).into_grease_error())?
    })
    .into()
}

fn cache_fn() -> Value {
    ergo_function!(independent std::value::cache, |ctx| {
        let mut to_cache = ctx.args.next().ok_or("no argument to cache")?.unwrap();

        ctx.unused_arguments()?;

        let store = ctx.store.item(item_name!("cache"));
        let log = ctx.log.sublog("cache");

        let ctx = ctx.as_context().clone();

        let id = to_cache.id();

        match to_cache.grease_type_immediate() {
            Some(tp) => unsafe {
                Value::with_id(RArc::new(tp.clone()), async move {
                    let err = match ctx.read_from_store(&store, id).await {
                        Ok(val) => {
                            log.debug(format!(
                                "successfully read cached value for {}",
                                id
                            ));
                            if val.grease_type_immediate() != to_cache.grease_type_immediate() {
                                "cached value had different value type".into()
                            } else {
                                return Ok(val
                                    .await
                                    .expect("value should have success value from cache"));
                            }
                        }
                        Err(e) => e,
                    };
                    log.debug(format!("failed to read cache value for {}, (re)caching: {}", id, err));
                    ctx.force_value_nested(to_cache.clone()).await?;
                    if let Err(e) = ctx.write_to_store(&store, to_cache.clone()).await
                    {
                        log.warn(format!("failed to cache value for {}: {}", id, e));
                    }
                    Ok(to_cache
                        .await
                        .expect("error should have been caught previously"))
                }, id)
            },
            None => Value::dyn_with_id(async move {
                let err = match ctx.read_from_store(&store, id).await {
                    Ok(mut val) => {
                        log.debug(format!(
                            "successfully read cached value for {}",
                            id
                        ));
                        return Ok(val.make_any_value().await.expect("value should have success value from cache"));
                    }
                    Err(e) => e,
                };
                log.debug(format!("failed to read cache value for {}, (re)caching: {}", id, err));
                ctx.force_value_nested(to_cache.clone()).await?;
                if let Err(e) = ctx.write_to_store(&store, to_cache.clone()).await
                {
                    log.warn(format!("failed to cache value for {}: {}", id, e));
                }
                Ok(to_cache
                    .make_any_value()
                    .await
                    .expect("error should have been caught previously"))
            }, id)
        }
    })
    .into()
}

fn variable_fn() -> Value {
    ergo_function!(independent std::value::variable, |ctx| {
        let val = ctx.args.next().ok_or("no argument to variable")?.unwrap();

        let deps = if let Some(v) = ctx.args.kw("depends") {
            depends![*v]
        } else {
            Default::default()
        };

        ctx.unused_arguments()?;

        val.set_dependencies(deps)
    })
    .into()
}

fn debug_fn() -> Value {
    types::Function::new(
        |ctx| {
            async move {
                let val = ctx.args.next().ok_or("value not provided")?;

                ctx.unused_arguments()?;

                let name = match val.grease_type_immediate() {
                    Some(tp) => {
                        let tp = ctx.type_name(tp);
                        tp.await?
                    }
                    None => "<dynamic>".into(),
                };

                ctx.log.debug(
                    val.as_ref()
                        .map(|v| format!("type: {}, id: {}", name, v.id()))
                        .to_string(),
                );

                Ok(val)
            }
            .boxed()
        },
        depends![ergo_runtime::namespace_id!(std::value::debug)],
    )
    .into()
}
