//! Value-related intrinsics.

use ergo_runtime::{
    ergo_function,
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
    map.insert("debug".into(), debug_fn());
    map.insert("force".into(), force_fn());
    map.insert("variable".into(), variable_fn());
    types::Map(map).into()
}

fn force_fn() -> Value {
    ergo_function!(independent std::value::force, |ctx| {
        let val = ctx.args.next().ok_or("value not provided")?;

        ctx.unused_arguments()?;

        let (source, val) = val.take();

        match ctx.traits.get::<ValueByContent>(&val) {
            Some(t) => {
                force_value_nested(&ctx.traits, val.clone()).await?;
                t.value_by_content(&ctx.traits, val)?
            }
            None => return Err(source
                .with(format!(
                    "ValueByContent not implemented for {}",
                    type_name(&ctx.traits, &val.grease_type())
                ))
                .into_grease_error()),
        }
    })
    .into()
}

fn cache_fn() -> Value {
    ergo_function!(independent std::value::cache, |ctx| {
        let to_cache = ctx.args.next().ok_or("no argument to cache")?.unwrap();

        ctx.unused_arguments()?;

        let store = ctx.store.item(item_name!("cache"));
        let log = ctx.log.sublog("cache");

        if let Some(_) = ctx.traits.get::<Stored>(&to_cache) {
            let deps = depends![to_cache];
            let traits = ctx.traits.clone();
            Value::new(
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
                    log.debug(format!("failed to read cache value for {}, (re)caching: {}", to_cache.id(), err));
                    force_value_nested(&traits, to_cache.clone()).await?;
                    if let Err(e) = write_to_store(&traits, &store, to_cache.clone())
                    {
                        log.warn(format!("failed to cache value for {}: {}", to_cache.id(), e));
                    }
                    Ok(to_cache
                        .await
                        .expect("error should have been caught previously"))
                },
                deps,
            )
        } else {
            return Err("no stored trait implemented for value".into())
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

                ctx.log.debug(
                    val.as_ref()
                        .map(|v| {
                            format!(
                                "type: {}, id: {}",
                                type_name(&ctx.traits, &*v.grease_type()),
                                v.id()
                            )
                        })
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
