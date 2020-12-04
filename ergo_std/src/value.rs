//! Value-related intrinsics.

use abi_stable::std_types::RArc;
use ergo_runtime::{
    context_ext::AsContext,
    ergo_function,
    metadata::{Doc, Runtime},
    types, ContextExt,
};
use futures::future::FutureExt;
use grease::{
    depends, item_name,
    value::{IntoValue, Value},
};

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of value-manipulation functions:"
        "by-content": "Identify a value by its content (forcing evaluation)." = by_content_fn(true),
        "cache": "Persist a value across program runs." = cache_fn(),
        "debug": "Display debug info of a value in the debug log."  = debug_fn(),
        "doc": "Value documentation." = crate::grease_string_map! {
            "A map of documentation functions:"
            "get": "Get the documentation of a value." = doc_get_fn(),
            "set": "Set the documentation of a value." = doc_set_fn()
        },
        "meta": "Value metadata manipulation." = crate::grease_string_map! {
            r"A map of metadata functions.
Arbitrary values can be attached as metadata to other values, keyed by the identity of a third key value.
Metadata allows one to retrieve associated values without evaluating a value. While this could be achieved by wrapping
values with a map, metadata does not alter how other code treats the value."
            "get": "Get the metadata of a value for a specific key." = meta_get_fn(),
            "set": "Set the metadata of a value for a specific key." = meta_set_fn()
        },
        "variable": "Mark a value as being variable, i.e., with an identity not wholly derived from its dependencies." = variable_fn()
    }
}

fn by_content_fn(deep: bool) -> Value {
    ergo_function!(independent std::value::force, |ctx| {
        let val = ctx.args.next().ok_or("value not provided")?;

        ctx.unused_arguments()?;

        let (source, val) = val.take();
        let val = ctx.value_by_content(val, deep);
        val.await.map_err(move |e| source.with(e).into_grease_error())?
    })
    .into()
}

fn cache_fn() -> Value {
    ergo_function!(independent std::value::cache, |ctx| {
        let to_cache = ctx.args.next().ok_or("no argument to cache")?.unwrap();

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
                    Ok(val) => {
                        log.debug(format!(
                            "successfully read cached value for {}",
                            id
                        ));
                        return Ok(val.into_any_value());
                    }
                    Err(e) => e,
                };
                log.debug(format!("failed to read cache value for {}, (re)caching: {}", id, err));
                ctx.force_value_nested(to_cache.clone()).await?;
                if let Err(e) = ctx.write_to_store(&store, to_cache.clone()).await
                {
                    log.warn(format!("failed to cache value for {}: {}", id, e));
                }
                Ok(to_cache.into_any_value())
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

fn doc_get_fn() -> Value {
    ergo_function!(independent std::value::doc::get, |ctx| {
        let val = ctx.args.next().ok_or("no argument to doc")?;

        ctx.unused_arguments()?;

        Doc::get(ctx, &val).into()
    })
    .into()
}

fn doc_set_fn() -> Value {
    ergo_function!(independent std::value::doc::set, |ctx| {
        let mut val = ctx.args.next().ok_or("no value argument")?.unwrap();
        let doc = ctx.args.next().ok_or("no documentation argument")?;

        ctx.unused_arguments()?;

        let doc = ctx.source_value_as::<types::String>(doc).await?.unwrap();

        val.set_metadata(&Doc, doc);
        val
    })
    .into()
}

fn meta_get_fn() -> Value {
    ergo_function!(independent std::value::meta::get, |ctx| {
        let val = ctx.args.next().ok_or("no value argument")?.unwrap();
        let key = ctx.args.next().ok_or("no metadata key provided")?.unwrap();

        ctx.unused_arguments()?;

        match val.get_metadata(&Runtime { key: key.id() }) {
            Some(v) => v.as_ref().clone(),
            None => types::Unit.into_value()
        }
    })
    .into()
}

fn meta_set_fn() -> Value {
    ergo_function!(independent std::value::meta::set, |ctx| {
        let mut val = ctx.args.next().ok_or("no value argument")?.unwrap();
        let key = ctx.args.next().ok_or("no metadata key provided")?.unwrap();
        let meta = ctx.args.next().map(|v| v.unwrap());

        ctx.unused_arguments()?;

        let key = Runtime { key: key.id() };

        match meta {
            Some(meta) => val.set_metadata(&key, meta),
            None => val.clear_metadata(&key),
        }
        val
    })
    .into()
}
