//! Value-related intrinsics.

use abi_stable::{external_types::RMutex, std_types::RArc};
use ergo_runtime::{
    context_ext::AsContext,
    ergo_function,
    metadata::{Doc, Runtime},
    types, ContextExt,
};
use grease::{depends, item_name, types::GreaseType, value::Value};
use std::collections::BTreeMap;
use std::sync::Arc;

pub fn module() -> Value {
    crate::grease_string_map! {
        "A map of value-manipulation functions:"
        "by-content": "Identify a value by its content (forcing evaluation)." = by_content_fn(),
        "cache": "Persist a value across program runs." = cache_fn(),
        "debug": "Display debug info of a value in the debug log."  = debug_fn(),
        "doc": "Value documentation." = crate::grease_string_map! {
            "A map of documentation functions:"
            "get": "Get the documentation of a value." = doc_get_fn(),
            "set": "Set the documentation of a value." = doc_set_fn()
        },
        "dynamic": "Force a value to be dynamically-typed." = dyn_fn(),
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

fn by_content_fn() -> Value {
    ergo_function!(independent std::value::force,
    r"Identify a value by its content.

Arguments: <value>

Returns a new value which has the same type and result as the given value, but has an identity derived from the result
(typically by forcing the value and any inner values).",
    |ctx, args| {
        let val = args.next().ok_or("value not provided")?;

        args.unused_arguments()?;

        let (source, val) = val.take();
        let val = ctx.value_by_content(val, true);
        val.await.map_err(move |e| source.with(e).into_grease_error())?
    })
    .into()
}

#[derive(GreaseType)]
struct Cached {
    map: Arc<RMutex<BTreeMap<u128, Value>>>,
}

impl Default for Cached {
    fn default() -> Self {
        Cached {
            map: Arc::new(RMutex::new(Default::default())),
        }
    }
}

fn cache_fn() -> Value {
    ergo_function!(independent std::value::cache,
    r"Cache a value by identity.

Arguments: <value>

By default, caches both at runtime and to persistent storage, to be cached across runs. If the
`no-persist` keyword-arg is present, it will only cache at runtime.

Returns a value identical to the argument, however possibly loads the value from a persisted store rather than
evaluating it. If not yet persisted, when the returned value is evaluated it will evaluate the inner value and persist
it. Multiple values with the same id will be normalized to the same single runtime value.",
    |ctx, args| {
        let to_cache = args.next().ok_or("no argument to cache")?.unwrap();
        let no_persist = args.kw("no-persist").is_some();

        args.unused_arguments()?;

        let id = to_cache.id();

        let cached = ctx.shared_state(|| Ok(Cached::default()))?;

        let mut guard = cached.map.lock();
        if !guard.contains_key(&id) {
            let val = if no_persist {
                to_cache
            } else {
                let store = ctx.store.item(item_name!("cache"));
                let log = ctx.log.sublog("cache");
                let ctx = ctx.as_context().clone();
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
                            log.debug(format!("wrote cache value for {}", id));
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
            };
            guard.insert(id, val);
        }
        guard.get(&id).unwrap().clone()
    })
    .into()
}

fn variable_fn() -> Value {
    ergo_function!(independent std::value::variable,
    r"Change the identity of a value.

Arguments: <value>

Keyword Arguments:
* <depends>: The value for which the identity should be used to set the identity of the argument.

Returns a value identical to the argument, but with a different identity. If `depends` is not set, the value will have
a fixed identity derived from nothing else.",
    |ctx, args| {
        let val = args.next().ok_or("no argument to variable")?.unwrap();

        let deps = if let Some(v) = args.kw("depends") {
            depends![*v]
        } else {
            Default::default()
        };

        args.unused_arguments()?;

        val.set_dependencies(deps)
    })
    .into()
}

fn debug_fn() -> Value {
    ergo_function!(independent std::value::debug,
            r"Print a value's type (if immediately available) and identity to the debug log.

Arguments: <value>

Returns the argument exactly as-is. The logging occurs immediately.",
    |ctx, args| {
        let val = args.next().ok_or("value not provided")?;

        args.unused_arguments()?;

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

        val.unwrap()
    })
    .into()
}

fn doc_get_fn() -> Value {
    ergo_function!(independent std::value::doc::get,
    r"Get the documentation for a value.

Arguments: <value>",
    |ctx, args| {
        let val = args.next().ok_or("no argument to doc")?;

        args.unused_arguments()?;

        Doc::get(ctx, &val).into()
    })
    .into()
}

fn doc_set_fn() -> Value {
    ergo_function!(independent std::value::doc::set,
    r"Set the documentation for a value.

Arguments: <value> <doc: String>",
    |ctx, args| {
        let mut val = args.next().ok_or("no value argument")?.unwrap();
        let doc = args.next().ok_or("no documentation argument")?;

        args.unused_arguments()?;

        let doc = ctx.source_value_as::<types::String>(doc).await?.unwrap();

        val.set_metadata(&Doc, doc);
        val
    })
    .into()
}

fn dyn_fn() -> Value {
    ergo_function!(independent std::value::dynamic,
    r"Return a dynamically-typed value which evaluates to the given value.

Arguments: <value>",
    |ctx, args| {
        let val = args.next().ok_or("no value argument")?.unwrap();

        args.unused_arguments()?;

        let deps = depends![val];
        Value::dyn_new(async move { Ok(val.into_any_value()) }, deps)
    })
    .into()
}

fn meta_get_fn() -> Value {
    ergo_function!(independent std::value::meta::get,
    r"Get metadata of a value.

Arguments: <value> <metadata key>
Returns the metadata value or `Unset` if no key is set.",
    |ctx, args| {
        let val = args.next().ok_or("no value argument")?.unwrap();
        let key = args.next().ok_or("no metadata key provided")?.unwrap();

        args.unused_arguments()?;

        match val.get_metadata(&Runtime { key: key.id() }) {
            Some(v) => v.as_ref().clone(),
            None => types::Unset::new().into()
        }
    })
    .into()
}

fn meta_set_fn() -> Value {
    ergo_function!(independent std::value::meta::set,
    r"Set metadata of a value.

Arguments: <value> <metadata key> [metadata value]
You may omit metadata value to unset a metadata key.",
    |ctx, args| {
        let mut val = args.next().ok_or("no value argument")?.unwrap();
        let key = args.next().ok_or("no metadata key provided")?.unwrap();
        let meta = args.next().map(|v| v.unwrap());

        args.unused_arguments()?;

        let key = Runtime { key: key.id() };

        match meta {
            Some(meta) => val.set_metadata(&key, meta),
            None => val.clear_metadata(&key),
        }
        val
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn by_content(t) {
            t.assert_eq("self:value:by-content [a,b,c]:2", "c");
        }
    }
}
