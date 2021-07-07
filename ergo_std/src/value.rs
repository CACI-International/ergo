//! Value-related intrinsics.

use ergo_runtime::abi_stable::external_types::RMutex;
use ergo_runtime::{
    context::item_name,
    depends,
    metadata::{Runtime, Source},
    traits,
    type_system::ErgoType,
    types, Value,
};
use std::collections::HashMap as CacheMap;
use std::sync::Arc;

pub fn module() -> Value {
    crate::make_string_map! {
        "by-content" = by_content(),
        "cache" = cache(),
        "debug" = debug(),
        "dynamic" = crate::make_string_map! {
            "get" = dynamic_binding_get(),
            "eval" = dynamic_binding_set()
        },
        "eval" = eval(),
        "meta" = crate::make_string_map! {
            "get" = meta_get(),
            "set" = meta_set()
        },
        "source-path" = source_path(),
        "variable" = variable()
    }
}

#[types::ergo_fn]
/// Identify a value by its content.
///
/// Arguments: `:value`
///
/// Returns a new value which has the same type and result as the given value, but has an identity
/// derived from the result (typically by forcing the value and any inner values).
async fn by_content(value: _) -> Value {
    traits::value_by_content(CONTEXT, value, true).await
}

#[derive(ErgoType)]
struct Cached {
    map: RMutex<CacheMap<u128, Arc<futures::lock::Mutex<Option<Value>>>>>,
}

impl Default for Cached {
    fn default() -> Self {
        Cached {
            map: RMutex::new(Default::default()),
        }
    }
}

#[types::ergo_fn]
/// Cache a value by identity.
///
/// Arguments: `:value`
///
/// Keyed Arguments:
/// * `:no-persist`: If present, the caching will not be persisted (only runtime caching will occur).
///
/// Caches the given value both at runtime and to persistent storage (to be cached across invocations).
///
/// Returns a value identical to the argument, however possibly loads the value from a persisted store
/// rather than evaluating it. If not yet persisted, when the returned value is evaluated it will
/// evaluate the inner value and persist it. Multiple values with the same id will be normalized to the
/// same single runtime value.
async fn cache(value: _, (no_persist): [_]) -> Value {
    let id = value.id();

    let no_persist = no_persist.is_some();

    let cached = CONTEXT.shared_state.get(|| Ok(Cached::default())).unwrap();

    let entry = cached
        .map
        .lock()
        .entry(id)
        .or_insert_with(|| Arc::new(futures::lock::Mutex::new(None)))
        .clone();
    let mut guard = entry.lock().await;
    if guard.is_none() {
        let value = if no_persist {
            value
        } else {
            let store = CONTEXT.store.item(item_name!("cache"));
            let log = CONTEXT.log.sublog("cache");

            match traits::read_from_store(CONTEXT, &store, id).await {
                Ok(mut val) => {
                    log.debug(format!("successfully read cached value for {}", id));
                    val.copy_metadata(&value);
                    val
                }
                Err(err) => {
                    log.debug(format!(
                        "failed to read cache value for {}, (re)caching: {}",
                        id, err
                    ));
                    drop(traits::eval_nested(CONTEXT, value.clone()).await);
                    if let Err(e) = traits::write_to_store(CONTEXT, &store, value.clone()).await {
                        log.warn(format!("failed to cache value for {}: {}", id, e));
                    } else {
                        log.debug(format!("wrote cache value for {}", id));
                    }
                    value
                }
            }
        };
        *guard = Some(value);
    }
    guard.as_ref().unwrap().clone()
}

#[types::ergo_fn]
/// Change the identity of a value.
///
/// Arguments: `:value`
///
/// Keyed Arguments:
/// * `:depends`: A value whose identity will be used to set the identity of the returned value.
///
/// Returns a value identical to the argument, but with a different identity. If `depends` is not set, the value will have
/// a fixed identity derived from nothing else.
async fn variable(mut value: _, (depends): [_]) -> Value {
    let deps = if let Some(v) = depends {
        depends![v]
    } else {
        Default::default()
    };

    value.set_dependencies(deps);
    value
}

#[types::ergo_fn]
/// Print a value's type, identity, and (if typed) display output to the debug log.
///
/// Arguments: `:value`
///
/// Returns the argument as-is.
async fn debug(value: _) -> Value {
    let name = traits::type_name(CONTEXT, &value);

    let rest = if value.is_evaluated() {
        let mut s = String::from(", value: ");
        {
            let mut formatter = traits::Formatter::new(&mut s);
            if let Err(e) = traits::display(CONTEXT, value.clone(), &mut formatter).await {
                drop(formatter);
                s.push_str("<error: ");
                s.push_str(&e.to_string());
                s.push('>');
            }
        }
        s
    } else {
        Default::default()
    };

    CONTEXT.log.debug(
        Source::get(&value)
            .with(format!("type: {}{}, id: {}", name, rest, value.id()))
            .to_string(),
    );

    value
}

#[types::ergo_fn]
/// Evaluate a value.
///
/// Arguments: `:value`
async fn eval(mut value: _) -> Value {
    drop(CONTEXT.eval(&mut value).await);
    value
}

#[types::ergo_fn]
/// Get metadata of a value.
///
/// Arguments: `:value :metadata-key`
///
/// Returns the metadata value or `Unset` if no key is set.
async fn meta_get(value: _, metadata_key: _) -> Value {
    match value.get_metadata(&Runtime {
        key: metadata_key.id(),
    }) {
        Some(v) => v.as_ref().clone(),
        None => types::Unset.into(),
    }
}

#[types::ergo_fn]
/// Set metadata of a value.
///
/// Arguments: `:value :metadata-key (Optional :metadata-value)`
///
/// You may omit the metadata value to unset a metadata key.
async fn meta_set(mut value: _, metadata_key: _, metadata_value: [_]) -> Value {
    let key = Runtime {
        key: metadata_key.id(),
    };

    match metadata_value {
        Some(meta) => value.set_metadata(&key, meta),
        None => value.clear_metadata(&key),
    }
    value
}

#[types::ergo_fn]
/// Get the source path of a value.
///
/// Arguments: `:value`
///
/// Returns the Path of the script file from which the value originates, or Unset if no path is
/// available.
async fn source_path(value: _) -> Value {
    match Source::get_option(&value).and_then(|s| s.path()) {
        None => types::Unset.into(),
        Some(p) => types::Path::from(p).into(),
    }
}

#[types::ergo_fn]
/// Get a dynamic binding.
///
/// Arguments: `:key`
///
/// Returns the dynamic binding corresponding to `key`, or `Unset` if none exists.
async fn dynamic_binding_get(key: _) -> Value {
    match CONTEXT.dynamic_scope.get(&key) {
        None => types::Unset.into(),
        Some(r) => (*r).clone(),
    }
}

#[types::ergo_fn]
/// Evaluate a value with the given dynamic bindings.
///
/// Arguments: `(Map :bindings) :eval`
///
/// Returns the result of evaluating `eval` with all `bindings` set in the dynamic scope.
async fn dynamic_binding_set(bindings: types::Map, mut eval: _) -> Value {
    CONTEXT
        .fork(
            |ctx| {
                for (k, v) in bindings.to_owned().0 {
                    ctx.dynamic_scope.set(&Source::extract(k), v);
                }
            },
            |ctx| async move {
                drop(ctx.eval(&mut eval).await);
                eval
            },
        )
        .await
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn by_content(t) {
            t.assert_eq("self:value:by-content [a,b,c]:2", "c");
        }
    }

    ergo_script::test! {
        fn eval(t) {
            t.assert_script_ne("fn :a -> :a |> str", "str");
            t.assert_eq("self:value:eval <| fn :a -> :a |> str", "str");
        }
    }

    ergo_script::test! {
        fn source_path(t) {
            t.assert_eq("x = 1; self:value:source-path :x", "self:type:Unset:");
        }
    }

    ergo_script::test! {
        fn dynamic_binding(t) {
            t.assert_eq("self:value:dynamic:get something", "self:type:Unset:");
            t.assert_eq("v = self:value:dynamic:get something; self:value:dynamic:eval { something = value } :v", "value");
            t.assert_eq("f = fn :x -> <| self:value:dynamic:get my_func |> :x
                say_hello = fn :name -> self:string:format 'hi, {}' :name
                self:value:dynamic:eval { my_func = :say_hello } <| f dude", "'hi, dude'");
        }
    }

    ergo_script::test! {
        fn dynamic_binding_multiple_scopes(t) {
            t.assert_content_eq("the-value = self:value:dynamic:get the-value
                [self:value:dynamic:eval {the-value = 10} :the-value, self:value:dynamic:eval {the-value = hi} :the-value]", "[10,hi]");
        }
    }
}
