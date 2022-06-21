//! Create a value cache from a Path.

use ergo_runtime::abi_stable::{
    future::BoxFuture,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox},
    u128::U128,
    StableAbi,
};
use ergo_runtime::{
    error::DiagnosticInfo, traits, type_system::ErgoType, types, Context, RResult, Value,
};

mod memory;
mod sqlite;

#[derive(ErgoType, StableAbi, Clone)]
#[repr(C)]
pub struct Cache(RArc<CacheInterface_TO<'static, RBox<()>>>);

unsafe impl ergo_runtime::value::InnerValues for Cache {
    fn visit<'a, F: FnMut(&'a Value)>(&'a self, _f: F) {}
}

impl Cache {
    fn new<T: CacheInterface + 'static>(interface: T) -> Self {
        Cache(RArc::new(CacheInterface_TO::from_value(
            interface, TD_Opaque,
        )))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, StableAbi)]
#[repr(u8)]
pub enum ErrorHandling {
    Store,
    StoreTopLevel,
    Error,
}

impl ErrorHandling {
    pub fn child_should_error(&self) -> bool {
        self != &ErrorHandling::Store
    }

    pub fn top_level_should_error(&self) -> bool {
        self == &ErrorHandling::Error
    }
}

/// Evaluate a value deeply for caching.
pub async fn eval_for_cache(
    mut v: Value,
    error_handling: ErrorHandling,
) -> ergo_runtime::Result<Value> {
    if let Err(e) = Context::eval(&mut v).await {
        if error_handling.top_level_should_error() {
            return Err(e);
        }
    }
    match traits::deep_eval(v).await {
        Ok(v) => Ok(v),
        Err(e) => {
            if error_handling.child_should_error() {
                Err(e)
            } else {
                Ok(e.into())
            }
        }
    }
}

#[sabi_trait]
trait CacheInterface: Send + Sync {
    #[sabi(last_prefix_field)]
    fn cache_value(
        &self,
        key: U128,
        value: Value,
        error_handling: ErrorHandling,
    ) -> BoxFuture<'_, RResult<Value>>;
}

pub fn r#type() -> Value {
    types::Type {
        tp: Cache::ergo_type(),
        index: crate::make_string_map! {
            "memory" = memory(),
            "open" = open(),
            "entry" = entry()
        },
    }
    .into()
}

#[types::ergo_fn]
/// Create a new in-memory cache.
///
/// Arguments: `()`
///
/// As a means of preventing the accidental creation of reference cycles, entries of an in-memory
/// cache will be deeply evaluated (using `Functor`). Otherwise a reference cycle could be created
/// by e.g. referencing the cache in a value that is inserted in the cache.
async fn memory(_: types::Unit) -> Value {
    Value::with_id(Cache::new(memory::MemCache::default()), CALL_DEPENDS)
}

#[types::ergo_fn]
/// Open a cache at the given Path.
///
/// Arguments: `(Into:into $Path |> :path)`
async fn open(path: _) -> Value {
    let path = traits::into::<types::Path>(path).await?;
    let cache = sqlite::SqliteCache::open(path.as_ref().as_ref())
        .add_note(format!("cache path was {}", path.as_ref().display()))?;
    Value::with_id(Cache::new(cache), CALL_DEPENDS)
}

#[types::ergo_fn]
/// Retrieve (and possibly store) a value in a cache.
///
/// Arguments: `(Cache :cache) :value`
///
/// Keyed Arguments:
/// * `:key` - a value to use as the cache key, defaulting to `value`.
///   The key is what determines the cache entry, whereas the `value` identity (if different from
///   the `key` determines whether the value can be retrieved or whether it needs to be
///   re-evaluated and updated.
/// * `:allow-error` - whether to allow errors when caching; may be:
///     * `Bool:false`, `:unset` (the default) - disallow errors to be cached, propagating them as normal
///     * `Bool:true`, `()` - allow errors to be cached
///     * `top` - allow only the immediate value passed to the function to be cached if it is an
///       error
async fn entry(cache: Cache, value: _, (key): [_], (allow_error): [_]) -> Value {
    let key = match key {
        Some(v) => v.id().await,
        None => value.id().await,
    };
    let error_handling = match allow_error {
        None => ErrorHandling::Error,
        Some(mut v) => {
            let src = ergo_runtime::metadata::Source::get(&v);
            Context::eval(&mut v).await?;
            ergo_runtime::value::match_value! {v,
                types::Bool(b) => if b { ErrorHandling::Store } else { ErrorHandling::Error },
                types::Unset => ErrorHandling::Error,
                types::Unit => ErrorHandling::Store,
                types::String(s) => {
                    if s == "top" {
                        ErrorHandling::StoreTopLevel
                    } else {
                        Err(src.with("invalid allow-error value").into_error())?
                    }
                },
                o => Err(traits::type_error(o, "Bool, Unset, Unit, or String"))?
            }
        }
    };
    cache
        .as_ref()
        .0
        .cache_value(key.into(), value, error_handling)
        .await
        .into_result()
        .into()
}

ergo_runtime::type_system::ergo_traits_fn! {
    ergo_runtime::ergo_type_name!(traits, Cache);
}
