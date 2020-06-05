//! Cache arbitrary values.

use super::{apply_value, builtin_function_prelude::*};
use crate::script::traits::nested::force_value_nested;
use bincode;
use grease::{
    depends, item_name, trait_generator, Item, ItemContent, SplitInto, Trait, TraitImpl, Traits,
    ValueData, ValueType,
};
use std::collections::BTreeMap;

/// Context passed to Stored traits.
pub struct StoredContext {
    pub traits: Traits,
    store_item: Item,
}

/// Stored trait information.
#[derive(Clone, Trait)]
pub struct StoredTrait {
    put: fn(&StoredContext, &ValueData, &mut ItemContent) -> std::io::Result<()>,
    get: fn(&StoredContext, &mut ItemContent) -> std::io::Result<ValueData>,
}

impl StoredTrait {
    pub fn put(
        &self,
        ctx: &StoredContext,
        val: &ValueData,
        into: &mut ItemContent,
    ) -> std::io::Result<()> {
        (self.put)(ctx, val, into)
    }

    pub fn get(&self, ctx: &StoredContext, from: &mut ItemContent) -> std::io::Result<ValueData> {
        (self.get)(ctx, from)
    }
}

/// Stored trait reference.
pub type Stored = grease::TraitRef<StoredTrait>;

impl From<StoredTrait> for TraitImpl {
    fn from(v: StoredTrait) -> Self {
        TraitImpl::new(StoredTrait::trait_type(), v)
    }
}

/// Stored rust trait.
///
/// This trait should be used with `impl_stored`.
pub trait StoredValue
where
    Self: Sized,
{
    fn put(&self, ctx: &StoredContext, into: &mut ItemContent) -> std::io::Result<()>;
    fn get(ctx: &StoredContext, from: &mut ItemContent) -> std::io::Result<Self>;
}

fn put_data<T: StoredValue + Sync>(
    ctx: &StoredContext,
    data: &ValueData,
    into: &mut ItemContent,
) -> std::io::Result<()> {
    unsafe { data.as_ref::<T>() }.put(ctx, into)
}

fn get_data<T: StoredValue>(
    ctx: &StoredContext,
    from: &mut ItemContent,
) -> std::io::Result<ValueData> {
    T::get(ctx, from).map(ValueData::new)
}

/// Convenience function to create Stored trait instances for rust types.
pub fn impl_stored<T: StoredValue + Sync>() -> TraitImpl {
    TraitImpl::for_trait::<Stored>(StoredTrait {
        put: put_data::<T>,
        get: get_data::<T>,
    })
}

fn bincode_to_io_result<T>(res: bincode::Result<T>) -> std::io::Result<T> {
    res.map_err(|e| match *e {
        bincode::ErrorKind::Io(error) => error,
        other => std::io::Error::new(std::io::ErrorKind::Other, other),
    })
}

macro_rules! StoredValueSerde {
    ( $t:ty ) => {
        impl StoredValue for $t {
            fn put(&self, _ctx: &StoredContext, into: &mut ItemContent) -> std::io::Result<()> {
                bincode_to_io_result(bincode::serialize_into(into, self))
            }

            fn get(_ctx: &StoredContext, from: &mut ItemContent) -> std::io::Result<Self> {
                bincode_to_io_result(bincode::deserialize_from(from))
            }
        }
    };
}

StoredValueSerde!(ScriptUnit);

StoredValueSerde!(ScriptString);

StoredValueSerde!(std::path::PathBuf);

impl StoredValue for ScriptArray {
    fn put(&self, ctx: &StoredContext, into: &mut ItemContent) -> std::io::Result<()> {
        let mut ids: Vec<u128> = Vec::new();
        for v in self.0.iter().cloned() {
            ids.push(v.id());
            write_to_cache(ctx, v)?;
        }
        bincode_to_io_result(bincode::serialize_into(into, &ids))
    }

    fn get(ctx: &StoredContext, from: &mut ItemContent) -> std::io::Result<Self> {
        let ids: Vec<u128> = bincode_to_io_result(bincode::deserialize_from(from))?;
        let mut vals = Vec::new();
        for id in ids {
            vals.push(read_from_cache(ctx, id)?);
        }
        Ok(ScriptArray(vals))
    }
}

impl StoredValue for ScriptMap {
    fn put(&self, ctx: &StoredContext, into: &mut ItemContent) -> std::io::Result<()> {
        let mut ids: BTreeMap<String, u128> = BTreeMap::new();
        for (k, v) in self.0.iter() {
            let v = v.clone();
            ids.insert(k.clone(), v.id());
            write_to_cache(ctx, v)?;
        }
        bincode_to_io_result(bincode::serialize_into(into, &ids))
    }

    fn get(ctx: &StoredContext, from: &mut ItemContent) -> std::io::Result<Self> {
        let ids: BTreeMap<String, u128> = bincode_to_io_result(bincode::deserialize_from(from))?;
        let mut vals = BTreeMap::new();
        for (k, id) in ids {
            vals.insert(k, read_from_cache(ctx, id)?);
        }
        Ok(ScriptMap(vals))
    }
}

trait_generator!(impl_stored(
    ScriptUnit,
    ScriptString,
    std::path::PathBuf,
    ScriptArray,
    ScriptMap
));

/// Read a Value from the cache by id.
pub fn read_from_cache(ctx: &StoredContext, id: u128) -> Result<Value, Error> {
    let item = ctx.store_item.value_id(id);
    let mut content = item.read_existing().map_err(|e| e.to_string())?;
    let tp_id = bincode_to_io_result(bincode::deserialize_from(&mut content))?;
    let tp_data = bincode_to_io_result(bincode::deserialize_from(&mut content))?;
    let tp = std::sync::Arc::new(ValueType::with_data(tp_id, tp_data));
    if let Some(s) = ctx.traits.get_type::<Stored>(tp.clone()) {
        let data = s.get(ctx, &mut content)?;
        Ok(Value::from_raw(tp, std::sync::Arc::new(data), id))
    } else {
        Err(format!("no stored trait for {}", tp_id).into())
    }
}

/// Write a value to the cache.
///
/// The value must already be forced (using traits::nested::force_value_nested).
pub fn write_to_cache(ctx: &StoredContext, v: Value) -> Result<(), Error> {
    if let Some(s) = ctx.traits.get::<Stored>(&v) {
        let item = ctx.store_item.value(&v);
        let data = v
            .peek()
            .expect("value not forced prior to writing")
            .clone()?;
        item.write()
            .and_then(|mut content| {
                let tp = v.value_type();
                bincode_to_io_result(bincode::serialize_into(&mut content, &tp.id))?;
                bincode_to_io_result(bincode::serialize_into(&mut content, &tp.data))?;
                s.put(ctx, data.as_ref(), &mut content)
            })
            .map_err(|e| e.to_string())?;
        Ok(())
    } else {
        Err(format!("no stored trait for {}", v.value_type().id).into())
    }
}

def_builtin!(ctx => {
    let v = ctx.args.next().ok_or("no argument to cache")?;
    let args = std::mem::take(&mut ctx.args);

    let store = ctx.store.item(item_name!("cache"));
    let log = ctx.log.sublog("cache");

    let to_cache = match ctx.split_map(move |ctx| apply_value(ctx, v, args.unchecked(), true))? {
        Eval::Error => return Ok(Eval::Error),
        Eval::Value(v) => v
    };

    let traits = ctx.traits.clone();
    if let Some(_) = ctx.traits.get::<Stored>(&to_cache) {
        let deps = depends![to_cache];
        Ok(Eval::Value(Value::new(to_cache.value_type(), async move {
            let stored_ctx = StoredContext{traits,store_item: store};
            let err = match read_from_cache(&stored_ctx, to_cache.id()) {
                Ok(val) => {
                    log.debug(format!("successfully read cached value for {}", to_cache.id()));
                    if val.value_type() != to_cache.value_type() {
                        "cached value had different value type".into()
                    }
                    else {
                        return Ok(val.await.expect("value should have success value from cache"));
                    }
                }
                Err(e) => e
            };
            log.debug(format!("failed to read cache entry, (re)caching: {}", err));
            force_value_nested(&stored_ctx.traits, to_cache.clone()).await?;
            if let Err(e) = write_to_cache(&stored_ctx, to_cache.clone()).map_err(|e| e.to_string()) {
                log.warn(format!("failed to cache value: {}", e));
            }
            Ok(to_cache.await.expect("error should have been caught previously"))
        }, deps)
        ))
    }
    else {
        Err(Error::ValueError("no stored trait implemented for value".into()))
    }
});
