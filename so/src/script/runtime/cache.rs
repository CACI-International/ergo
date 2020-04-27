//! Cache arbitrary values.

use super::{apply_value, builtin_function_prelude::*};
use grease::{
    depends, item_name, type_uuid, GetValueType, Item, SplitInto, Trait, TraitImpl, TraitType,
    ValueData, ValueType,
};

pub struct StoredTrait {
    put: fn(&ValueData, &Item) -> std::io::Result<()>,
    get: fn(&Item) -> std::io::Result<ValueData>,
}

impl Trait<'_> for StoredTrait {
    type Impl = StoredTrait;

    fn trait_type() -> TraitType {
        TraitType::new(type_uuid(b"so::script::runtime::cache::StoredTrait"))
    }

    fn create(imp: &'_ Self::Impl) -> Self {
        StoredTrait {
            put: imp.put,
            get: imp.get,
        }
    }
}

impl StoredTrait {
    pub fn put(&self, val: &ValueData, into: &Item) -> std::io::Result<()> {
        (self.put)(val, into)
    }

    pub fn get(&self, from: &Item) -> std::io::Result<ValueData> {
        (self.get)(from)
    }
}

pub type Stored<'a> = grease::TraitRef<'a, StoredTrait>;

impl From<StoredTrait> for TraitImpl {
    fn from(v: StoredTrait) -> Self {
        TraitImpl::new(StoredTrait::trait_type(), v)
    }
}

pub trait StoredValue
where
    Self: Sized,
{
    fn put(&self, into: &Item) -> std::io::Result<()>;
    fn get(from: &Item) -> std::io::Result<Self>;
}

fn put_data<T: StoredValue + Sync>(data: &ValueData, into: &Item) -> std::io::Result<()> {
    unsafe { data.as_ref::<T>() }.put(into)
}

fn get_data<T: StoredValue>(from: &Item) -> std::io::Result<ValueData> {
    T::get(from).map(ValueData::new)
}

pub fn impl_stored<T: StoredValue + Sync>() -> TraitImpl {
    TraitImpl::for_trait::<Stored<'_>>(StoredTrait {
        put: put_data::<T>,
        get: get_data::<T>,
    })
}

impl StoredValue for String {
    fn put(&self, into: &Item) -> std::io::Result<()> {
        use std::io::Write;
        into.write().and_then(|mut v| v.write_all(self.as_bytes()))
    }

    fn get(from: &Item) -> std::io::Result<Self> {
        use std::io::Read;
        from.open(std::fs::OpenOptions::new().read(true))
            .and_then(|mut v| {
                let mut s = String::new();
                v.read_to_string(&mut s).map(move |_| s)
            })
    }
}

impl StoredValue for std::path::PathBuf {
    fn put(&self, into: &Item) -> std::io::Result<()> {
        self.to_string_lossy().into_owned().put(into)
    }

    fn get(from: &Item) -> std::io::Result<Self> {
        String::get(from).map(Into::into)
    }
}

pub fn trait_generator(v: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    if *v == String::value_type() {
        vec![impl_stored::<String>()]
    } else if *v == std::path::PathBuf::value_type() {
        vec![impl_stored::<std::path::PathBuf>()]
    } else {
        vec![]
    }
}

def_builtin!(ctx => {
    let v = ctx.args.next().ok_or("no argument to cache")?;
    let args = std::mem::take(&mut ctx.args);

    let store = ctx.store.item(item_name!("cache"));

    let to_cache = match ctx.split_map(move |ctx| apply_value(ctx, v, args.unchecked(), true))? {
        Eval::Error => return Ok(Eval::Error),
        Eval::Value(v) => v
    };

    if let Some(s) = ctx.traits.get::<StoredTrait>(&to_cache) {
        let deps = depends![to_cache];
        Ok(Eval::Value(Value::new(to_cache.value_type(), async move {
            let item = store.value(&to_cache);
            match s.get(&item) {
                Ok(data) => {
                    Ok(data.into())
                }
                Err(_) => {
                    let v = to_cache.await?;
                    s.put(v.as_ref(), &item).map_err(|v| v.to_string())?;
                    Ok(v)
                }
            }
        }, deps
        )))
    }
    else {
        Err(Error::ValueError("no way to store value".into()))
    }
});
