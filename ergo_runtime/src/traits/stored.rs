//! Stored grease trait, allowing values to be written to and read from the store.

use super::type_name;
use crate::{types, Result, ResultAbi};
use abi_stable::{std_types::RVec, StableAbi};
use bincode;
use grease::{
    bst::BstMap,
    path::PathBuf,
    runtime::{Item, ItemContent, Traits},
    traits::GreaseTrait,
    type_erase::{Eraseable, Erased, ErasedTrivial},
    types::{GreaseType, Type},
    value::Value,
};
use std::collections::BTreeMap;

/// Context passed to Stored traits.
#[derive(StableAbi)]
#[repr(C)]
pub struct StoredContext<'a> {
    pub traits: &'a Traits,
    pub value_type: &'a Type,
    store_item: Item,
}

/// Stored trait information.
#[derive(Clone, StableAbi, GreaseTrait)]
#[repr(C)]
pub struct Stored {
    put: extern "C" fn(&StoredContext, &Erased, ItemContent) -> ResultAbi<()>,
    get: extern "C" fn(&StoredContext, ItemContent) -> ResultAbi<Erased>,
}

impl StoredContext<'_> {
    /// Read the given value from the store.
    pub fn read_from_store(&self, id: u128) -> Result<Value> {
        read_from_store(&self.traits, &self.store_item, id)
    }

    /// Write the given value to the store.
    pub fn write_to_store(&self, v: Value) -> Result<()> {
        write_to_store(&self.traits, &self.store_item, v)
    }
}

impl Stored {
    pub fn add_impl<T: GreaseStored + GreaseType + Eraseable>(traits: &mut Traits) {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn put<T: GreaseStored>(
            ctx: &StoredContext,
            data: &Erased,
            content: ItemContent,
        ) -> ResultAbi<()> {
            unsafe { data.as_ref::<T>() }.put(ctx, content).into()
        }

        #[allow(improper_ctypes_definitions)]
        extern "C" fn get<T: GreaseStored + Eraseable>(
            ctx: &StoredContext,
            content: ItemContent,
        ) -> ResultAbi<Erased> {
            T::get(ctx, content).map(|v| Erased::new(v)).into()
        }

        traits.add_impl_for_type::<T, Stored>(Stored {
            put: put::<T>,
            get: get::<T>,
        });
    }

    pub fn put(
        &self,
        traits: &Traits,
        store_item: &Item,
        val: &Value,
        into: ItemContent,
    ) -> Result<()> {
        (self.put)(
            &self.context(traits, val.grease_type().as_ref(), store_item),
            val.forced_value().as_ref(),
            into,
        )
        .into()
    }

    pub fn get(
        &self,
        traits: &Traits,
        tp: &Type,
        store_item: &Item,
        from: ItemContent,
    ) -> Result<Erased> {
        (self.get)(&self.context(traits, tp, store_item), from).into()
    }

    fn context<'a>(
        &self,
        traits: &'a Traits,
        value_type: &'a Type,
        store_item: &Item,
    ) -> StoredContext<'a> {
        StoredContext {
            traits,
            value_type,
            store_item: store_item.clone(),
        }
    }
}

/// Rust trait for types implementing the grease Stored trait.
pub trait GreaseStored
where
    Self: Sized,
{
    fn put(&self, ctx: &StoredContext, into: ItemContent) -> Result<()>;
    fn get(ctx: &StoredContext, from: ItemContent) -> Result<Self>;
}

macro_rules! GreaseStoredSerde {
    ( $t:ty ) => {
        impl GreaseStored for $t {
            fn put(&self, _ctx: &StoredContext, into: ItemContent) -> Result<()> {
                Ok(bincode::serialize_into(into, self)?)
            }

            fn get(_ctx: &StoredContext, from: ItemContent) -> Result<Self> {
                Ok(bincode::deserialize_from(from)?)
            }
        }
    };
}

GreaseStoredSerde!(types::Unit);
GreaseStoredSerde!(types::String);

impl GreaseStored for PathBuf {
    fn put(&self, _ctx: &StoredContext, into: ItemContent) -> Result<()> {
        Ok(bincode::serialize_into(into, self.as_ref().as_ref())?)
    }

    fn get(_ctx: &StoredContext, from: ItemContent) -> Result<Self> {
        let p: std::path::PathBuf = bincode::deserialize_from(from)?;
        Ok(p.into())
    }
}

impl GreaseStored for types::Array {
    fn put(&self, ctx: &StoredContext, into: ItemContent) -> Result<()> {
        let mut ids: Vec<u128> = Vec::new();
        for v in self.0.iter().cloned() {
            ids.push(v.id());
            ctx.write_to_store(v)?;
        }
        Ok(bincode::serialize_into(into, &ids)?)
    }

    fn get(ctx: &StoredContext, from: ItemContent) -> Result<Self> {
        let ids: Vec<u128> = bincode::deserialize_from(from)?;
        let mut vals = RVec::new();
        for id in ids {
            vals.push(ctx.read_from_store(id)?);
        }
        Ok(types::Array(vals))
    }
}

impl GreaseStored for types::Map {
    fn put(&self, ctx: &StoredContext, into: ItemContent) -> Result<()> {
        let mut ids: BTreeMap<String, u128> = BTreeMap::new();
        for (k, v) in self.0.iter() {
            let v = v.clone();
            ids.insert(k.clone().into(), v.id());
            ctx.write_to_store(v)?;
        }
        Ok(bincode::serialize_into(into, &ids)?)
    }

    fn get(ctx: &StoredContext, from: ItemContent) -> Result<Self> {
        let ids: BTreeMap<String, u128> = bincode::deserialize_from(from)?;
        let mut vals = BstMap::new();
        for (k, id) in ids {
            vals.insert(k.into(), ctx.read_from_store(id)?);
        }
        Ok(types::Map(vals))
    }
}

impl GreaseStored for types::Either {
    fn put(&self, ctx: &StoredContext, mut into: ItemContent) -> Result<()> {
        bincode::serialize_into(&mut into, &self.index())?;
        ctx.write_to_store(self.value())?;
        Ok(bincode::serialize_into(into, &self.value().id())?)
    }

    fn get(ctx: &StoredContext, mut from: ItemContent) -> Result<Self> {
        let ind: usize = bincode::deserialize_from(&mut from)?;
        let id: u128 = bincode::deserialize_from(from)?;
        let val = ctx.read_from_store(id)?;
        Ok(Self::with_value(ind, val))
    }
}

/// Builtin traits implementations.
pub fn traits(traits: &mut Traits) {
    Stored::add_impl::<types::Unit>(traits);
    Stored::add_impl::<types::String>(traits);
    Stored::add_impl::<PathBuf>(traits);
    Stored::add_impl::<types::Array>(traits);
    Stored::add_impl::<types::Map>(traits);
    Stored::add_impl::<types::Either>(traits);
}

/// Read a Value from the store by id.
pub fn read_from_store(traits: &Traits, store_item: &Item, id: u128) -> Result<Value> {
    let item = store_item.value_id(id);
    let mut content = item.read_existing()?;
    let tp: Type = ErasedTrivial::deserialize(&mut content)?.into();
    if let Some(s) = traits.get_type::<Stored>(&tp) {
        let data = s.get(traits, &tp, store_item, content)?;
        // TODO revisit metadata
        Ok(Value::from_raw(
            tp.into(),
            std::sync::Arc::new(data),
            Default::default(),
            id,
        ))
    } else {
        Err(format!("no stored trait for {}", type_name(traits, &tp)).into())
    }
}

/// Write a value to the store.
///
/// The value must already be forced (using `force_value_nested`).
pub fn write_to_store(traits: &Traits, store_item: &Item, v: Value) -> Result<()> {
    if let Some(s) = traits.get::<Stored>(&v) {
        let item = store_item.value(&v);
        let mut content = item.write()?;

        let tp: ErasedTrivial = v.grease_type().as_ref().clone().into();
        tp.serialize(&mut content)?;
        s.put(traits, store_item, &v, content)
    } else {
        Err(format!(
            "no stored trait for {}",
            type_name(traits, &v.grease_type())
        )
        .into())
    }
}
