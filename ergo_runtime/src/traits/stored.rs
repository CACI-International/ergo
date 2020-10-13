//! Stored grease trait, allowing values to be written to and read from the store.

use super::type_name;
use crate::{types, Result, ResultAbi};
use abi_stable::{
    std_types::{ROption, RVec},
    StableAbi,
};
use bincode;
use futures::future::FutureExt;
use grease::{
    bst::BstMap,
    future::BoxFuture,
    path::PathBuf,
    runtime::{Context, Item, ItemContent, Traits},
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
    pub ctx: &'a Context,
    pub value_type: &'a Type,
    store_item: Item,
}

/// Stored trait information.
#[derive(Clone, StableAbi, GreaseTrait)]
#[repr(C)]
pub struct Stored {
    put: for<'a> extern "C" fn(
        &'a StoredContext,
        &'a Erased,
        ItemContent,
    ) -> BoxFuture<'a, ResultAbi<()>>,
    get: extern "C" fn(&StoredContext, ItemContent) -> ResultAbi<Erased>,
}

impl StoredContext<'_> {
    /// Read the given value from the store.
    pub fn read_from_store(&self, id: u128) -> Result<Value> {
        read_from_store(&self.ctx, &self.store_item, id)
    }

    /// Write the given value to the store.
    pub async fn write_to_store(&self, v: Value) -> Result<()> {
        write_to_store(&self.ctx, &self.store_item, v).await
    }
}

impl Stored {
    pub fn add_impl<T: GreaseStored + GreaseType + Eraseable>(traits: &mut Traits) {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn put<'a, T: GreaseStored + Sync>(
            ctx: &'a StoredContext,
            data: &'a Erased,
            content: ItemContent,
        ) -> BoxFuture<'a, ResultAbi<()>> {
            BoxFuture::new(
                async move { unsafe { data.as_ref::<T>() }.put(ctx, content).await.into() },
            )
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

    pub async fn put(
        &self,
        ctx: &Context,
        store_item: &Item,
        val: &Value,
        into: ItemContent,
    ) -> Result<()> {
        (self.put)(
            &self.context(ctx, val.grease_type().as_ref(), store_item),
            val.forced_value().as_ref(),
            into,
        )
        .await
        .into()
    }

    pub fn get(
        &self,
        ctx: &Context,
        tp: &Type,
        store_item: &Item,
        from: ItemContent,
    ) -> Result<Erased> {
        (self.get)(&self.context(ctx, tp, store_item), from).into()
    }

    fn context<'a>(
        &self,
        ctx: &'a Context,
        value_type: &'a Type,
        store_item: &Item,
    ) -> StoredContext<'a> {
        StoredContext {
            ctx,
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
    fn put<'a>(
        &'a self,
        ctx: &'a StoredContext,
        into: ItemContent,
    ) -> futures::future::BoxFuture<'a, Result<()>>;

    fn get(ctx: &StoredContext, from: ItemContent) -> Result<Self>;
}

macro_rules! GreaseStoredSerde {
    ( $t:ty ) => {
        impl GreaseStored for $t {
            fn put<'a>(
                &'a self,
                _ctx: &'a StoredContext,
                into: ItemContent,
            ) -> futures::future::BoxFuture<Result<()>> {
                async move { Ok(bincode::serialize_into(into, self)?) }.boxed()
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
    fn put<'a>(
        &'a self,
        _ctx: &'a StoredContext,
        into: ItemContent,
    ) -> futures::future::BoxFuture<'a, Result<()>> {
        async move { Ok(bincode::serialize_into(into, self.as_ref().as_ref())?) }.boxed()
    }

    fn get(_ctx: &StoredContext, from: ItemContent) -> Result<Self> {
        let p: std::path::PathBuf = bincode::deserialize_from(from)?;
        Ok(p.into())
    }
}

impl GreaseStored for types::Array {
    fn put<'a>(
        &'a self,
        ctx: &'a StoredContext,
        into: ItemContent,
    ) -> futures::future::BoxFuture<'a, Result<()>> {
        async move {
            let mut ids: Vec<u128> = Vec::new();
            for v in self.0.iter().cloned() {
                ids.push(v.id());
                ctx.write_to_store(v).await?;
            }
            Ok(bincode::serialize_into(into, &ids)?)
        }
        .boxed()
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
    fn put<'a>(
        &'a self,
        ctx: &'a StoredContext,
        into: ItemContent,
    ) -> futures::future::BoxFuture<'a, Result<()>> {
        async move {
            let mut ids: BTreeMap<String, u128> = BTreeMap::new();
            for (k, v) in self.0.iter() {
                let v = v.clone();
                ids.insert(k.clone().into(), v.id());
                ctx.write_to_store(v).await?;
            }
            Ok(bincode::serialize_into(into, &ids)?)
        }
        .boxed()
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
    fn put<'a>(
        &'a self,
        ctx: &'a StoredContext,
        mut into: ItemContent,
    ) -> futures::future::BoxFuture<'a, Result<()>> {
        async move {
            bincode::serialize_into(&mut into, &self.index())?;
            ctx.write_to_store(self.value()).await?;
            Ok(bincode::serialize_into(into, &self.value().id())?)
        }
        .boxed()
    }

    fn get(ctx: &StoredContext, mut from: ItemContent) -> Result<Self> {
        let ind: usize = bincode::deserialize_from(&mut from)?;
        let id: u128 = bincode::deserialize_from(from)?;
        let val = ctx.read_from_store(id)?;
        Ok(Self::with_value(ind, val))
    }
}

macro_rules! try_abi {
    ( $e:expr ) => {
        match $e {
            Ok(v) => v,
            Err(e) => return Err(e.into()).into(),
        }
    };
}

/// Builtin traits implementations.
pub fn traits(traits: &mut Traits) {
    Stored::add_impl::<types::Unit>(traits);
    Stored::add_impl::<types::String>(traits);
    Stored::add_impl::<PathBuf>(traits);
    Stored::add_impl::<types::Array>(traits);
    Stored::add_impl::<types::Map>(traits);

    // types::Either
    {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn put<'a>(
            ctx: &'a StoredContext,
            data: &'a Erased,
            mut into: ItemContent,
        ) -> BoxFuture<'a, ResultAbi<()>> {
            BoxFuture::new(async move {
                let either = unsafe { data.as_ref::<types::Either>() };
                try_abi!(bincode::serialize_into(&mut into, &either.index()));
                try_abi!(ctx.write_to_store(either.value()).await);
                Ok(try_abi!(bincode::serialize_into(
                    into,
                    &either.value().id()
                )))
                .into()
            })
        }

        #[allow(improper_ctypes_definitions)]
        extern "C" fn get(ctx: &StoredContext, mut from: ItemContent) -> ResultAbi<Erased> {
            let ind: usize = try_abi!(bincode::deserialize_from(&mut from));
            let id: u128 = try_abi!(bincode::deserialize_from(from));
            let val = try_abi!(ctx.read_from_store(id));
            Ok(Erased::new(types::Either::with_value(ind, val))).into()
        }

        traits.add_generator_by_trait_for_trait(|_traits, tp| {
            if !types::Either::matches_grease_type(tp) {
                return ROption::RNone;
            }

            ROption::RSome(Stored { put, get })
        });
    }
}

/// Read a Value from the store by id.
pub fn read_from_store(ctx: &Context, store_item: &Item, id: u128) -> Result<Value> {
    let item = store_item.value_id(id);
    let mut content = item.read_existing()?;
    let tp: Type = ErasedTrivial::deserialize(&mut content)?.into();
    if let Some(s) = ctx.traits.get_type::<Stored>(&tp) {
        let data = s.get(ctx, &tp, store_item, content)?;
        // TODO revisit metadata
        Ok(Value::from_raw(
            tp.into(),
            std::sync::Arc::new(data),
            Default::default(),
            id,
        ))
    } else {
        Err(format!("no stored trait for {}", type_name(&ctx.traits, &tp)).into())
    }
}

/// Write a value to the store.
///
/// The value must already be forced (using `force_value_nested`).
pub async fn write_to_store(ctx: &Context, store_item: &Item, v: Value) -> Result<()> {
    if let Some(s) = ctx.traits.get::<Stored>(&v) {
        let item = store_item.value(&v);
        let mut content = item.write()?;

        let tp: ErasedTrivial = v.grease_type().as_ref().clone().into();
        tp.serialize(&mut content)?;
        s.put(ctx, store_item, &v, content).await
    } else {
        Err(format!(
            "no stored trait for {}",
            type_name(&ctx.traits, &v.grease_type())
        )
        .into())
    }
}
