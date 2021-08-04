//! The Map type.

use crate as ergo_runtime;
use crate::abi_stable::{bst::BstMap, type_erase::Erased, StableAbi};
use crate::metadata::{Doc, Source};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue, Value};
use bincode;
use std::collections::BTreeMap;

/// Script map type.
#[derive(Clone, Debug, ErgoType, PartialEq, StableAbi)]
#[repr(C)]
pub struct Map(pub BstMap<Value, Value>);

impl From<&'_ Map> for Dependencies {
    fn from(m: &'_ Map) -> Self {
        depends![Map::ergo_type(), ^m.0.iter().map(|(k, v)| depends![k, v])]
    }
}

impl From<Map> for TypedValue<Map> {
    fn from(m: Map) -> Self {
        let doc = format!("map with {} entries", m.0.len());
        let mut v = Self::constant(m);
        Doc::set_string(&mut v, doc);
        v
    }
}

impl From<Map> for super::Iter {
    fn from(v: Map) -> Self {
        super::Iter::from_iter(v.0.into_iter().map(|(key, value)| {
            Source::imbue(Source::get(&key).with(super::MapEntry { key, value }.into()))
        }))
    }
}

impl traits::NestedValues for Map {
    fn nested_values(&self) -> Vec<&Value> {
        self.0.iter().map(|(k, v)| vec![k, v]).flatten().collect()
    }
    fn nested_values_mut(&mut self) -> Vec<&mut Value> {
        self.0
            .iter_mut()
            .map(|(k, v)| vec![k, v])
            .flatten()
            .collect()
    }
}

ergo_traits_fn! {
    impl traits::Display for Map {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::error::RResult<()> {
            async move {
                let mut iter = self.0.iter();
                write!(f, "{{")?;
                if let Some((k,v)) = iter.next() {
                    traits::display(k.clone(), f).await?;
                    write!(f, " = ")?;
                    traits::display(v.clone(), f).await?;
                }

                for (k,v) in iter {
                    write!(f, ", ")?;
                    traits::display(k.clone(), f).await?;
                    write!(f, " = ")?;
                    traits::display(v.clone(), f).await?;
                }
                write!(f, "}}")?;
                Ok(())
            }.await.into()
        }
    }

    traits::IntoTyped::<super::Iter>::add_depending_impl::<Map>(traits);

    traits::Nested::add_impl::<Map>(traits);

    impl traits::Stored for Map {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            async move {
                let mut ids: BTreeMap<u128, u128> = BTreeMap::new();
                let mut writes = Vec::new();
                for (k, v) in self.0.iter() {
                    let k = k.clone();
                    let v = v.clone();
                    ids.insert(k.id(), v.id());
                    writes.push(stored_ctx.write_to_store(k));
                    writes.push(stored_ctx.write_to_store(v));
                }
                crate::Context::global().task.join_all(writes).await?;
                Ok(bincode::serialize_into(item, &ids)?)
            }.await.into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            async move {
                let ids: BTreeMap<u128, u128> = bincode::deserialize_from(item)?;
                let mut vals = BstMap::new();
                let keys: Vec<_> = ids.iter().map(|i| stored_ctx.read_from_store(*i.0)).collect();
                let values: Vec<_> = ids.iter().map(|i| stored_ctx.read_from_store(*i.1)).collect();

                let read = crate::Context::global().task.join_all(vec![
                    crate::Context::global().task.join_all(keys),
                    crate::Context::global().task.join_all(values)
                ]).await?;
                let mut read = read.into_iter();
                let keys = read.next().unwrap();
                let values = read.next().unwrap();

                for (k, v) in keys.into_iter().zip(values) {
                    vals.insert(
                        Source::imbue(crate::Source::stored(k)),
                        Source::imbue(crate::Source::stored(v)),
                    );
                }
                Ok(Erased::new(Map(vals)))
            }.await.into()
        }
    }

    impl traits::Bind for Map {
        async fn bind(&self, mut arg: Value) -> Value {
            let source = Source::get(&arg);

            crate::try_result!(crate::Context::eval(&mut arg).await);

            crate::value::match_value! { arg,
                super::Index(index) => {
                    self.0.get(&index).cloned().unwrap_or(super::Unset.into())
                },
                Map(map) => {
                    crate::try_result!(traits::bind_map(self.0.clone(), source.with(map.clone()), false).await);
                    super::Unit.into()
                },
                v => traits::bind_error(v).into()
            }
        }
    }

    crate::ergo_type_name!(traits, Map);
    traits::ValueByContent::add_nested_impl::<Map>(traits);
}
