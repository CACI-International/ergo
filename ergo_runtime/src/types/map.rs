//! The Map type.

use crate as ergo_runtime;
use crate::abi_stable::{bst::BstMap, type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{EvaluatedValue, TypedValue, Value};
use bincode;
use std::collections::BTreeMap;

/// Script map type.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Map(pub BstMap<EvaluatedValue, Value>);

unsafe impl crate::value::InnerValues for Map {
    fn visit_info<'a, F: FnMut(crate::value::VisitInfo<'a>)>(&'a self, mut f: F) {
        for (k, v) in &self.0 {
            f(crate::value::VisitInfo::Immutable(k));
            f(crate::value::VisitInfo::Value(v));
        }
    }
}

impl From<Map> for TypedValue<Map> {
    fn from(m: Map) -> Self {
        Self::new(m)
    }
}

impl From<Map> for super::Iter {
    fn from(v: Map) -> Self {
        super::Iter::from_iter(v.0.into_iter().map(|(key, value)| {
            Source::imbue(
                Source::get(&key).with(
                    super::MapEntry {
                        key: key.into(),
                        value,
                    }
                    .into(),
                ),
            )
        }))
    }
}

impl traits::NestedValues for Map {
    fn nested_values(&self) -> Vec<&Value> {
        self.0
            .iter()
            .map(|(k, v)| vec![&**k, v])
            .flatten()
            .collect()
    }

    // Skips keys, as they cannot be safely mutated.
    fn nested_values_mut(&mut self) -> Vec<&mut Value> {
        self.0.iter_mut().map(|(_, v)| v).collect()
    }
}

ergo_traits_fn! {
    impl traits::Display for Map {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while displaying this value"))
                ],
                async {
                    let mut iter = self.0.iter();
                    write!(f, "{{")?;
                    if let Some((k,v)) = iter.next() {
                        traits::display(k.clone().into(), f).await?;
                        write!(f, " = ")?;
                        traits::display(v.clone(), f).await?;
                    }

                    for (k,v) in iter {
                        write!(f, ", ")?;
                        traits::display(k.clone().into(), f).await?;
                        write!(f, " = ")?;
                        traits::display(v.clone(), f).await?;
                    }
                    write!(f, "}}")?;
                    crate::Result::Ok(())
                }
            ).into()
        }
    }

    traits::IntoTyped::<super::Iter>::add_depending_impl::<Map>(traits);

    traits::Nested::add_impl::<Map>(traits);
    traits::Functor::add_nested_impl::<Map>(traits);

    impl traits::Stored for Map {
        async fn put(&self, data: &mut traits::PutData<'_>) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while storing this value"))
                ],
                async {
                    let mut ids: BTreeMap<u128, u128> = BTreeMap::new();
                    let mut writes = Vec::new();
                    for (k, v) in self.0.iter() {
                        let k = k.clone();
                        let v = v.clone();
                        ids.insert(*k.id(), v.id().await);
                        writes.push(data.write_value(k.into()));
                        writes.push(data.write_value(v));
                    }
                    crate::Context::global().task.join_all(writes).await?;
                    bincode::serialize_into(data, &ids)
                }
            ).into()
        }

        async fn get(mut data: &mut traits::GetData<'_>) -> crate::RResult<Erased> {
            crate::error_info!(
                async {
                    let ids: BTreeMap<u128, u128> = bincode::deserialize_from(&mut data)?;
                    let mut vals = BstMap::new();
                    let keys: Vec<_> = ids.iter().map(|i| data.read_value(*i.0)).collect();
                    let values: Vec<_> = ids.iter().map(|i| data.read_value(*i.1)).collect();

                    let read = crate::Context::global().task.join_all(vec![
                        crate::Context::global().task.join_all(keys),
                        crate::Context::global().task.join_all(values)
                    ]).await?;
                    let mut read = read.into_iter();
                    let keys = read.next().unwrap();
                    let values = read.next().unwrap();

                    for (k, v) in keys.into_iter().zip(values) {
                        vals.insert(k.as_evaluated().await, v);
                    }
                    crate::Result::Ok(Erased::new(Map(vals)))
                }
            ).into()
        }
    }

    impl traits::Bind for Map {
        async fn bind(&self, mut arg: Value) -> Value {
            crate::try_result!(crate::Context::eval(&mut arg).await);

            crate::value::match_value! { arg.clone(),
                super::Index(index) => {
                    self.0.get(&index.as_evaluated().await).cloned().unwrap_or(super::Unset.into())
                },
                Map(map) => {
                    crate::try_result!(traits::bind_map(self.0.clone(), SELF_VALUE, map, &arg, false).await);
                    super::Unit.into()
                },
                v => traits::bind_error(v).into()
            }
        }
    }

    crate::ergo_type_name!(traits, Map);
}
