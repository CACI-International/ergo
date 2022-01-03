//! The Array type.

use crate as ergo_runtime;
use crate::abi_stable::{std_types::RVec, type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, GetDependencies, TypedValue, Value};
use bincode;

/// Script array type.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Array(pub RVec<Value>);

impl GetDependencies for Array {
    fn get_depends(&self) -> Dependencies {
        depends![Array::ergo_type(), ^@self.0]
    }
}

impl From<Array> for TypedValue<Array> {
    fn from(v: Array) -> Self {
        Self::new(v)
    }
}

impl From<Array> for super::Iter {
    fn from(v: Array) -> Self {
        super::Iter::from_iter(v.0.into_iter())
    }
}

impl traits::NestedValues for Array {
    fn nested_values(&self) -> Vec<&Value> {
        self.0.iter().collect()
    }
    fn nested_values_mut(&mut self) -> Vec<&mut Value> {
        self.0.iter_mut().collect()
    }
}

ergo_traits_fn! {
    impl traits::Display for Array {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while displaying this value"))
                ],
                async {
                    let mut iter = self.0.iter();
                    write!(f, "[")?;
                    if let Some(v) = iter.next() {
                        traits::display(v.clone(), f).await?;
                    }
                    for v in iter {
                        write!(f, ", ")?;
                        traits::display(v.clone(), f).await?;
                    }
                    write!(f, "]")?;
                    crate::Result::Ok(())
                }
            ).into()
        }
    }

    traits::IntoTyped::<super::Iter>::add_depending_impl::<Array>(traits);

    traits::Nested::add_impl::<Array>(traits);

    impl traits::Stored for Array {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while storing this value"))
                ],
                async {
                    let mut ids: Vec<u128> = Vec::new();
                    let mut writes = Vec::new();
                    for v in self.0.iter().cloned() {
                        ids.push(v.id().await);
                        writes.push(stored_ctx.write_to_store(v));
                    }
                    crate::Context::global().task.join_all(writes).await?;
                    bincode::serialize_into(item, &ids)
                }
            ).into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            crate::error_info!(
                async {
                    let ids: Vec<u128> = bincode::deserialize_from(item)?;
                    let values = crate::Context::global().task
                        .join_all(ids.into_iter().map(|id| stored_ctx.read_from_store(id)))
                        .await?;
                    crate::Result::Ok(Erased::new(Array(values.into())))
                }
            ).into()
        }
    }

    impl traits::Bind for Array {
        async fn bind(&self, mut arg: Value) -> Value {
            let source = Source::get(&arg);

            crate::try_result!(crate::Context::eval(&mut arg).await);

            crate::value::match_value! { arg,
                super::Index(ind) => {
                    // Return value at index
                    let ind = crate::try_result!(traits::into::<super::Number>(ind).await);
                    let indsrc = Source::get(&ind);
                    match ind.as_ref().to_isize() {
                        None => Err(crate::error! {
                            labels: [
                                primary(indsrc.with("")),
                                secondary(source.with("while indexing this array"))
                            ],
                            error: "non-integer index"
                        }),
                        Some(mut ind) => {
                            // Negative indices are relative to the end.
                            if ind < 0 {
                                ind += self.0.len() as isize;
                                if ind < 0 {
                                    // use length to ensure access will fail
                                    ind = self.0.len() as isize;
                                }
                            }
                            let ind = ind as usize;
                            self.0.get(ind).map(|v| v.clone())
                                .ok_or_else(|| crate::error! {
                                    labels: [
                                        primary(indsrc.with("")),
                                        secondary(source.with("while indexing this array"))
                                    ],
                                    notes: [
                                        format!("array has length {}", self.0.len())
                                    ],
                                    error: "index out of bounds"
                                })
                        }
                    }.into()
                },
                Array(arr) => {
                    crate::try_result!(traits::bind_array(
                            self.0.clone(),
                            source.with(arr.clone()),
                            |_,rest| Array(rest.into()).into(),
                    ).await);
                    super::Unit.into()
                }
                v => traits::bind_error(v).into()
            }
        }
    }

    crate::ergo_type_name!(traits, Array);
    traits::ValueByContent::add_nested_impl::<Array>(traits);
}
