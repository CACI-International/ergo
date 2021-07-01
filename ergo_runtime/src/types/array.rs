//! The Array type.

use crate as ergo_runtime;
use crate::abi_stable::{std_types::RVec, type_erase::Erased, StableAbi};
use crate::metadata::{Doc, Source};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue, Value};
use bincode;

/// Script array type.
#[derive(Clone, Debug, ErgoType, PartialEq, StableAbi)]
#[repr(C)]
pub struct Array(pub RVec<Value>);

impl From<&'_ Array> for Dependencies {
    fn from(a: &'_ Array) -> Self {
        depends![Array::ergo_type(), ^@a.0]
    }
}

impl From<Array> for TypedValue<Array> {
    fn from(v: Array) -> Self {
        let doc = format!("array with {} elements", v.0.len());
        let mut v = Self::constant(v);
        Doc::set_string(&mut v, doc);
        v
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
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::error::RResult<()> {
            async move {
                let mut iter = self.0.iter();
                write!(f, "[")?;
                if let Some(v) = iter.next() {
                    traits::display(CONTEXT, v.clone(), f).await?;
                }
                for v in iter {
                    write!(f, ", ")?;
                    traits::display(CONTEXT, v.clone(), f).await?;
                }
                write!(f, "]")?;
                Ok(())
            }.await.into()
        }
    }

    traits::IntoTyped::<super::Iter>::add_impl::<Array>(traits);

    traits::Nested::add_impl::<Array>(traits);

    impl traits::Stored for Array {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            async move {
                let mut ids: Vec<u128> = Vec::new();
                for v in self.0.iter().cloned() {
                    ids.push(v.id());
                    stored_ctx.write_to_store(CONTEXT, v).await?;
                }
                Ok(bincode::serialize_into(item, &ids)?)
            }.await.into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            async move {
                let ids: Vec<u128> = bincode::deserialize_from(item)?;
                let mut vals = RVec::new();
                for id in ids {
                    vals.push(Source::imbue(crate::Source::stored(stored_ctx.read_from_store(CONTEXT, id).await?)));
                }
                Ok(Erased::new(Array(vals)))
            }.await.into()
        }
    }

    impl traits::Bind for Array {
        async fn bind(&self, mut arg: Value) -> Value {
            let source = Source::get(&arg);

            crate::try_result!(CONTEXT.eval(&mut arg).await);

            crate::value::match_value! { arg,
                super::Index(ind) => {
                    // Return value at index
                    let ind = crate::try_result!(traits::into::<super::Number>(CONTEXT, ind).await);
                    source.with(match ind.as_ref().to_isize() {
                        None => return Source::get(&ind).with("non-integer index").into_error().into(),
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
                                .ok_or_else(|| format!("array has length {}", self.0.len()))
                        }
                    })
                    .transpose_err_with_context("while indexing array").into()
                },
                Array(arr) => {
                    crate::try_result!(traits::bind_array(
                            CONTEXT,
                            self.0.clone(),
                            source.with(arr.clone()),
                            |_,rest| Array(rest.into()).into(),
                    ).await);
                    super::Unit.into()
                }
                v => traits::bind_error(CONTEXT, v).into()
            }
        }
    }

    crate::ergo_type_name!(traits, Array);
    traits::ValueByContent::add_nested_impl::<Array>(traits);
}
