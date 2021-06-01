//! The Iter type.

use crate as ergo_runtime;
use crate::abi_stable::{
    bst::BstMap, future::stream::BoxStream, std_types::RVec,
    stream::shared_async_stream::SharedAsyncStream, type_erase::Erased, StableAbi,
};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::value::IntoValue;
use crate::{depends, Dependencies, Source, TypedValue, Value};
use bincode;
use futures::stream::Stream;
use std::pin::Pin;
use std::task;

/// An iterator type.
///
/// Note that iterator types are meant to be fully consumed wherever they are used. Changing one
/// partially and creating a new Value may not correctly capture dependencies; for a partial
/// consumption case, use `Stream`.
///
/// You may notice that Iter itself implements Stream rather than Iterator: this is to make Iter
/// useful (otherwise you can't really do meaningful manipulations of the contents), while from a
/// script perspective it still seems like an iterator (since in scripts everything is async).
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Iter(SharedAsyncStream<BoxStream<'static, Source<Value>>>);

impl Stream for Iter {
    type Item = Source<Value>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<Option<Self::Item>> {
        unsafe { self.map_unchecked_mut(|v| &mut v.0) }
            .poll_next(cx)
            .map(|opt| opt.map(|res| res.into()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl Iter {
    /// Create a new Iter value with the given dependencies.
    pub fn new<I>(iter: I, deps: Dependencies) -> TypedValue<Self>
    where
        I: Iterator<Item = Source<Value>> + Send + Sync + 'static,
    {
        TypedValue::constant_deps(Self::from_iter(iter), deps)
    }

    /// Create a new Iter value from a Stream with the given dependencies.
    pub fn new_stream<S>(stream: S, deps: Dependencies) -> TypedValue<Self>
    where
        S: Stream<Item = Source<Value>> + Send + 'static,
    {
        TypedValue::constant_deps(Self::from_stream(stream), deps)
    }

    /// Create an Iter from an iterator.
    pub fn from_iter<I>(iter: I) -> Self
    where
        I: Iterator<Item = Source<Value>> + Send + Sync + 'static,
    {
        Self::from_stream(futures::stream::iter(iter))
    }

    /// Create an Iter from a stream.
    pub fn from_stream<S>(stream: S) -> Self
    where
        S: Stream<Item = Source<Value>> + Send + 'static,
    {
        Iter(SharedAsyncStream::new(BoxStream::new(stream)))
    }

    /// Collect the iterator values.
    pub async fn collect<C>(self) -> C
    where
        C: Default + Extend<Source<Value>>,
    {
        futures::stream::StreamExt::collect(self).await
    }
}

ergo_traits_fn! {
    impl traits::Nested for Iter {
        async fn nested(&self) -> RVec<Value> {
            self.clone().collect::<Vec<_>>().await.into_iter().map(Source::unwrap).collect()
        }
    }

    impl traits::ValueByContent for Iter {
        async fn value_by_content(self, deep: bool) -> Value {
            let mut vals: Vec<_> = self.to_owned().collect().await;
            if deep {
                let mut_vals = vals.iter_mut().map(|src_v| &mut **src_v).collect::<Vec<_>>();
                let mut mut_vals = mut_vals.into_iter();
                crate::try_result!(CONTEXT.eval_all(mut_vals.by_ref()).await);
                for v in mut_vals {
                    let old_v = std::mem::replace(v, super::Unset.into());
                    *v = traits::value_by_content(CONTEXT, old_v, deep).await;
                }
            }
            let deps = depends![^@vals];
            Iter::new(vals.into_iter(), deps).into()
        }
    }

    impl traits::IntoTyped<super::Array> for Iter {
        async fn into_typed(self) -> Value {
            super::Array(self.to_owned().collect().await).into_value()
        }
    }

    impl traits::IntoTyped<super::Map> for Iter {
        async fn into_typed(self) -> Value {
            let mut vals: Vec<_> = self.to_owned().collect().await;
            let mut ret = BstMap::default();
            let mut errs = vec![];
            let mut_vals = vals.iter_mut().map(|src_v| &mut **src_v).collect::<Vec<_>>();
            drop(CONTEXT.eval_all(mut_vals).await);
            for v in vals {
                let (source, v) = v.take();
                crate::value::match_value! { v,
                    super::MapEntry { key, value } => {
                        // Remove Unset values.
                        if value.is_type::<super::Unset>() {
                            ret.remove(&key);
                        } else {
                            ret.insert(key, value);
                        }
                    }
                    e @ super::Error{..} => {
                        errs.push(e);
                    }
                    other => {
                        errs.push(traits::type_error_for::<super::MapEntry>(CONTEXT, source.with(other)));
                    }
                }
            }
            if errs.is_empty() {
                super::Map(ret).into_value()
            } else {
                crate::Error::aggregate(errs).into()
            }
        }
    }

    impl traits::Stored for Iter {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            async move {
                let mut ids: Vec<u128> = Vec::new();
                let vals: Vec<_> = self.clone().collect().await;
                for v in vals {
                    ids.push(v.id());
                    stored_ctx.write_to_store(CONTEXT, v.unwrap()).await?;
                }
                bincode::serialize_into(item, &ids).map_err(|e| e.into())
            }.await.into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            async move {
                let ids: Vec<u128> = bincode::deserialize_from(item)?;
                let mut vals = Vec::new();
                for id in ids {
                    vals.push(Source::stored(stored_ctx.read_from_store(CONTEXT, id).await?));
                }
                Ok(Erased::new(Iter::from_iter(vals.into_iter())))
            }.await.into()
        }
    }

    crate::ergo_type_name!(traits, Iter);
}
