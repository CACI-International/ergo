//! The Iter type.

use crate as ergo_runtime;
use crate::abi_stable::{
    bst::BstMap, std_types::RVec, stream::shared_async_stream::SharedAsyncStream,
    type_erase::Erased, StableAbi,
};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::value::match_value;
use crate::{depends, Dependencies, GetDependencies, TypedValue, Value};
use bincode;
use futures::stream::{Stream, StreamExt};

/// An iterator type.
///
/// Internally the type contains a single value which may evaluate to either Next, Unset, or Error.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Iter {
    next: Value,
}

/// The next value from an Iter.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
struct Next {
    pub value: Value,
    pub iter: Iter,
}

/// The trait used for arbitrary iterator generators.
pub trait Generator: Send + Sync + Clone + 'static {
    fn next<'a>(&'a mut self) -> futures::future::BoxFuture<'a, crate::Result<Option<Value>>>;
}

/// Implement the Generator trait for a type.
///
/// This will take care of defining the trait and boxing the future.
#[macro_export]
macro_rules! ImplGenerator {
    ( $t:ty => | $self:ident | $body:expr ) => {
        impl $crate::types::iter::Generator for $t {
            fn next<'a>(
                &'a mut $self,
            ) -> $crate::future::BoxFuture<'a, $crate::Result<Option<$crate::Value>>> {
                $crate::future::FutureExt::boxed(async move { $body })
            }
        }
    };
}

#[derive(Clone)]
struct Streamed(SharedAsyncStream<futures::stream::BoxStream<'static, Value>>);

ImplGenerator!(Streamed => |self| Ok(self.0.next().await));

impl GetDependencies for Next {
    fn get_depends(&self) -> Dependencies {
        depends![Next::ergo_type(), self.value, self.iter.next]
    }
}

impl From<Next> for TypedValue<Next> {
    fn from(n: Next) -> Self {
        Self::new(n)
    }
}

impl Iter {
    /// Create a new Iter value with the given dependencies.
    pub fn new<G: Generator>(g: G, deps: Dependencies) -> TypedValue<Self> {
        TypedValue::with_id(Self::from_generator(g), depends![Self::ergo_type(), ^deps])
    }

    /// Create a new Iter value from an Iterator with the given dependencies.
    pub fn new_iter<I>(iter: I, deps: Dependencies) -> TypedValue<Self>
    where
        I: Iterator<Item = Value> + Send + Sync + 'static,
    {
        TypedValue::with_id(Self::from_iter(iter), depends![Self::ergo_type(), ^deps])
    }

    /// Create a new Iter value from a Stream with the given dependencies.
    pub fn new_stream<S>(stream: S, deps: Dependencies) -> TypedValue<Self>
    where
        S: Stream<Item = Value> + Send + 'static,
    {
        TypedValue::with_id(
            Self::from_stream(stream),
            depends![Self::ergo_type(), ^deps],
        )
    }

    /// Create an Iter from an iterator.
    pub fn from_iter<I>(iter: I) -> Self
    where
        I: Iterator<Item = Value> + Send + Sync + 'static,
    {
        Self::from_stream(futures::stream::iter(iter))
    }

    /// Create an Iter from a stream.
    pub fn from_stream<S>(stream: S) -> Self
    where
        S: Stream<Item = Value> + Send + 'static,
    {
        let stream = SharedAsyncStream::new(stream.boxed());
        Self::from_generator(Streamed(SharedAsyncStream::new(stream.boxed())))
    }

    /// Create an Iter from a generator.
    pub fn from_generator<G: Generator>(mut generator: G) -> Self {
        Iter {
            next: Value::dynamic(
                move || async move {
                    match generator.next().await {
                        Err(e) => e.into(),
                        Ok(None) => super::Unset.into(),
                        Ok(Some(value)) => Next {
                            value,
                            iter: Iter::from_generator(generator),
                        }
                        .into(),
                    }
                },
                depends![Next::ergo_type()] as crate::dependency::DependenciesConstant,
            ),
        }
    }

    /// Get the next value from the Iter.
    ///
    /// An Err is returned if `next` evaluates to an Error or a type other than Next or Unset, or
    /// if a Next::iter evaluates to an Error or a type other than Iter.
    pub async fn next(&mut self) -> crate::Result<Option<Value>> {
        let mut next = std::mem::replace(&mut self.next, super::Unset.into());
        crate::Context::eval(&mut next).await?;
        match_value! {next,
            super::Unset => Ok(None),
            Next { value, iter } => {
                self.next = iter.next;
                Ok(Some(value))
            }
            o => Err(traits::type_error(o, "Next or Unset").into())
        }
    }

    /// Collect the iterator values.
    ///
    /// Repeatedly calls `next`; any Err that is returned by `next` will be propagated.
    pub async fn collect<C>(mut self) -> crate::Result<C>
    where
        C: Default + Extend<Value>,
    {
        let mut ret = C::default();

        while let Some(value) = self.next().await? {
            ret.extend(std::iter::once(value));
        }
        Ok(ret)
    }
}

ergo_traits_fn! {
    impl traits::Display for Iter {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while displaying this value"))
                ],
                async {
                    let items: Vec<_> = self.clone().collect().await?;
                    let mut iter = items.into_iter();
                    write!(f, "[")?;
                    if let Some(v) = iter.next() {
                        traits::display(v, f).await?;
                    }
                    for v in iter {
                        write!(f, ", ")?;
                        traits::display(v, f).await?;
                    }
                    write!(f, "]")?;
                    crate::Result::Ok(())
                }
            ).into()
        }
    }

    impl traits::Nested for Iter {
        async fn nested(&self) -> RVec<Value> {
            self.clone().collect().await.unwrap_or_default()
        }
    }

    impl traits::IntoTyped<super::Array> for Iter {
        async fn into_typed(self) -> Value {
            super::Array(crate::try_result!(self.to_owned().collect().await)).into()
        }
    }

    impl traits::IntoTyped<super::Map> for Iter {
        async fn into_typed(self) -> Value {
            let mut vals: Vec<_> = crate::try_result!(self.to_owned().collect().await);
            let mut ret = BstMap::default();
            let mut errs = vec![];
            let mut_vals = vals.iter_mut().collect::<Vec<_>>();
            drop(crate::Context::eval_all(mut_vals).await);
            for v in vals {
                crate::value::match_value! { v,
                    super::MapEntry { key, value } => {
                        let key = key.as_identified().await;
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
                        errs.push(traits::type_error_for::<super::MapEntry>(other).into());
                    }
                }
            }
            if errs.is_empty() {
                super::Map(ret).into()
            } else {
                crate::Error::aggregate(errs).into()
            }
        }
    }

    impl traits::Stored for Iter {
        async fn put(&self, stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while storing this value"))
                ],
                async {
                    let mut ids: Vec<u128> = Vec::new();
                    let vals: Vec<_> = self.clone().collect().await?;
                    let mut writes = Vec::new();
                    for v in vals {
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
                    crate::Result::Ok(Erased::new(Iter::from_iter(values.into_iter())))
                }
            ).into()
        }
    }

    crate::ergo_type_name!(traits, Iter);
}
