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
use crate::{depends, Dependencies, TypedValue, Value};
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
    fn next<'a>(
        &'a mut self,
        ctx: &'a crate::Context,
    ) -> futures::future::BoxFuture<'a, crate::Result<Option<Value>>>;
}

/// Implement the Generator trait for a type.
///
/// This will take care of defining the trait and boxing the future.
#[macro_export]
macro_rules! ImplGenerator {
    ( $t:ty => | $self:ident, $ctx:ident | $body:expr ) => {
        impl $crate::types::iter::Generator for $t {
            fn next<'a>(
                &'a mut $self,
                $ctx: &'a $crate::Context,
            ) -> $crate::future::BoxFuture<'a, $crate::Result<Option<$crate::Value>>> {
                $crate::future::FutureExt::boxed(async move { $body })
            }
        }
    };
}

#[derive(Clone)]
struct Streamed(SharedAsyncStream<futures::stream::BoxStream<'static, Value>>);

ImplGenerator!(Streamed => |self,_ctx| Ok(self.0.next().await));

impl From<&'_ Next> for Dependencies {
    fn from(n: &'_ Next) -> Self {
        depends![Next::ergo_type(), n.value, n.iter.next]
    }
}

impl From<Next> for TypedValue<Next> {
    fn from(n: Next) -> Self {
        Self::constant(n)
    }
}

impl Iter {
    /// Create a new Iter value with the given dependencies.
    pub fn new<G: Generator>(g: G, deps: Dependencies) -> TypedValue<Self> {
        TypedValue::constant_deps(Self::from_generator(g), depends![Self::ergo_type(), ^deps])
    }

    /// Create a new Iter value from an Iterator with the given dependencies.
    pub fn new_iter<I>(iter: I, deps: Dependencies) -> TypedValue<Self>
    where
        I: Iterator<Item = Value> + Send + Sync + 'static,
    {
        TypedValue::constant_deps(Self::from_iter(iter), depends![Self::ergo_type(), ^deps])
    }

    /// Create a new Iter value from a Stream with the given dependencies.
    pub fn new_stream<S>(stream: S, deps: Dependencies) -> TypedValue<Self>
    where
        S: Stream<Item = Value> + Send + 'static,
    {
        TypedValue::constant_deps(
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
            next: Value::dyn_new(
                |ctx| async move {
                    match generator.next(ctx).await {
                        Err(e) => e.into(),
                        Ok(None) => super::Unset.into(),
                        Ok(Some(value)) => Next {
                            value,
                            iter: Iter::from_generator(generator),
                        }
                        .into(),
                    }
                },
                depends![Next::ergo_type()],
            ),
        }
    }

    /// Get the next value from the Iter.
    ///
    /// An Err is returned if `next` evaluates to an Error or a type other than Next or Unset, or
    /// if a Next::iter evaluates to an Error or a type other than Iter.
    pub async fn next(&mut self, ctx: &crate::Context) -> crate::Result<Option<Value>> {
        let mut next = std::mem::replace(&mut self.next, super::Unset.into());
        ctx.eval(&mut next).await?;
        match_value! {next,
            super::Unset => Ok(None),
            Next { value, iter } => {
                self.next = iter.next;
                Ok(Some(value))
            }
            o => Err(traits::type_error(ctx, o, "Next or Unset"))
        }
    }

    /// Collect the iterator values.
    ///
    /// Repeatedly calls `next`; any Err that is returned by `next` will be propagated.
    pub async fn collect<C>(mut self, ctx: &crate::Context) -> crate::Result<C>
    where
        C: Default + Extend<Value>,
    {
        let mut ret = C::default();

        while let Some(value) = self.next(ctx).await? {
            ret.extend(std::iter::once(value));
        }
        Ok(ret)
    }
}

ergo_traits_fn! {
    impl traits::Display for Iter {
        async fn fmt(&self, f: &mut traits::Formatter) -> crate::error::RResult<()> {
            async move {
                let items: Vec<_> = self.clone().collect(CONTEXT).await?;
                let mut iter = items.into_iter();
                write!(f, "[")?;
                if let Some(v) = iter.next() {
                    traits::display(CONTEXT, v, f).await?;
                }
                for v in iter {
                    write!(f, ", ")?;
                    traits::display(CONTEXT, v, f).await?;
                }
                write!(f, "]")?;
                Ok(())
            }.await.into()
        }
    }

    impl traits::Nested for Iter {
        async fn nested(&self) -> RVec<Value> {
            self.clone().collect(CONTEXT).await.unwrap_or_default()
        }
    }

    impl traits::ValueByContent for Iter {
        async fn value_by_content(self, deep: bool) -> Value {
            let mut vals: Vec<_> = crate::try_result!(self.to_owned().collect(CONTEXT).await);
            if deep {
                CONTEXT.task.join_all(vals.iter_mut().map(|v| async move {
                    let old_v = std::mem::replace(v, super::Unset.into());
                    *v = traits::value_by_content(CONTEXT, old_v, deep).await;
                    Ok(())
                })).await.unwrap();
            }
            let deps = depends![^@vals];
            Iter::new_iter(vals.into_iter(), deps).into()
        }
    }

    impl traits::IntoTyped<super::Array> for Iter {
        async fn into_typed(self) -> Value {
            super::Array(crate::try_result!(self.to_owned().collect(CONTEXT).await)).into()
        }
    }

    impl traits::IntoTyped<super::Map> for Iter {
        async fn into_typed(self) -> Value {
            let mut vals: Vec<_> = crate::try_result!(self.to_owned().collect(CONTEXT).await);
            let mut ret = BstMap::default();
            let mut errs = vec![];
            let mut_vals = vals.iter_mut().collect::<Vec<_>>();
            drop(CONTEXT.eval_all(mut_vals).await);
            for v in vals {
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
                        errs.push(traits::type_error_for::<super::MapEntry>(CONTEXT, other));
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
            async move {
                let mut ids: Vec<u128> = Vec::new();
                let vals: Vec<_> = self.clone().collect(CONTEXT).await?;
                for v in vals {
                    ids.push(v.id());
                    stored_ctx.write_to_store(CONTEXT, v).await?;
                }
                bincode::serialize_into(item, &ids).map_err(|e| e.into())
            }.await.into()
        }

        async fn get(stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            async move {
                let ids: Vec<u128> = bincode::deserialize_from(item)?;
                let mut vals = Vec::new();
                for id in ids {
                    vals.push(Source::imbue(crate::Source::stored(stored_ctx.read_from_store(CONTEXT, id).await?)));
                }
                Ok(Erased::new(Iter::from_iter(vals.into_iter())))
            }.await.into()
        }
    }

    crate::ergo_type_name!(traits, Iter);
}
