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
use std::mem::ManuallyDrop;

/// An iterator type.
///
/// Internally the type contains a single value which may evaluate to either Next, Unset, or Error.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
pub struct Iter {
    next: Value,
}

/// The next value from an Iter.
///
/// This has `iter` as a `ManuallyDrop` and has a `Drop` impl to avoid a stack overflow with large
/// iterators. The issue is that iterators (and values in general) are kept in memory when
/// evaluated/collected (usually), so that when the first item is dropped it causes a cascade of
/// drops of all of the cached values, which will be a huge stack for somewhat large iterators
/// (1000s+ of items). Thread local storage is used to drop the values in a loop rather than
/// recursively.
#[derive(Clone, Debug, ErgoType, StableAbi)]
#[repr(C)]
struct Next {
    pub value: Value,
    pub iter: ManuallyDrop<Iter>,
}

plugin_tls::thread_local! {
    // We use a Vec rather than simply Option<Value> because when dropping a `Next`, the `value`
    // may itself be another `Iter` which will possibly drop a second `Next` (so dropping a single
    // `Next` may produce more than one value to subsequently drop).
    static TO_DROP: std::cell::UnsafeCell<RVec<Value>> = std::cell::UnsafeCell::new(Default::default());
    static DROPPING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
}

impl Drop for Next {
    fn drop(&mut self) {
        use std::sync::atomic::Ordering::Relaxed;
        let do_drop = !DROPPING.with(|v| v.fetch_or(true, Relaxed));
        // Safety: self.iter is not used again after the `take`.
        let v = unsafe { ManuallyDrop::take(&mut self.iter) };
        // Safety: the mutable reference is only used here, by a single thread, and then discarded
        TO_DROP.with(|td| unsafe { td.get().as_mut() }.unwrap().push(v.next));

        if do_drop {
            // Safety: the mutable reference is only used here, by a single thread, and then discarded
            while let Some(v) = TO_DROP.with(|td| unsafe { td.get().as_mut() }.unwrap().pop()) {
                std::mem::drop(v);
            }
            DROPPING.with(|v| v.store(false, Relaxed));
        }
    }
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

#[derive(Clone)]
struct Map {
    iter: Iter,
    f: Value,
}

ergo_runtime::ImplGenerator!(Map => |self| {
    match self.iter.next().await? {
        None => Ok(None),
        Some(v) => Ok(Some(traits::bind(self.f.clone(), v).await))
    }
});

impl GetDependencies for Next {
    fn get_depends(&self) -> Dependencies {
        depends![Next::ergo_type(), self.value, self.iter.next]
    }
}

unsafe impl crate::value::InnerValues for Next {
    fn visit<'a, F: FnMut(&'a Value)>(&'a self, mut f: F) {
        f(&self.value);
        self.iter.visit(f);
    }
}

impl From<Next> for TypedValue<Next> {
    fn from(n: Next) -> Self {
        Self::new(n)
    }
}

unsafe impl crate::value::InnerValues for Iter {
    fn visit<'a, F: FnMut(&'a Value)>(&'a self, mut f: F) {
        f(&self.next);
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
            next: crate::lazy_value! {
                #![depends(const Next::ergo_type())]
                match generator.next().await {
                    Err(e) => e.into(),
                    Ok(None) => super::Unset.into(),
                    Ok(Some(value)) => Next {
                        value,
                        iter: ManuallyDrop::new(Iter::from_generator(generator)),
                    }
                    .into(),
                }
            },
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
            n@Next { .. } => {
                self.next = n.iter.next.clone();
                Ok(Some(n.value.clone()))
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
                    write!(f, "Iter[")?;
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

    impl traits::Functor for Iter {
        async fn map(self, f: Value) -> Value {
            let deps = depends![self, f];
            let iter = self.into_owned();
            Iter::new(Map { iter, f, }, deps).into()
        }
    }

    impl traits::IntoTyped<super::Array> for Iter {
        async fn into_typed(self) -> Value {
            super::Array(crate::try_result!(self.into_owned().collect().await)).into()
        }
    }

    impl traits::IntoTyped<super::Map> for Iter {
        async fn into_typed(self) -> Value {
            let mut vals: Vec<_> = crate::try_result!(self.into_owned().collect().await);
            let mut ret = BstMap::default();
            let mut errs = vec![];
            drop(crate::Context::eval_all(&mut vals).await);
            for v in vals {
                crate::value::match_value! { v,
                    super::MapEntry { key, value } => {
                        let key = key.as_evaluated().await;
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
        async fn put(&self, data: &mut traits::PutData<'_>) -> crate::RResult<()> {
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
                    let ids: Vec<u128> = bincode::deserialize_from(&mut data)?;
                    let values = crate::Context::global().task
                        .join_all(ids.into_iter().map(|id| data.read_value(id)))
                        .await?;
                    crate::Result::Ok(Erased::new(Iter::from_iter(values.into_iter())))
                }
            ).into()
        }
    }

    crate::ergo_type_name!(traits, Iter);
}
