//! Synchronization functions.

use ergo_runtime::abi_stable::{
    external_types::RMutex, future::RawWaker, std_types::RVec, StableAbi,
};
use ergo_runtime::{
    error::DiagnosticInfo, metadata::Source, traits, type_system::ErgoType, types, Context, Value,
};

pub fn module() -> Value {
    crate::make_string_map! {
        "resource" = resource(),
        "consume" = consume()
    }
}

#[derive(ErgoType, StableAbi)]
#[repr(C)]
struct SyncResource {
    inner: RMutex<SyncResourceInner>,
    max: usize,
}

#[derive(StableAbi)]
#[repr(C)]
struct SyncResourceInner {
    count: usize,
    pending: RVec<RawWaker>,
}

impl SyncResource {
    pub fn new(count: usize) -> Self {
        SyncResource {
            inner: RMutex::new(SyncResourceInner {
                count,
                pending: Default::default(),
            }),
            max: count,
        }
    }
}

struct Acquire<'a>(&'a SyncResource, usize);

struct AcquireGuard<'a>(&'a SyncResource, usize);

impl<'a> std::future::Future for Acquire<'a> {
    type Output = AcquireGuard<'a>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        let mut guard = self.0.inner.lock();
        if guard.count >= self.1 {
            guard.count -= self.1;
            drop(guard);
            std::task::Poll::Ready(AcquireGuard(self.0, self.1))
        } else {
            guard.pending.push(cx.into());
            std::task::Poll::Pending
        }
    }
}

impl<'a> Drop for AcquireGuard<'a> {
    fn drop(&mut self) {
        let mut guard = self.0.inner.lock();
        guard.count += self.1;
        let pending = std::mem::take(&mut guard.pending);
        drop(guard);
        for waker in pending {
            waker.into_waker().wake()
        }
    }
}

impl SyncResource {
    pub fn acquire(&self, count: usize) -> Acquire {
        let count = std::cmp::min(count, self.max);
        Acquire(self, count)
    }
}

#[types::ergo_fn]
/// Create a limited resource.
///
/// Arguments: `:id (Into<Number> :count)`
///
/// Returns a SyncResource with the given count.
async fn resource(id: _, count: _) -> Value {
    let id = id.id();
    let count = traits::into::<types::Number>(count).await?;

    let count = count
        .as_ref()
        .to_usize()
        .add_primary_label(Source::get(&count).with("expected this to be unsigned integer"))?;

    Value::constant_deps(SyncResource::new(count), ergo_runtime::depends![id])
}

#[types::ergo_fn]
/// Consume a limited resource while evaluating a value.
///
/// Arguments: `(SyncResource :resource) :value`
///
/// Keyed Arguments:
/// * `Into<Number> count` - the count of the resource to consume (default 1)
///
/// Returns the result of evaluating `value`.
async fn consume(resource: SyncResource, mut value: _, (count): [_]) -> Value {
    let count = match count {
        None => 1,
        Some(v) => {
            let v = traits::into::<types::Number>(v).await?;
            v.as_ref()
                .to_usize()
                .add_primary_label(Source::get(&v).with("expected this to be unsigned integer"))?
        }
    };

    let _guard = resource.as_ref().acquire(count).await;
    drop(Context::eval(&mut value).await);
    value
}

ergo_runtime::type_system::ergo_traits_fn! {
    ergo_runtime::ergo_type_name!(traits, SyncResource);
}
