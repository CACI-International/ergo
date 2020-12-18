//! Task manager.

// For generated sabi trait code with Closure<(), ...>.
#![allow(improper_ctypes_definitions)]

use crate::bst::BstMap;
use crate::closure::ClosureOnce;
use crate::future::{BoxFuture, LocalBoxFuture};
use crate::type_erase::{Eraseable, Erased, Ref};
use crate::u128::U128;
use crate::Error;
use abi_stable::{
    external_types::{RMutex, RRwLock},
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RResult, RVec},
    DynTrait, StableAbi,
};
use futures::future::{abortable, try_join, try_join_all, AbortHandle, Aborted, Future, FutureExt};
use log::debug;
use tokio::runtime as tokio_runtime;

plugin_tls::thread_local! {
    static THREAD_ID: RRwLock<ROption<u64>> = RRwLock::new(ROption::RNone);
    static TASK_LOCAL: RMutex<BstMap<U128, RArc<Erased>>> = RMutex::new(Default::default());
}

static NEXT_THREAD_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Create a task local key from the given namespaced name.
///
/// Example usage:
/// ```
/// # #[macro_use] extern crate grease;
/// task_local_key!(grease::task_description);
/// ```
#[macro_export]
macro_rules! task_local_key {
    ( $( $l:ident )::+ ) => {
        {
            let mut id = $crate::uuid::grease_uuid(b"grease_task_local");
            $( id = $crate::uuid::Uuid::new_v5(&id, stringify!($l).as_bytes()); )+
            id.as_u128()
        }
    }
}

/// Get the current thread id, if it is a grease pool thread.
pub fn thread_id() -> Option<u64> {
    THREAD_ID.with(|m| m.read().as_ref().copied().into_option())
}

/// A reference to a task local value.
pub type TaskLocalRef<T> = Ref<T, RArc<Erased>>;

/// A type that has an associated task local key.
pub trait TaskLocal: Eraseable + StableAbi {
    /// The associated key.
    fn task_local_key() -> u128;

    /// Get the task local value, if set.
    fn task_local() -> Option<TaskLocalRef<Self>>
    where
        Self: Sized,
    {
        get_task_local::<Self>()
    }

    /// Run the given future with this task local value set.
    fn scoped<Fut: Future>(self, fut: Fut) -> ScopeTaskLocal<Fut>
    where
        Self: Sized,
    {
        scope_task_local(self, fut)
    }
}

/// Get a task local value.
pub fn get_task_local<T: TaskLocal>() -> Option<TaskLocalRef<T>> {
    let key = T::task_local_key();
    TASK_LOCAL
        .with(move |m| m.lock().get(&key).cloned())
        .map(|v| unsafe { TaskLocalRef::new(v) })
}

/// The future produced by `scope_task_local`.
#[pin_project::pin_project]
pub struct ScopeTaskLocal<Fut> {
    key: U128,
    value: Option<RArc<Erased>>,
    #[pin]
    future: Fut,
}

impl<Fut: Future> Future for ScopeTaskLocal<Fut> {
    type Output = Fut::Output;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        struct Guard<'a> {
            key: U128,
            value: &'a mut Option<RArc<Erased>>,
            prev: Option<RArc<Erased>>,
        }

        impl<'a> Drop for Guard<'a> {
            fn drop(&mut self) {
                let value = TASK_LOCAL.with(|m| {
                    let mut g = m.lock();
                    match self.prev.take() {
                        None => g.remove(&self.key),
                        Some(v) => g.insert(self.key, v),
                    }
                });
                *self.value = value;
            }
        }

        let mut proj = self.project();
        let val = proj.value.take().unwrap();
        let prev = TASK_LOCAL.with(|m| m.lock().insert(*proj.key, val));

        let _guard = Guard {
            prev,
            key: *proj.key,
            value: &mut proj.value,
        };

        proj.future.poll(cx)
    }
}

/// Set a task local value for the scope of the given future.
pub fn scope_task_local<T: TaskLocal, Fut: Future>(value: T, fut: Fut) -> ScopeTaskLocal<Fut> {
    ScopeTaskLocal {
        key: T::task_local_key().into(),
        value: Some(RArc::new(Erased::new(value))),
        future: fut,
    }
}

#[derive(StableAbi)]
#[repr(C)]
#[sabi(impl_InterfaceType(Send, Sync, Debug))]
struct SemaphorePermitInterface;

#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct SemaphorePermit<'a>(DynTrait<'a, RBox<()>, SemaphorePermitInterface>);

impl<'a> SemaphorePermit<'a> {
    fn new<T: Send + Sync + std::fmt::Debug + 'a>(key: T) -> Self {
        SemaphorePermit(DynTrait::from_borrowing_value(
            key,
            SemaphorePermitInterface,
        ))
    }
}

/// Trait to make a trait object from Semaphores.
#[sabi_trait]
trait SemaphoreInterface: Debug + Send + Sync {
    #[sabi(last_prefix_field)]
    fn acquire<'a>(&'a self, count: u32) -> BoxFuture<'a, SemaphorePermit<'a>>;
}

impl SemaphoreInterface for tokio::sync::Semaphore {
    fn acquire<'a>(&'a self, count: u32) -> BoxFuture<'a, SemaphorePermit<'a>> {
        BoxFuture::new(async move {
            if count == 0 {
                SemaphorePermit::new(())
            } else {
                let ret = SemaphorePermit::new(self.acquire_many(count).await);
                ret
            }
        })
    }
}

#[derive(Debug, StableAbi)]
#[repr(C)]
struct Semaphore(SemaphoreInterface_TO<'static, RBox<()>>, usize);

impl Semaphore {
    pub fn new(permits: usize) -> Self {
        Semaphore(
            SemaphoreInterface_TO::from_value(tokio::sync::Semaphore::new(permits), TU_Opaque),
            permits,
        )
    }
}

async fn acquire_owned(this: &RArc<Semaphore>, mut count: u32) -> SemaphorePermit<'static> {
    if count as usize > this.1 {
        count = this.1 as u32;
    }

    #[derive(Debug)]
    struct OwnedSemaphorePermit(SemaphorePermit<'static>, RArc<Semaphore>, u32);

    let permit = this.0.acquire(count).await;

    SemaphorePermit::new(OwnedSemaphorePermit(
        unsafe { std::mem::transmute::<SemaphorePermit<'_>, SemaphorePermit<'static>>(permit) },
        this.clone(),
        count,
    ))
}

/// Trait to make a trait object for the async runtime.
#[sabi_trait]
trait ThreadPoolInterface: Clone + Debug + Send + Sync {
    fn spawn_ok(&self, future: BoxFuture<'static, ()>);

    fn spawn_blocking(
        &self,
        f: ClosureOnce<(), Erased>,
    ) -> BoxFuture<'static, RResult<Erased, Error>>;

    #[sabi(last_prefix_field)]
    fn block_on<'a>(&self, future: LocalBoxFuture<'a, ()>);
}

impl ThreadPoolInterface for std::sync::Arc<tokio_runtime::Runtime> {
    fn spawn_ok(&self, future: BoxFuture<'static, ()>) {
        self.spawn(future);
    }

    fn spawn_blocking(
        &self,
        f: ClosureOnce<(), Erased>,
    ) -> BoxFuture<'static, RResult<Erased, Error>> {
        BoxFuture::new(
            self.as_ref()
                .spawn_blocking(move || f.call())
                .map(|r| r.map_err(|e| e.into()).into()),
        )
    }

    fn block_on<'a>(&self, future: LocalBoxFuture<'a, ()>) {
        self.as_ref().block_on(future)
    }
}

/// A permit to run a task.
pub type TaskPermit = SemaphorePermit<'static>;

/// Trait to be able to make a trait object from `AbortHandle`.
#[sabi_trait]
trait AbortHandleInterface: Clone + Debug + Send {
    #[sabi(last_prefix_field)]
    fn abort(&self);
}

impl AbortHandleInterface for AbortHandle {
    fn abort(&self) {
        self.abort()
    }
}

/// The task manager.
///
/// Allows tasks to spawn concurrent tasks to be run.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct TaskManager {
    pool: ThreadPoolInterface_TO<'static, RBox<()>>,
    tasks: RArc<Semaphore>,
    abort_handles: RArc<RMutex<RVec<AbortHandleInterface_TO<'static, RBox<()>>>>>,
    aggregate_errors: bool,
}

impl std::fmt::Debug for TaskManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("TaskManager")
            .field("pool", &self.pool)
            .field("tasks", &self.tasks)
            .field("abort_handles", &self.abort_handles.lock())
            .field("aggregate_errors", &self.aggregate_errors)
            .finish()
    }
}

impl TaskManager {
    /// Create a new task manager.
    ///
    /// The number of threads, if None, will be the number of cpus of the system.
    /// Aggregate errors determines whether joins will fail as soon as an error occurs (false) or
    /// whether it will wait for all results/errors (true).
    /// The on_error callback will be called once for each error that is created while futures are
    /// running in tasks.
    pub fn new(
        num_threads: Option<usize>,
        aggregate_errors: bool,
    ) -> Result<Self, futures::io::Error> {
        let threads = num_threads.unwrap_or_else(|| std::cmp::max(1, num_cpus::get()));
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(threads + 1));
        let is_core = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));

        let closure_barrier = barrier.clone();
        let closure_is_core = is_core.clone();
        let pool = tokio_runtime::Builder::new_multi_thread()
            .enable_all()
            .worker_threads(threads)
            .thread_name("grease-thread")
            .on_thread_start(move || {
                if closure_is_core.load(std::sync::atomic::Ordering::Relaxed) {
                    let thread_id =
                        NEXT_THREAD_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    THREAD_ID.with(|m| {
                        *m.write() = ROption::RSome(thread_id);
                    });
                    closure_barrier.wait();
                }
            })
            .on_thread_stop(|| {
                THREAD_ID.with(|m| {
                    *m.write() = ROption::RNone;
                });
            })
            .build()?;
        barrier.wait();
        is_core.store(false, std::sync::atomic::Ordering::Relaxed);

        Ok(TaskManager {
            pool: ThreadPoolInterface_TO::from_value(std::sync::Arc::new(pool), TU_Opaque),
            tasks: RArc::new(Semaphore::new(threads)),
            abort_handles: RArc::new(RMutex::new(Default::default())),
            aggregate_errors,
        })
    }

    /// Create a concurrent task to execute the given future to completion,
    /// returning the result of the future.
    pub fn spawn_basic<F, T>(&self, f: F) -> impl Future<Output = Result<T, Aborted>>
    where
        F: Future<Output = T> + Send + 'static,
        T: Send + 'static,
    {
        debug!("spawning new task");
        let (future, abort_handle) = abortable(f);
        self.abort_handles
            .lock()
            .push(AbortHandleInterface_TO::from_value(abort_handle, TU_Opaque));
        let (future, handle) = future.remote_handle();
        self.pool.spawn_ok(BoxFuture::new(future));
        handle
    }

    /// Create a concurrent task to execute the given future to completion, returning a future
    /// suitable for use with `Value`s.
    pub fn spawn<F, T>(&self, f: F) -> impl Future<Output = Result<T, Error>>
    where
        F: Future<Output = Result<T, Error>> + Send + 'static,
        T: Send + 'static,
    {
        self.spawn_basic(f).map(|res| match res {
            Ok(Ok(v)) => Ok(v),
            Ok(Err(e)) => Err(e),
            Err(_) => Err(Error::aborted()),
        })
    }

    pub fn spawn_blocking<F, R>(&self, f: F) -> impl Future<Output = Result<R, Error>> + 'static
    where
        F: FnOnce() -> R + Send + Sync + 'static,
        R: Send + Sync + 'static,
    {
        self.pool
            .spawn_blocking((|| Erased::new(f())).into())
            .map(|r| r.map(|v| unsafe { v.to_owned::<R>() }).into_result())
    }

    /// Join on the results of two futures according to the configured task aggregation strategy.
    pub fn join<Fut1, Fut2, T1, T2>(&self, future1: Fut1, future2: Fut2) -> Join<Fut1, Fut2>
    where
        Fut1: Future<Output = Result<T1, Error>>,
        Fut2: Future<Output = Result<T2, Error>>,
    {
        if self.aggregate_errors {
            Join::Aggregate(futures::future::join(future1, future2).map(join::aggregate))
        } else {
            Join::Normal(try_join(future1, future2))
        }
    }

    /// Join on the results of multiple futures according to the configured task aggregation
    /// strategy.
    pub fn join_all<I, Fut, T>(&self, i: I) -> JoinAll<Fut>
    where
        I: IntoIterator<Item = Fut>,
        Fut: Future<Output = Result<T, Error>>,
    {
        if self.aggregate_errors {
            JoinAll::Aggregate(futures::future::join_all(i).map(join_all::aggregate))
        } else {
            JoinAll::Normal(try_join_all(i))
        }
    }

    /// Abort all pending tasks.
    pub fn abort(&self) {
        for handle in self.abort_handles.lock().iter() {
            handle.abort();
        }
    }

    /// Block on the given future completing.
    pub fn block_on<F: Future>(&self, fut: F) -> F::Output {
        let (send, mut rcv) = futures::channel::oneshot::channel();
        self.pool.block_on(LocalBoxFuture::new(async move {
            send.send(fut.await)
                .map_err(|_| ())
                .expect("failed to send result");
        }));
        rcv.try_recv()
            .expect("channel unexpectedly cancelled")
            .expect("value not sent")
    }

    /// Count `n` tasks as being active.
    ///
    /// Until the returned permit is dropped, `n` active tasks will be counted against the total
    /// permissible concurrent tasks.
    pub async fn task_acquire(&self, count: u32) -> TaskPermit {
        acquire_owned(&self.tasks, count).await
    }

    /// Return whether the runtime is configured for error aggregation.
    pub fn aggregate_errors(&self) -> bool {
        self.aggregate_errors
    }
}

pub use join::Join;

mod join {
    use crate::Error;
    use futures::future::{Future, Map, TryFuture, TryJoin};
    use std::fmt::Debug;

    pub enum Join<Fut1, Fut2>
    where
        Fut1: Future + TryFuture,
        Fut2: Future + TryFuture,
    {
        Normal(TryJoin<Fut1, Fut2>),
        Aggregate(JoinAggregate<Fut1, Fut2>),
    }

    pub fn aggregate<A, B>((a, b): (Result<A, Error>, Result<B, Error>)) -> Result<(A, B), Error> {
        match (a, b) {
            (Ok(a), Ok(b)) => Ok((a, b)),
            (Ok(_), Err(b)) => Err(b),
            (Err(a), Ok(_)) => Err(a),
            (Err(a), Err(b)) => Err(vec![a, b].into_iter().collect()),
        }
    }

    type JoinAggregate<Fut1, Fut2> = Map<
        futures::future::Join<Fut1, Fut2>,
        fn(
            (<Fut1 as Future>::Output, <Fut2 as Future>::Output),
        ) -> Result<(<Fut1 as TryFuture>::Ok, <Fut2 as TryFuture>::Ok), Error>,
    >;

    impl<Fut1, Fut2> Debug for Join<Fut1, Fut2>
    where
        Fut1: Future + TryFuture + Debug,
        Fut2: Future + TryFuture + Debug,
        <Fut1 as Future>::Output: Debug,
        <Fut1 as TryFuture>::Ok: Debug,
        <Fut1 as TryFuture>::Error: Debug,
        <Fut2 as Future>::Output: Debug,
        <Fut2 as TryFuture>::Ok: Debug,
        <Fut2 as TryFuture>::Error: Debug,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                Join::Normal(inner) => Debug::fmt(inner, f),
                Join::Aggregate(inner) => Debug::fmt(inner, f),
            }
        }
    }

    impl<Fut1, Fut2> Future for Join<Fut1, Fut2>
    where
        Fut1: Future + TryFuture<Error = Error>,
        Fut2: Future + TryFuture<Error = Error>,
    {
        type Output = Result<(<Fut1 as TryFuture>::Ok, <Fut2 as TryFuture>::Ok), Error>;

        fn poll(
            self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context,
        ) -> std::task::Poll<Self::Output> {
            let me = unsafe { self.get_unchecked_mut() };
            match me {
                Join::Normal(inner) => {
                    Future::poll(unsafe { std::pin::Pin::new_unchecked(inner) }, cx)
                }
                Join::Aggregate(inner) => {
                    Future::poll(unsafe { std::pin::Pin::new_unchecked(inner) }, cx)
                }
            }
        }
    }

    impl<Fut1, Fut2> Unpin for Join<Fut1, Fut2>
    where
        Fut1: Future + TryFuture + Unpin,
        Fut2: Future + TryFuture + Unpin,
    {
    }
}

pub use join_all::JoinAll;

mod join_all {
    use crate::Error;
    use futures::future::{Future, Map, TryFuture, TryJoinAll};
    use std::fmt::Debug;

    pub enum JoinAll<F>
    where
        F: Future + TryFuture,
    {
        Normal(TryJoinAll<F>),
        Aggregate(JoinAggregate<F>),
    }

    pub fn aggregate<T>(vs: Vec<Result<T, Error>>) -> Result<Vec<T>, Error> {
        let mut oks = Vec::new();
        let mut errs = Vec::new();
        for v in vs {
            match v {
                Ok(v) => oks.push(v),
                Err(v) => errs.push(v),
            }
        }
        if errs.is_empty() {
            Ok(oks)
        } else {
            Err(errs.into_iter().collect())
        }
    }

    type JoinAggregate<F> = Map<
        futures::future::JoinAll<F>,
        fn(Vec<<F as Future>::Output>) -> Result<Vec<<F as TryFuture>::Ok>, Error>,
    >;

    impl<F> Debug for JoinAll<F>
    where
        F: Future + TryFuture + Debug,
        <F as Future>::Output: Debug,
        <F as TryFuture>::Ok: Debug,
        <F as TryFuture>::Error: Debug,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            match self {
                JoinAll::Normal(inner) => Debug::fmt(inner, f),
                JoinAll::Aggregate(inner) => Debug::fmt(inner, f),
            }
        }
    }

    impl<F> Future for JoinAll<F>
    where
        F: Future + TryFuture<Error = Error>,
    {
        type Output = Result<Vec<<F as TryFuture>::Ok>, Error>;

        fn poll(
            self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context,
        ) -> std::task::Poll<Self::Output> {
            let me = unsafe { self.get_unchecked_mut() };
            match me {
                JoinAll::Normal(inner) => Future::poll(std::pin::Pin::new(inner), cx),
                JoinAll::Aggregate(inner) => Future::poll(std::pin::Pin::new(inner), cx),
            }
        }
    }

    impl<F> Unpin for JoinAll<F> where F: Future + TryFuture + Unpin {}
}
