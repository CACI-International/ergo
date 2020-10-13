//! Task manager.

// For generated sabi trait code with Closure<(), ...>.
#![allow(improper_ctypes_definitions)]

use crate::closure::ClosureOnce;
use crate::future::{BoxFuture, LocalBoxFuture};
use crate::type_erase::Erased;
use crate::value::Error;
use abi_stable::{
    external_types::{RMutex, RRwLock},
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RResult, RVec},
    StableAbi,
};
use futures::future::{abortable, try_join, try_join_all, AbortHandle, Aborted, Future, FutureExt};
use log::debug;
use tokio::runtime as tokio_runtime;

thread_local! {
    static ERROR_CALLBACK: RMutex<ROption<RArc<RMutex<ErrorCallback>>>> = RMutex::new(ROption::RNone);
    static THREAD_ID: RRwLock<ROption<u64>> = RRwLock::new(ROption::RNone);
}

static NEXT_THREAD_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

#[derive(StableAbi)]
#[repr(C)]
struct ErrorCallback(*const (), Erased);

unsafe impl Send for ErrorCallback {}

impl ErrorCallback {
    pub fn call(&self, added: bool) {
        unsafe { (std::mem::transmute::<*const (), fn(&Erased, bool)>(self.0))(&self.1, added) }
    }
}

/// Call the on_error handler for the active task manager.
pub fn call_on_error(added: bool) {
    ERROR_CALLBACK.with(|m| {
        let guard = m.lock();
        if let ROption::RSome(m2) = &*guard {
            m2.lock().call(added)
        }
    })
}

/// Get the current thread id, if it is a grease pool thread.
pub fn thread_id() -> Option<u64> {
    THREAD_ID.with(|m| m.read().as_ref().copied().into_option())
}

fn call_on_error_internal(cb: &Erased, added: bool) {
    (unsafe { cb.as_ref::<Box<OnError>>() })(added)
}

/// Callback when an error is created in a task.
pub type OnError = dyn Fn(bool) + Send + Sync;

/// Trait to be able to make a trait object from `ThreadPool`.
///
/// The only method we currently use on `ThreadPool` is `spawn_ok`.
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
                .handle()
                .spawn_blocking(move || f.call())
                .map(|r| r.map_err(|e| e.into()).into()),
        )
    }

    fn block_on<'a>(&self, future: LocalBoxFuture<'a, ()>) {
        self.as_ref().handle().block_on(future)
    }
}

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
    thread_ids: RVec<u64>,
    abort_handles: RArc<RMutex<RVec<AbortHandleInterface_TO<'static, RBox<()>>>>>,
    aggregate_errors: bool,
}

impl std::fmt::Debug for TaskManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("TaskManager")
            .field("pool", &self.pool)
            .field("thread_ids", &self.thread_ids)
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
        on_error: Option<Box<OnError>>,
    ) -> Result<Self, futures::io::Error> {
        let threads = num_threads.unwrap_or_else(|| std::cmp::max(1, num_cpus::get()));
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(threads + 1));
        let thread_ids = std::sync::Arc::new(std::sync::Mutex::new(Vec::with_capacity(threads)));
        let is_core = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));

        let error_cb = ROption::from(on_error.map(|v| {
            RArc::new(RMutex::new(ErrorCallback(
                call_on_error_internal as *const (),
                Erased::new(v),
            )))
        }));
        ERROR_CALLBACK.with(|m| {
            *m.lock() = error_cb.clone();
        });

        let closure_thread_ids = thread_ids.clone();
        let closure_barrier = barrier.clone();
        let closure_is_core = is_core.clone();
        let pool = tokio_runtime::Builder::new()
            .threaded_scheduler()
            .enable_all()
            .core_threads(threads)
            .thread_name("grease-thread")
            .on_thread_start(move || {
                if closure_is_core.load(std::sync::atomic::Ordering::Relaxed) {
                    ERROR_CALLBACK.with(|m| {
                        *m.lock() = error_cb.clone();
                    });
                    let thread_id =
                        NEXT_THREAD_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    THREAD_ID.with(|m| {
                        *m.write() = ROption::RSome(thread_id);
                    });
                    {
                        let mut v = closure_thread_ids.lock().unwrap();
                        v.push(thread_id);
                    }
                    closure_barrier.wait();
                }
            })
            .on_thread_stop(|| {
                ERROR_CALLBACK.with(|m| {
                    *m.lock() = ROption::RNone;
                });
                THREAD_ID.with(|m| {
                    *m.write() = ROption::RNone;
                });
            })
            .build()?;
        barrier.wait();
        is_core.store(false, std::sync::atomic::Ordering::Relaxed);
        let ids = thread_ids.lock().unwrap().drain(..).collect();

        Ok(TaskManager {
            pool: ThreadPoolInterface_TO::from_value(std::sync::Arc::new(pool), TU_Opaque),
            thread_ids: ids,
            abort_handles: RArc::new(RMutex::new(Default::default())),
            aggregate_errors,
        })
    }

    /// Get the thread ids of pool threads.
    pub fn thread_ids(&self) -> &[u64] {
        self.thread_ids.as_ref()
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
}

pub use join::Join;

mod join {
    use crate::value::Error;
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
        Fut1: Future + TryFuture<Error = Error> + Unpin,
        Fut2: Future + TryFuture<Error = Error> + Unpin,
    {
        type Output = Result<(<Fut1 as TryFuture>::Ok, <Fut2 as TryFuture>::Ok), Error>;

        fn poll(
            self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context,
        ) -> std::task::Poll<Self::Output> {
            match &*self {
                Join::Normal(_) => Future::poll(
                    unsafe {
                        self.map_unchecked_mut(|s| match s {
                            Join::Normal(inner) => inner,
                            _ => panic!("invalid"),
                        })
                    },
                    cx,
                ),
                Join::Aggregate(_) => Future::poll(
                    unsafe {
                        self.map_unchecked_mut(|s| match s {
                            Join::Aggregate(inner) => inner,
                            _ => panic!("invalid"),
                        })
                    },
                    cx,
                ),
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
    use crate::value::Error;
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
            match &*self {
                JoinAll::Normal(_) => Future::poll(
                    unsafe {
                        self.map_unchecked_mut(|s| match s {
                            JoinAll::Normal(inner) => inner,
                            _ => panic!("invalid"),
                        })
                    },
                    cx,
                ),
                JoinAll::Aggregate(_) => Future::poll(
                    unsafe {
                        self.map_unchecked_mut(|s| match s {
                            JoinAll::Aggregate(inner) => inner,
                            _ => panic!("invalid"),
                        })
                    },
                    cx,
                ),
            }
        }
    }

    impl<F> Unpin for JoinAll<F> where F: Future + TryFuture + Unpin {}
}
