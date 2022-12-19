//! Task management.

// For generated sabi trait code with Closure<(), ...>.
#![allow(improper_ctypes_definitions)]

use crate::abi_stable::{
    closure::ClosureOnceT,
    external_types::RMutex,
    future::{BoxFuture, LocalBoxFuture},
    marker_type::UnsyncSend,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RVec},
    type_erase::Eraseable,
    DynTrait, StableAbi,
};
use crate::error::DiagnosticInfo;
use crate::Error;
use futures::future::{abortable, try_join_all, AbortHandle, Aborted, Future, FutureExt};
use std::cell::Cell;
pub mod runtime;

/// Create a task local value.
#[macro_export]
macro_rules! task_local {
    ($(#[$attr:meta])* $vis:vis static $name:ident : $t:ty ;) => {
        $(#[$attr])*
        $vis static $name: $crate::context::LocalKey<$t> = {
            use $crate::abi_stable::{std_types::{ROption}};
            use std::cell::Cell;
            type Storage = Cell<ROption<$t>>;
            plugin_tls::thread_local! {
                static VALUE: Storage = Cell::new(ROption::RNone);
            }

            unsafe extern "C" fn __task_local_read() -> *const Storage {
                VALUE.with(|v| v as *const Storage)
            }

            $crate::context::LocalKey { read: __task_local_read }
        };
    }
}

/// A key for a task local value.
pub struct LocalKey<T: 'static> {
    #[doc(hidden)]
    pub read: unsafe extern "C" fn() -> *const Cell<ROption<T>>,
}

#[pin_project::pin_project]
struct ScopeTaskLocal<T: 'static, Fut> {
    key: &'static LocalKey<T>,
    value: ROption<T>,
    #[pin]
    future: Fut,
}

impl<T: StableAbi + Eraseable, Fut: Future> Future for ScopeTaskLocal<T, Fut> {
    type Output = Fut::Output;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Self::Output> {
        struct Guard<'a, T: StableAbi + Eraseable> {
            key: &'static LocalKey<T>,
            value: &'a mut ROption<T>,
            prev: ROption<T>,
        }

        impl<'a, T: StableAbi + Eraseable> Drop for Guard<'a, T> {
            fn drop(&mut self) {
                *self.value = self.key.cell().replace(self.prev.take());
            }
        }

        let mut proj = self.project();
        let prev = proj.key.cell().replace(proj.value.take());

        let _guard = Guard {
            prev,
            key: proj.key,
            value: &mut proj.value,
        };

        proj.future.poll(cx)
    }
}

impl<T: StableAbi + Eraseable> LocalKey<T> {
    pub async fn scope<F>(&'static self, value: T, future: F) -> F::Output
    where
        F: Future,
    {
        ScopeTaskLocal {
            key: self,
            value: ROption::RSome(value),
            future,
        }
        .await
    }

    pub async fn final_scope<F>(&'static self, value: T, future: F) -> (T, F::Output)
    where
        F: Future,
    {
        let scope = ScopeTaskLocal {
            key: self,
            value: ROption::RSome(value),
            future,
        };
        tokio::pin!(scope);
        let ret = (&mut scope).await;
        (scope.project().value.take().unwrap(), ret)
    }

    pub fn with<F, R>(&'static self, f: F) -> R
    where
        F: FnOnce(Option<&T>) -> R,
    {
        f(unsafe { self.cell().as_ptr().as_ref().unwrap() }
            .as_ref()
            .into_option())
    }

    pub fn get(&'static self) -> Option<T>
    where
        T: Clone,
    {
        self.with(|v| v.cloned())
    }

    fn cell(&'static self) -> &Cell<ROption<T>> {
        unsafe { (self.read)().as_ref() }.unwrap()
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
        SemaphorePermit(DynTrait::from_borrowing_value(key))
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
                SemaphorePermit::new(self.acquire_many(count).await)
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
            SemaphoreInterface_TO::from_value(tokio::sync::Semaphore::new(permits), TD_Opaque),
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
    fn spawn_ok(&self, priority: u32, future: BoxFuture<'static, ()>);

    fn spawn_blocking(&self, f: ClosureOnceT<'static, UnsyncSend, (), ()>);

    fn block_on<'a>(&self, future: LocalBoxFuture<'a, ()>);

    #[sabi(last_prefix_field)]
    fn wait_single(&self);
}

#[derive(Debug)]
struct ThreadPool {
    pool: runtime::Runtime,
}

impl ThreadPoolInterface for std::sync::Arc<ThreadPool> {
    fn spawn_ok(&self, priority: u32, future: BoxFuture<'static, ()>) {
        self.pool.spawn(priority, future);
    }

    fn spawn_blocking(&self, f: ClosureOnceT<'static, UnsyncSend, (), ()>) {
        self.pool.spawn_blocking(f);
    }

    fn block_on<'a>(&self, future: LocalBoxFuture<'a, ()>) {
        self.pool.block_on(future)
    }

    fn wait_single(&self) {
        while std::sync::Arc::strong_count(self) > 1 {
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
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
    threads: usize,
    aggregate_errors: bool,
}

impl std::fmt::Debug for TaskManager {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("TaskManager")
            .field("pool", &self.pool)
            .field("tasks", &self.tasks)
            .field("abort_handles", &self.abort_handles.lock())
            .field("threads", &self.threads)
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
    pub fn new(
        num_threads: Option<usize>,
        aggregate_errors: bool,
        progress: super::progress::Progress,
    ) -> Result<Self, futures::io::Error> {
        let threads = std::cmp::max(1, num_threads.unwrap_or_else(num_cpus::get));
        let abort_handles: RArc<RMutex<RVec<AbortHandleInterface_TO<'static, RBox<()>>>>> =
            RArc::new(RMutex::new(Default::default()));

        // XXX temporary until task cancellation is improved
        let batch_task_cancellation = std::env::var_os("ERGO_DEADLOCK_BATCH")
            .and_then(|s| s.to_str().and_then(|s| s.parse().ok()))
            .unwrap_or(usize::MAX);

        let pool = {
            let abort_handles = abort_handles.clone();
            let abort_handles2 = abort_handles.clone();
            let progress2 = progress.clone();
            runtime::Runtime::builder()
                .pool_size(threads)
                .on_inactivity(move || {
                    log::warn!("indicating deadlock due to inactivity");
                    progress.indicate_deadlock();
                    for _ in 0..batch_task_cancellation {
                        if let Some(handle) = abort_handles.lock().pop() {
                            handle.abort();
                        } else {
                            break;
                        }
                    }
                })
                .on_maintenance(move |rthandle| {
                    if progress2.check_for_deadlock(rthandle.has_blocking_tasks()) {
                        log::warn!("deadlock detected; aborting tasks");
                        for _ in 0..batch_task_cancellation {
                            if let Some(handle) = abort_handles2.lock().pop() {
                                handle.abort();
                            } else {
                                break;
                            }
                        }
                    }
                })
                .build()?
        };

        Ok(TaskManager {
            pool: ThreadPoolInterface_TO::from_value(
                std::sync::Arc::new(ThreadPool { pool }),
                TD_Opaque,
            ),
            tasks: RArc::new(Semaphore::new(threads)),
            abort_handles,
            threads,
            aggregate_errors,
        })
    }

    /// Create a concurrent task to execute the given future to completion,
    /// returning the result of the future.
    ///
    /// The priority is used to set the relative scheduling of the task. Lower priority values will
    /// execute sooner.
    pub fn spawn_basic<F, T>(&self, priority: u32, f: F) -> impl Future<Output = Result<T, Aborted>>
    where
        F: Future<Output = T> + Send + 'static,
        T: Send + 'static,
    {
        let (future, abort_handle) = abortable(f);
        {
            self.abort_handles
                .lock()
                .push(AbortHandleInterface_TO::from_value(abort_handle, TD_Opaque));
        }
        let (future, handle) = future.remote_handle();
        self.pool.spawn_ok(priority, BoxFuture::new(future));
        handle
    }

    /// Create a concurrent task to execute the given future to completion.
    ///
    /// The priority is used to set the relative scheduling of the task. Lower priority values will
    /// execute sooner.
    pub fn spawn<F, T>(&self, priority: u32, f: F) -> impl Future<Output = Result<T, Error>>
    where
        F: Future<Output = Result<T, Error>> + Send + 'static,
        T: Send + 'static,
    {
        self.spawn_basic(priority, f).map(|res| match res {
            Ok(Ok(v)) => Ok(v),
            Ok(Err(e)) => Err(e),
            Err(_) => Err(Error::aborted()),
        })
    }

    /// Create a concurrent task to execute the given function (containing an IO blocking
    /// operation). The concurrent task is spawned on the IO thread pool.
    pub fn spawn_blocking<F, R>(&self, f: F) -> impl Future<Output = Result<R, Error>> + 'static
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        let (send, rcv) = futures::channel::oneshot::channel();
        self.pool
            .spawn_blocking((move || drop(send.send(f()))).into());
        rcv.map(|r| r.into_diagnostic().map_err(|e| e.into()))
    }

    /// Join on the results of multiple futures according to the configured task aggregation
    /// strategy.
    pub fn join_all<I, Fut, T>(&self, i: I) -> JoinAll<Fut>
    where
        I: IntoIterator<Item = Fut>,
        Fut: Future<Output = crate::Result<T>>,
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

    /// Return the number of configured threads.
    pub fn threads(&self) -> usize {
        self.threads
    }

    /// Shutdown the runtime, waiting for pending tasks to complete.
    pub fn shutdown(&self) {
        self.abort();
        self.pool.wait_single();
    }
}

pub use join_all::JoinAll;

mod join_all {
    use futures::future::{Future, Map, TryFuture, TryJoinAll};
    use std::fmt::Debug;

    pub enum JoinAll<F>
    where
        F: Future + TryFuture,
    {
        Normal(TryJoinAll<F>),
        Aggregate(JoinAggregate<F>),
    }

    pub fn aggregate<T>(vs: Vec<crate::Result<T>>) -> crate::Result<Vec<T>> {
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
        fn(Vec<<F as Future>::Output>) -> crate::Result<Vec<<F as TryFuture>::Ok>>,
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
        F: Future + TryFuture<Error = crate::Error>,
    {
        type Output = crate::Result<Vec<<F as TryFuture>::Ok>>;

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
