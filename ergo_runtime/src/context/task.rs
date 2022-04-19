//! Task management.

// For generated sabi trait code with Closure<(), ...>.
#![allow(improper_ctypes_definitions)]

use crate::abi_stable::{
    closure::ClosureOnce,
    external_types::{RMutex, RRwLock},
    future::{BoxFuture, LocalBoxFuture},
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RResult, RVec},
    type_erase::{Eraseable, Erased},
    DynTrait, StableAbi,
};
use crate::error::DiagnosticInfo;
use crate::Error;
use futures::future::{abortable, try_join_all, AbortHandle, Aborted, Future, FutureExt};
use std::cell::Cell;
//use tokio::runtime as tokio_runtime;
mod runtime;

plugin_tls::thread_local! {
    static THREAD_ID: RRwLock<ROption<u64>> = RRwLock::new(ROption::RNone);
}

static NEXT_THREAD_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

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

/// Get the current thread id, if it is a task pool thread.
pub fn thread_id() -> Option<u64> {
    THREAD_ID.with(|m| m.read().as_ref().copied().into_option())
}

mod task_priority {
    use crate::abi_stable::external_types::RMutex;
    use std::collections::{BTreeMap, HashSet};
    use std::future::Future;
    use std::pin::Pin;
    use std::ptr::NonNull;
    use std::sync::Arc;
    use std::task::Waker;
    use std::task::{Context, Poll};

    enum WaitState {
        Runnable,
        FuturePending,
        WaitingForPriority(Waker),
        Done,
    }

    #[derive(Default)]
    struct Control {
        runnable: BTreeMap<u32, HashSet<NonNull<Registration>>>,
    }

    // NonNull<Registration> are always accessed while locking the control, so are Send and Sync.
    unsafe impl Send for Control {}
    unsafe impl Sync for Control {}

    impl Control {
        pub fn wake_next(&mut self) {
            if let Some((_, v)) = self.runnable.iter_mut().next() {
                for reg in v.iter() {
                    match std::mem::replace(
                        &mut unsafe { reg.clone().as_mut() }.wait_state,
                        WaitState::Runnable,
                    ) {
                        WaitState::WaitingForPriority(waker) => waker.wake(),
                        _ => (),
                    }
                }
            }
        }
    }

    pub struct TaskPriority {
        control: Arc<RMutex<Control>>,
    }

    pub struct Registration {
        priority: u32,
        wait_state: WaitState,
    }

    #[pin_project::pin_project(PinnedDrop)]
    pub struct Prioritized<Fut> {
        registration: Registration,
        control: Arc<RMutex<Control>>,
        #[pin]
        future: Fut,
    }

    impl<Fut: Future> Future for Prioritized<Fut> {
        type Output = Fut::Output;

        fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
            let mut proj = self.project();
            let ptr = NonNull::from(&*proj.registration);
            let mut guard = proj.control.lock();
            // If we previously returned a Poll::Pending due from the future, we should re-add
            // ourselves to the pending queue and then see whether we should run.
            if let WaitState::FuturePending = proj.registration.wait_state {
                let set = guard
                    .runnable
                    .entry(proj.registration.priority)
                    .or_default();
                set.insert(ptr);
                proj.registration.wait_state = WaitState::Runnable;
            }
            // Get the front of the queue.
            let (k, v) = guard
                .runnable
                .iter_mut()
                .next()
                .expect("registration polled but no registrations pending");
            // Check whether our priority matches the front of the queue (indicating we can run).
            let run_now = *k == proj.registration.priority;
            if run_now {
                // Remove ourselves from the queue (and possibly wake up the next tier).
                v.remove(&ptr);
                if v.is_empty() {
                    let k = k.clone();
                    drop(v);
                    guard.runnable.remove(&k);
                    guard.wake_next();
                }
                proj.registration.wait_state = WaitState::Done;
                drop(guard);
                // Run the future.
                let ret = proj.future.poll(cx);
                if ret.is_pending() {
                    // If the future is pending, store that state but don't add ourselves back to
                    // the queue (as we are not runnable).
                    proj.registration.wait_state = WaitState::FuturePending;
                }
                ret
            } else {
                // We still remain in the runnable queue, but add the waker to be awoken later.
                proj.registration.wait_state = WaitState::WaitingForPriority(cx.waker().clone());
                Poll::Pending
            }
        }
    }

    #[pin_project::pinned_drop]
    impl<Fut> PinnedDrop for Prioritized<Fut> {
        fn drop(self: Pin<&mut Self>) {
            let proj = self.project();
            match &proj.registration.wait_state {
                WaitState::Done | WaitState::FuturePending => return,
                _ => (),
            }
            let ptr = NonNull::from(&*proj.registration);
            let mut guard = proj.control.lock();
            let should_wake_next =
                *guard.runnable.keys().next().unwrap() == proj.registration.priority;
            let set = guard.runnable.get_mut(&proj.registration.priority).unwrap();
            set.remove(&ptr);
            if set.is_empty() {
                guard.runnable.remove(&proj.registration.priority);
                if should_wake_next {
                    guard.wake_next();
                }
            }
        }
    }

    impl TaskPriority {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn prioritize<Fut>(&self, future: Fut, priority: u32) -> Pin<Box<Prioritized<Fut>>> {
            let ret = Box::pin(Prioritized {
                registration: Registration {
                    priority,
                    wait_state: WaitState::Runnable,
                },
                control: self.control.clone(),
                future,
            });
            let mut guard = self.control.lock();
            let set = guard.runnable.entry(priority).or_default();
            set.insert((&ret.as_ref().get_ref().registration).into());
            ret
        }
    }

    impl Default for TaskPriority {
        fn default() -> Self {
            TaskPriority {
                control: Arc::new(RMutex::new(Default::default())),
            }
        }
    }

    impl std::fmt::Debug for TaskPriority {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("TaskPriority").finish_non_exhaustive()
        }
    }
}

use task_priority::TaskPriority;

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
    fn spawn_ok(&self, priority: u32, future: BoxFuture<'static, ()>);

    fn spawn_blocking(&self, f: ClosureOnce<(), ()>);

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

    fn spawn_blocking(&self, f: ClosureOnce<(), ()>) {
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
    /// The on_error callback will be called once for each error that is created while futures are
    /// running in tasks.
    pub fn new(
        num_threads: Option<usize>,
        aggregate_errors: bool,
    ) -> Result<Self, futures::io::Error> {
        let threads = std::cmp::max(1, num_threads.unwrap_or_else(num_cpus::get));
        let pool = runtime::Runtime::new(threads)?;

        Ok(TaskManager {
            pool: ThreadPoolInterface_TO::from_value(
                std::sync::Arc::new(ThreadPool { pool }),
                TU_Opaque,
            ),
            tasks: RArc::new(Semaphore::new(threads)),
            abort_handles: RArc::new(RMutex::new(Default::default())),
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
        self.abort_handles
            .lock()
            .push(AbortHandleInterface_TO::from_value(abort_handle, TU_Opaque));
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
        F: FnOnce() -> R + Eraseable,
        R: Eraseable,
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
