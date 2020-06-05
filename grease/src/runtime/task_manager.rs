//! Task manager.

use crate::value::Error;
use futures::executor::ThreadPool;
use futures::future::{abortable, try_join, try_join_all, AbortHandle, Aborted, Future, FutureExt};
use log::debug;
use std::cell::RefCell;
use std::sync::{Arc, Mutex};

thread_local! {
    static ON_ERROR: RefCell<Option<Arc<OnError>>> = RefCell::new(None);
}

/// Call the on_error handler for the active task manager.
pub fn call_on_error() {
    ON_ERROR.with(|v| {
        if let Some(ref f) = *v.borrow() {
            f();
        }
    });
}

/// Callback when an error is created in a task.
pub type OnError = dyn Fn() + Send + Sync;

/// The task manager.
///
/// Allows tasks to spawn concurrent tasks to be run.
#[derive(Debug, Clone)]
pub struct TaskManager {
    pool: ThreadPool,
    thread_ids: Vec<std::thread::ThreadId>,
    abort_handles: Arc<Mutex<Vec<AbortHandle>>>,
    aggregate_errors: bool,
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

        let closure_thread_ids = thread_ids.clone();
        let closure_barrier = barrier.clone();
        let on_error = on_error.map(|v| Arc::from(v));
        let pool = ThreadPool::builder()
            .pool_size(threads)
            .after_start(move |_| {
                ON_ERROR.with(|v| {
                    *v.borrow_mut() = on_error.clone();
                });
                {
                    let mut v = closure_thread_ids.lock().unwrap();
                    v.push(std::thread::current().id().clone());
                }
                closure_barrier.wait();
            })
            .before_stop(|_| {
                ON_ERROR.with(|v| {
                    *v.borrow_mut() = None;
                });
            })
            .create()?;
        barrier.wait();
        let ids = thread_ids.lock().unwrap().drain(..).collect();

        Ok(TaskManager {
            pool,
            thread_ids: ids,
            abort_handles: Default::default(),
            aggregate_errors,
        })
    }

    /// Get the thread ids of pool threads.
    pub fn thread_ids(&self) -> &[std::thread::ThreadId] {
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
        self.abort_handles.lock().unwrap().push(abort_handle);
        let (future, handle) = future.remote_handle();
        self.pool.spawn_ok(future);
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
        for handle in self.abort_handles.lock().unwrap().iter() {
            handle.abort();
        }
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
        F: Future + TryFuture<Error = Error> + Unpin,
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
