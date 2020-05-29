//! Task manager.

use crate::value::Error;
use futures::executor::ThreadPool;
use futures::future::{
    abortable, try_join, try_join_all, AbortHandle, Aborted, Future, FutureExt, LocalBoxFuture,
};
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
    pub fn spawn<F, T>(&self, f: F) -> impl Future<Output = Result<T, Aborted>>
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
    pub fn spawn_result<F, T, E>(&self, f: F) -> impl Future<Output = Result<T, Error>>
    where
        F: Future<Output = Result<T, E>> + Send + 'static,
        T: Send + 'static,
        E: Into<Error> + Send + 'static,
    {
        self.spawn(f).map(|res| match res {
            Ok(Ok(v)) => Ok(v),
            Ok(Err(e)) => Err(e.into()),
            Err(_) => Err(Error::aborted()),
        })
    }

    /// Join on the results of two futures according to the configured task aggregation strategy.
    pub fn join<'a, Fut1, Fut2, T1, T2>(
        &self,
        future1: Fut1,
        future2: Fut2,
    ) -> LocalBoxFuture<'a, Result<(T1, T2), Error>>
    where
        Fut1: Future<Output = Result<T1, Error>> + 'a,
        Fut2: Future<Output = Result<T2, Error>> + 'a,
    {
        if self.aggregate_errors {
            futures::future::join(future1, future2)
                .map(|(f1, f2)| match (f1, f2) {
                    (Ok(a), Ok(b)) => Ok((a, b)),
                    (Ok(_), Err(b)) => Err(b),
                    (Err(a), Ok(_)) => Err(a),
                    (Err(a), Err(b)) => Err(vec![a, b].into_iter().collect()),
                })
                .boxed_local()
        } else {
            try_join(future1, future2).boxed_local()
        }
    }

    /// Join on the results of multiple futures according to the configured task aggregation
    /// strategy.
    pub fn join_all<'a, I, Fut, T>(&self, i: I) -> LocalBoxFuture<'a, Result<Vec<T>, Error>>
    where
        I: IntoIterator<Item = Fut>,
        Fut: Future<Output = Result<T, Error>> + 'a,
    {
        if self.aggregate_errors {
            futures::future::join_all(i)
                .map(|vs| {
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
                })
                .boxed_local()
        } else {
            try_join_all(i).boxed_local()
        }
    }

    /// Abort all pending tasks.
    pub fn abort(&self) {
        for handle in self.abort_handles.lock().unwrap().iter() {
            handle.abort();
        }
    }
}
