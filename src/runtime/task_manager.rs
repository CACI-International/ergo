//! Task manager.

use futures::executor::ThreadPool;
use futures::future::{lazy, Future, FutureExt, RemoteHandle};

/// The task manager.
///
/// Allows tasks to spawn concurrent tasks to be run.
#[derive(Debug, Clone)]
pub struct TaskManager {
    pool: ThreadPool,
}

impl TaskManager {
    pub fn new() -> Result<Self, futures::io::Error> {
        Ok(TaskManager {
            pool: ThreadPool::new()?,
        })
    }

    /// Create a concurrent task to execute the given future to completion.
    pub fn spawn<F>(&self, f: F)
    where
        F: Future<Output = ()> + Send + 'static,
    {
        self.pool.spawn_ok(f)
    }

    /// Create a concurrent task to execute the given future to completion,
    /// returning the result of the future.
    pub fn spawn_value<F>(&self, f: F) -> RemoteHandle<<F as Future>::Output>
    where
        F: Future + Send + 'static,
        <F as Future>::Output: Send,
    {
        let (future, handle) = f.remote_handle();
        self.pool.spawn_ok(future);
        handle
    }

    /// Create a future which will run the given future in a concurrent task lazily.
    ///
    /// The returned future, when first polled, will launch a concurrent task to evaluate the
    /// provided future.
    pub fn delayed<F>(&self, f: F) -> impl Future<Output = <F as Future>::Output>
    where
        F: Future + Send + 'static,
        <F as Future>::Output: Send,
    {
        let (future, handle) = f.remote_handle();
        let p = self.pool.clone();
        let spawn = lazy(move |_| p.spawn_ok(future));
        spawn.then(move |()| handle)
    }

    /// Create a future which will concurrently run the given function lazily.
    ///
    /// Similar to `futures::future::lazy`, however lazily queues a concurrent task as well.
    pub fn delayed_fn<F, R>(&self, f: F) -> impl Future<Output = R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send,
    {
        self.delayed(lazy(move |_| f()))
    }
}
