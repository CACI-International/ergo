//! Task manager.

use futures::executor::ThreadPool;
use futures::future::{lazy, Future, FutureExt, RemoteHandle};

use crate::prelude::*;

/// The task manager.
///
/// Allows tasks to spawn concurrent tasks to be run.
#[derive(Debug, Clone)]
pub struct TaskManager {
    pool: ThreadPool,
    thread_ids: Vec<std::thread::ThreadId>
}

impl TaskManager {
    pub fn new() -> Result<Self, futures::io::Error> {
        let threads = std::cmp::max(1, num_cpus::get());
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(threads + 1));
        let thread_ids = std::sync::Arc::new(std::sync::Mutex::new(Vec::with_capacity(threads)));

        let closure_thread_ids = thread_ids.clone();
        let closure_barrier = barrier.clone();
        let pool = ThreadPool::builder()
            .pool_size(threads)
            .after_start(move |_| {
                {
                    let mut v = closure_thread_ids.lock().unwrap();
                    v.push(std::thread::current().id().clone());
                }
                closure_barrier.wait();
            })
            .create()?;
        barrier.wait();
        let ids = thread_ids.lock().unwrap().drain(..).collect();

        Ok(TaskManager { pool, thread_ids: ids })
    }

    /// Get the thread ids of pool threads.
    pub fn thread_ids(&self) -> &[std::thread::ThreadId] {
        self.thread_ids.as_ref()
    }

    /// Create a concurrent task to execute the given future to completion.
    pub fn spawn_future<F>(&self, f: F)
    where
        F: Future<Output = ()> + Send + 'static,
    {
        debug!("spawning new task");
        self.pool.spawn_ok(f)
    }

    /// Create a concurrent task to execute the given future to completion,
    /// returning the result of the future.
    pub fn spawn<F>(&self, f: F) -> RemoteHandle<<F as Future>::Output>
    where
        F: Future + Send + 'static,
        <F as Future>::Output: Send,
    {
        debug!("spawning new task");
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
        debug!("creating new delayed task");
        let (future, handle) = f.remote_handle();
        let p = self.pool.clone();
        let spawn = lazy(move |_| {
            debug!("spawning delayed task");
            p.spawn_ok(future)
        });
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
