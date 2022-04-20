//! Threaded runtime for task execution.
//!
//! The runtime offers the following services:
//! * async task execution
//! * a CPU-bound task pool and IO-bound task pool
//! * task priorities
//! * deadlock detection
//! * deadlock tracing

use crate::abi_stable::{closure::ClosureOnce, future::BoxFuture};
use parking_lot::{Condvar, Mutex};
use std::cell::UnsafeCell;
use std::collections::{BinaryHeap, HashSet, VecDeque};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};

const MIN_BLOCKING_POOL_SIZE: usize = 2;

const CONTROL_STATS_DURATION: std::time::Duration = std::time::Duration::from_millis(100);

#[derive(Debug)]
pub struct Runtime {
    handle: RuntimeHandle,
}

#[derive(Clone, Default)]
pub struct RuntimeHandle {
    inner: Arc<Inner>,
}

#[derive(Default)]
struct Inner {
    tasks: Arc<TaskPool>,
    blocking_tasks: BlockingTaskPool,
    next_task_id: AtomicU64,
    shutdown: AtomicBool,
}

#[derive(Default)]
struct BlockingTaskPool {
    tasks: Mutex<VecDeque<BlockingTask>>,
    waiting_on_tasks: Condvar,
    size: Mutex<BlockingPoolSize>,
    bored: AtomicUsize,
}

#[derive(Default)]
struct BlockingPoolSize {
    actual: usize,
    target: usize,
}

struct BlockingTask {
    id: u64,
    f: ClosureOnce<(), ()>,
}

#[derive(Default)]
struct TaskPool {
    tasks: Mutex<BinaryHeap<Arc<Task>>>,
    waiting_on_ready: Condvar,
}

struct Task {
    id: u64,
    priority: u32,
    future: TaskFuture,
    pool: Weak<TaskPool>,
    state: AtomicU8,
}

impl PartialEq for Task {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl Eq for Task {}

impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Task {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

const READY: u8 = 0;
const PENDING: u8 = 1;
const COMPLETE: u8 = 2;

impl std::task::Wake for Task {
    fn wake(self: Arc<Self>) {
        if let Some(pool) = self.pool.upgrade() {
            if self
                .state
                .compare_exchange(PENDING, READY, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                pool.tasks.lock().push(self);
                pool.waiting_on_ready.notify_one();
            }
        }
    }
}

struct TaskFuture(Mutex<Option<BoxFuture<'static, ()>>>);

impl TaskFuture {
    pub fn new(future: BoxFuture<'static, ()>) -> Self {
        TaskFuture(Mutex::new(Some(future)))
    }

    pub fn poll(&self, cx: &mut std::task::Context) -> bool {
        let mut guard = self.0.lock();
        if let Some(fut) = guard.as_mut() {
            let done = Pin::new(fut).poll(cx).is_ready();
            if done {
                *guard = None;
            }
            done
        } else {
            true
        }
    }
}

impl Runtime {
    pub fn new(pool_size: usize) -> std::io::Result<Self> {
        let rt = Runtime {
            handle: Default::default(),
        };

        // Normal pool
        for i in 0..pool_size {
            let handle = rt.handle.clone();
            std::thread::Builder::new()
                .name(format!("ergo pool {}", i))
                .spawn(move || handle.pool_worker())?;
        }

        // Blocking pool
        rt.handle.set_blocking_pool_size(MIN_BLOCKING_POOL_SIZE)?;

        // Control thread
        {
            let handle = rt.handle.clone();
            std::thread::Builder::new()
                .name("ergo pool control".into())
                .spawn(move || handle.control_worker())?;
        }

        Ok(rt)
    }

    pub fn shutdown(&self) {
        self.inner.indicate_shutdown();
    }

    pub fn handle(&self) -> RuntimeHandle {
        self.handle.clone()
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        self.shutdown();
    }
}

impl std::ops::Deref for Runtime {
    type Target = RuntimeHandle;

    fn deref(&self) -> &Self::Target {
        &self.handle
    }
}

impl std::borrow::Borrow<RuntimeHandle> for Runtime {
    fn borrow(&self) -> &RuntimeHandle {
        &self.handle
    }
}

struct BlockOnWaker {
    state: AtomicU8,
    thread: std::thread::Thread,
}

impl BlockOnWaker {
    pub fn new() -> Arc<Self> {
        Arc::new(BlockOnWaker {
            state: AtomicU8::new(PENDING),
            thread: std::thread::current(),
        })
    }
}

impl std::task::Wake for BlockOnWaker {
    fn wake(self: Arc<Self>) {
        self.wake_by_ref();
    }

    fn wake_by_ref(self: &Arc<Self>) {
        if self
            .state
            .compare_exchange(PENDING, READY, Ordering::Release, Ordering::Relaxed)
            .is_ok()
        {
            self.thread.unpark();
        }
    }
}

impl std::fmt::Debug for RuntimeHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("RuntimeHandle").finish()
    }
}

impl RuntimeHandle {
    /// Spawns a future onto the runtime that will be run to completion.
    pub fn spawn(&self, priority: u32, future: BoxFuture<'static, ()>) {
        let id = self.inner.next_task_id.fetch_add(1, Ordering::Relaxed);
        self.inner.tasks.new_task(id, priority, future);
    }

    /// Runs a function on the runtime.
    ///
    /// Blocking tasks will be serviced by a separate, dynamic thread pool.
    pub fn spawn_blocking(&self, f: ClosureOnce<(), ()>) {
        let id = self.inner.next_task_id.fetch_add(1, Ordering::Relaxed);
        self.inner.blocking_tasks.new_task(id, f);
    }

    pub fn block_on<'a, Fut: std::future::Future<Output = ()> + 'a>(&self, mut fut: Fut) {
        let block_on_waker = BlockOnWaker::new();
        use std::task::{Context, Poll, Waker};
        let waker = Waker::from(block_on_waker.clone());
        // Safety: we guarantee that `fut` will not be moved after this call by overwriting the
        // `fut` binding itself.
        let mut fut = unsafe { Pin::new_unchecked(&mut fut) };
        while let Poll::Pending = fut.as_mut().poll(&mut Context::from_waker(&waker)) {
            std::thread::park();
            while block_on_waker
                .state
                .compare_exchange(READY, PENDING, Ordering::Acquire, Ordering::Relaxed)
                .is_err()
            {
                std::thread::park();
            }
        }
    }

    fn new_blocking_pool_thread(&self) -> std::io::Result<()> {
        let handle = self.clone();
        std::thread::Builder::new()
            .name("ergo blocking pool".into())
            .spawn(move || handle.blocking_pool_worker())?;
        Ok(())
    }

    fn set_blocking_pool_size(&self, size: usize) -> std::io::Result<()> {
        let to_spawn = {
            let mut guard = self.inner.blocking_tasks.size.lock();
            guard.target = size;
            if guard.actual < guard.target {
                guard.target - guard.actual
            } else {
                0
            }
        };

        for _ in 0..to_spawn {
            self.new_blocking_pool_thread()?;
            self.inner.blocking_tasks.size.lock().actual += 1;
        }

        Ok(())
    }

    fn pool_worker(&self) {
        while !self.inner.shutdown() {
            self.inner.tasks.try_run_task(&self.inner);
        }
    }

    fn blocking_pool_worker(&self) {
        while !self.inner.shutdown() && !self.inner.blocking_tasks.exit() {
            self.inner.blocking_tasks.try_run_task();
        }
    }

    fn control_worker(&self) {
        let mut ready = 0;
        let mut bored = 0;
        while !self.inner.shutdown() {
            std::thread::sleep(CONTROL_STATS_DURATION);
            let blocking_tasks = &self.inner.blocking_tasks;
            let ready_now = blocking_tasks.tasks.lock().len();
            let bored_now = blocking_tasks.bored.swap(0, Ordering::Relaxed);
            // Set `ready` to `ready_now` and take the min of the two values
            let waiting = std::cmp::min(std::mem::replace(&mut ready, ready_now), ready_now);
            // Set `bored` to `bored_now` and take the min of the two values
            let wasted = std::cmp::min(std::mem::replace(&mut bored, bored_now), bored_now);

            let pool_size = blocking_tasks.size.lock().target;
            let result = if waiting > 0 {
                // If we have a lot of ready and waiting tasks, we need to expand the thread pool.
                self.set_blocking_pool_size(pool_size + waiting)
            } else if wasted > 0 && pool_size != MIN_BLOCKING_POOL_SIZE {
                // Clip `wasted` for safety, though in general it shouldn't be more than pool_size (but
                // sleep scheduling might be wonky).
                let wasted = std::cmp::min(wasted, pool_size);
                // If we had threads not doing anything, we should decrease the thread pool size.
                self.set_blocking_pool_size(std::cmp::max(
                    pool_size - wasted,
                    MIN_BLOCKING_POOL_SIZE,
                ))
            } else {
                Ok(())
            };
            if let Err(e) = result {
                log::error!("blocking thread launch error: {}", e);
            }
        }
    }
}

impl TaskPool {
    pub fn new_task(self: &Arc<Self>, id: u64, priority: u32, future: BoxFuture<'static, ()>) {
        self.tasks.lock().push(Arc::new(Task {
            id,
            priority,
            future: TaskFuture::new(future),
            pool: Arc::downgrade(self),
            state: AtomicU8::new(READY),
        }));
        self.waiting_on_ready.notify_one();
    }

    // Return an Option to allow spurious wakeup for shutdown
    fn next_ready_task(&self, inner: &Inner) -> Option<Arc<Task>> {
        let mut guard = self.tasks.lock();
        while guard.is_empty() {
            self.waiting_on_ready.wait(&mut guard);
            if inner.shutdown() {
                return None;
            }
        }
        let ret = guard.pop();
        debug_assert!(ret.is_some());
        ret
    }

    pub fn try_run_task(&self, inner: &Inner) {
        if let Some(task) = self.next_ready_task(inner) {
            use std::task::{Context, Poll, Waker};
            if task
                .state
                .compare_exchange(READY, PENDING, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                if task
                    .future
                    .poll(&mut Context::from_waker(&task.clone().into()))
                {
                    task.state.store(COMPLETE, Ordering::Relaxed);
                }
            }
        }
    }
}

impl BlockingTaskPool {
    pub fn new_task(&self, id: u64, f: ClosureOnce<(), ()>) {
        self.tasks.lock().push_back(BlockingTask { id, f });
        self.waiting_on_tasks.notify_one();
    }

    pub fn exit(&self) -> bool {
        let mut guard = self.size.lock();
        if guard.actual > guard.target {
            guard.actual -= 1;
            true
        } else {
            false
        }
    }

    // Return an Option to allow spurious wakeup for shutdown
    fn next_task(&self) -> Option<BlockingTask> {
        let mut guard = self.tasks.lock();
        if guard.is_empty() {
            self.bored.fetch_add(1, Ordering::Relaxed);
            self.waiting_on_tasks
                .wait_for(&mut guard, CONTROL_STATS_DURATION);
            return None;
        }
        let ret = guard.pop_front();
        debug_assert!(ret.is_some());
        ret
    }

    pub fn try_run_task(&self) {
        if let Some(BlockingTask { f, .. }) = self.next_task() {
            f.call();
        }
    }
}

impl Inner {
    pub fn indicate_shutdown(&self) {
        self.shutdown.store(true, Ordering::Relaxed);
        self.tasks.waiting_on_ready.notify_all();
        self.blocking_tasks.waiting_on_tasks.notify_all();
    }

    pub fn shutdown(&self) -> bool {
        self.shutdown.load(Ordering::Relaxed)
    }
}
