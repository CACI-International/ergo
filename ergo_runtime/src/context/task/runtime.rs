//! Threaded runtime for task execution.
//!
//! The runtime offers the following services:
//! * async task execution
//! * a CPU-bound task pool and IO-bound task pool
//! * task priorities
//! * deadlock detection
//! * deadlock tracing

use crate::abi_stable::{closure::ClosureOnceT, future::BoxFuture, marker_type::UnsyncSend};
use parking_lot::{Condvar, Mutex, RwLock, RwLockUpgradableReadGuard, RwLockWriteGuard};
use std::cell::UnsafeCell;
use std::collections::{BTreeMap, VecDeque};
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};

const MIN_BLOCKING_POOL_SIZE: usize = 2;
pub const MAINTENANCE_INTERVAL: std::time::Duration = std::time::Duration::from_millis(100);
const INACTIVITY_DURATION: std::time::Duration =
    std::time::Duration::from_secs(if cfg!(debug_assertions) { 10 } else { 1 });
const WORKER_QUEUE_SIZE: u16 = 128;

const INACTIVITY_INTERVALS: usize =
    (INACTIVITY_DURATION.as_millis() / MAINTENANCE_INTERVAL.as_millis()) as usize;

std::thread_local! {
    static WORKER_QUEUE: UnsafeCell<Option<WorkerQueue>> = UnsafeCell::new(None);
}

#[derive(Debug)]
pub struct Runtime {
    handle: RuntimeHandle,
}

pub struct Builder {
    pool_size: usize,
    inactivity: Option<Box<dyn FnMut() + Send + 'static>>,
    maintenance: Option<Box<dyn FnMut(&RuntimeHandle) + Send + 'static>>,
}

#[derive(Clone)]
pub struct RuntimeHandle {
    inner: Arc<Inner>,
}

struct Inner {
    tasks: Arc<TaskPool>,
    blocking_tasks: BlockingTaskPool,
    next_task_id: AtomicU64,
    maintenance: ConditionFlag,
    on_inactivity: Option<Mutex<Box<dyn FnMut() + Send + 'static>>>,
    on_maintenance: Option<Mutex<Box<dyn FnMut(&RuntimeHandle) + Send + 'static>>>,
    shutdown: AtomicBool,
}

#[derive(Default)]
struct ConditionFlag {
    // The bool is used to indicate that the condition was signalled at least once, both as a
    // fast-path for `wait` and as a means of preventing an edge case deadlock when the condition
    // and notification occurred after the condition is checked but before a wait() is called.
    // In other words, `notify_one()` will always allow at least one `wait()` to proceed, whether
    // it's called before or after `notify_one()`.
    lock: Mutex<bool>,
    condition: Condvar,
}

#[derive(Default)]
struct BlockingTaskPool {
    tasks: Mutex<VecDeque<BlockingTask>>,
    waiting: Condvar,
    size: Mutex<BlockingPoolSize>,
    made_progress: AtomicBool,
    made_progress_last: AtomicBool,
    currently_running: AtomicUsize,
    bored: AtomicUsize,
}

#[derive(Default)]
struct BlockingPoolSize {
    actual: usize,
    target: usize,
}

type BlockingTaskFn = ClosureOnceT<'static, UnsyncSend, (), ()>;

struct BlockingTask {
    #[allow(dead_code)]
    id: u64,
    f: BlockingTaskFn,
}

struct Priorities {
    /// The current greatest priority (lowest numeric priority).
    current: RwLock<u32>,
    /// The number of ready queued tasks at the current priority.
    queued: AtomicUsize,
    /// The ready tasks of all lesser priorities (greater numeric priorities).
    /// The tuple contains the number of tasks that are currently queued and the unqueued ready tasks.
    ready: Mutex<BTreeMap<u32, (usize, Vec<Arc<Task>>)>>,
}

struct TaskPool {
    queue: work_queue::Queue<Arc<Task>>,
    waiting: ConditionFlag,
    priorities: Priorities,
    made_progress: AtomicBool,
    currently_polling: AtomicUsize,
}

struct WorkerQueue {
    queue: work_queue::LocalQueue<Arc<Task>>,
}

struct Task {
    #[allow(dead_code)]
    id: u64,
    priority: u32,
    future: TaskFuture,
    pool: Weak<TaskPool>,
    state: AtomicU8,
}

struct TaskFuture(Mutex<Option<BoxFuture<'static, ()>>>);

impl ConditionFlag {
    pub fn wait(&self) {
        let mut guard = self.lock.lock();
        if !std::mem::take(&mut *guard) {
            self.condition.wait(&mut guard);
        }
    }

    pub fn wait_for(&self, duration: std::time::Duration) {
        self.condition.wait_for(&mut self.lock.lock(), duration);
    }

    pub fn notify_one(&self) -> bool {
        let mut guard = self.lock.lock();
        *guard = true;
        self.condition.notify_one()
    }

    pub fn notify_all(&self) -> bool {
        self.condition.notify_all() > 0
    }
}

impl Default for Priorities {
    fn default() -> Self {
        Priorities {
            current: RwLock::new(u32::MAX),
            queued: Default::default(),
            ready: Default::default(),
        }
    }
}

impl Priorities {
    /// Indicate that a task is ready, returning the task if it should be queued immediately.
    pub fn ready(&self, task: Arc<Task>) -> Option<Arc<Task>> {
        let mut current = self.current.upgradable_read();
        loop {
            if task.priority == *current {
                // Task is the current priority level, increment the queued count
                self.queued.fetch_add(1, Ordering::Relaxed);
                log::trace!(
                    "task {}: queued + 1 == {}",
                    task.id,
                    self.queued.load(Ordering::Relaxed)
                );
                break Some(task);
            } else if task.priority < *current {
                // Task is a greater priority, change current/queued to reflect this, and make a
                // `ready` entry for the old (lesser) priority.
                let mut wcurrent = RwLockUpgradableReadGuard::upgrade(current);
                if task.priority >= *wcurrent {
                    current = RwLockWriteGuard::downgrade_to_upgradable(wcurrent);
                    continue;
                }
                log::trace!(
                    "task {}: changing highest priority to {} from {}",
                    task.id,
                    task.priority,
                    *wcurrent
                );
                let prev_priority = std::mem::replace(&mut *wcurrent, task.priority);
                let prev = self.queued.swap(1, Ordering::Relaxed);
                log::trace!("task {}: queued = 1", task.id);
                if prev > 0 {
                    self.ready.lock().insert(prev_priority, (prev, vec![]));
                }
                break Some(task);
            } else {
                // Task is a lesser priority, push it into the ready tasks.
                self.ready
                    .lock()
                    .entry(task.priority)
                    .or_default()
                    .1
                    .push(task);
                break None;
            }
        }
    }

    /// Verify that a task has the correct priority to be run right now.
    pub fn verify(&self, task: Arc<Task>) -> Option<Arc<Task>> {
        let current = self.current.read();
        if task.priority == *current {
            Some(task)
        } else {
            debug_assert!(
                task.priority > *current,
                "task {}: priority ({}) was unexpectedly higher than current ({})",
                task.id,
                task.priority,
                *current
            );
            let mut guard = self.ready.lock();
            let e = guard.get_mut(&task.priority).unwrap();
            e.0 -= 1;
            e.1.push(task);
            None
        }
    }

    /// Indicate that a task is not runnable (is pending), optionally returning the ready tasks
    /// that should now be queued. Tasks will be returned when the task was the last ready task
    /// with the given priority.
    pub fn pending(&self, task: &Task) -> Vec<Arc<Task>> {
        let current = self.current.upgradable_read();
        if task.priority == *current {
            if self.queued.fetch_sub(1, Ordering::Relaxed) == 1 {
                log::trace!("task {}: queued - 1 == 0", task.id);
                let mut current = RwLockUpgradableReadGuard::upgrade(current);
                // Once we have the write lock, verify that the state is still the same as prior to
                // acquiring the lock and get the new current priority and tasks.
                if task.priority == *current && self.queued.load(Ordering::Relaxed) == 0 {
                    log::trace!("task {}: queued - 1 still == 0", task.id);
                    let mut guard = self.ready.lock();
                    if let Some(new_current) = guard.iter().next().map(|(k, _)| *k) {
                        log::trace!(
                            "task {}: changing (downgrading) highest priority to {} from {}",
                            task.id,
                            new_current,
                            *current
                        );
                        *current = new_current;
                        let (c, v) = guard.remove(&new_current).unwrap();
                        // Update the queued count.
                        self.queued.store(c + v.len(), Ordering::Relaxed);
                        // Return the tasks to be queued.
                        return v;
                    } else {
                        log::trace!(
                            "task {}: changing (downgrading) highest priority to {} from {}",
                            task.id,
                            u32::MAX,
                            *current
                        );
                        *current = u32::MAX;
                    }
                }
            } else {
                log::trace!(
                    "task {}: queued - 1 == {}",
                    task.id,
                    self.queued.load(Ordering::Relaxed)
                );
            }
        } else {
            // This case can happen, but should happen infrequently (`verify` should catch things
            // earlier).
            debug_assert!(
                task.priority > *current,
                "task {}: priority ({}) was unexpectedly higher than current ({})",
                task.id,
                task.priority,
                *current
            );
            let mut guard = self.ready.lock();
            let e = guard.get_mut(&task.priority).unwrap();
            e.0 -= 1;
        }
        vec![]
    }
}

impl TaskPool {
    pub fn new(pool_size: usize) -> Self {
        TaskPool {
            queue: work_queue::Queue::new(pool_size, WORKER_QUEUE_SIZE),
            waiting: Default::default(),
            priorities: Default::default(),
            made_progress: AtomicBool::new(false),
            currently_polling: AtomicUsize::new(0),
        }
    }

    pub fn worker_queues(&self) -> Vec<WorkerQueue> {
        self.queue
            .local_queues()
            .map(|queue| WorkerQueue { queue })
            .collect()
    }
}

impl WorkerQueue {
    pub fn push(&mut self, task: Arc<Task>) {
        self.queue.push(task);
    }

    pub fn push_yield(&mut self, task: Arc<Task>) {
        self.queue.push_yield(task);
    }

    pub fn pop(&mut self) -> Option<Arc<Task>> {
        self.queue.pop()
    }
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
const TRANSITIONING: u8 = 2;
const COMPLETE: u8 = 3;

impl std::task::Wake for Task {
    fn wake(self: Arc<Self>) {
        if self
            .state
            .compare_exchange(PENDING, READY, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok()
        {
            log::trace!("task {}: PENDING -> READY (wake)", self.id);
            if let Some(pool) = self.pool.upgrade() {
                pool.is_ready(self);
            }
        }
    }
}

#[cfg(debug_assertions)]
impl Drop for Task {
    fn drop(&mut self) {
        log::trace!(
            "task {}: {} (dropping)",
            self.id,
            self.state.load(Ordering::Relaxed)
        );
        debug_assert!(
            self.state.load(Ordering::Relaxed) != READY,
            "task {} was READY when dropping",
            self.id
        );
    }
}

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

impl Default for Builder {
    fn default() -> Self {
        Builder {
            pool_size: 1,
            inactivity: None,
            maintenance: None,
        }
    }
}

impl Builder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn pool_size(mut self, pool_size: usize) -> Self {
        self.pool_size = pool_size;
        self
    }

    pub fn on_inactivity<F: FnMut() + Send + 'static>(mut self, callback: F) -> Self {
        self.inactivity = Some(Box::new(callback));
        self
    }

    pub fn on_maintenance<F: FnMut(&RuntimeHandle) + Send + 'static>(
        mut self,
        callback: F,
    ) -> Self {
        self.maintenance = Some(Box::new(callback));
        self
    }

    pub fn build(self) -> std::io::Result<Runtime> {
        let rt = Runtime {
            handle: RuntimeHandle {
                inner: Arc::new(Inner {
                    tasks: Arc::new(TaskPool::new(self.pool_size)),
                    blocking_tasks: Default::default(),
                    next_task_id: Default::default(),
                    maintenance: Default::default(),
                    on_inactivity: self.inactivity.map(Mutex::new),
                    on_maintenance: self.maintenance.map(Mutex::new),
                    shutdown: Default::default(),
                }),
            },
        };

        // Normal pool
        for (i, wq) in rt
            .handle
            .inner
            .tasks
            .worker_queues()
            .into_iter()
            .enumerate()
        {
            let handle = rt.handle.clone();
            std::thread::Builder::new()
                .name(format!("ergo pool {}", i))
                .spawn(move || handle.pool_worker(wq))?;
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
}

impl Runtime {
    pub fn builder() -> Builder {
        Default::default()
    }

    pub fn shutdown(&self) {
        self.inner.indicate_shutdown();
    }

    #[allow(dead_code)]
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

/// An executor that allows blocking on an async task by parking the current thread.
#[derive(Default)]
pub struct MiniExecutor(());

impl MiniExecutor {
    pub fn block_on<'a, Fut: std::future::Future<Output = R> + 'a, R: 'a>(
        &self,
        mut fut: Fut,
    ) -> R {
        let block_on_waker = BlockOnWaker::new();
        use std::task::{Context, Poll, Waker};
        let waker = Waker::from(block_on_waker.clone());
        // Safety: we guarantee that `fut` will not be moved after this call by overwriting the
        // `fut` binding itself.
        let mut fut = unsafe { Pin::new_unchecked(&mut fut) };
        loop {
            if let Poll::Ready(ret) = fut.as_mut().poll(&mut Context::from_waker(&waker)) {
                break ret;
            }
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
    pub fn spawn_blocking(&self, f: BlockingTaskFn) {
        let id = self.inner.next_task_id.fetch_add(1, Ordering::Relaxed);
        self.inner.blocking_tasks.new_task(id, f);
    }

    /// Poll the given future to completion.
    pub fn block_on<'a, Fut: std::future::Future<Output = ()> + 'a>(&self, mut fut: Fut) {
        let block_on_waker = BlockOnWaker::new();
        use std::task::{Context, Waker};
        let waker = Waker::from(block_on_waker.clone());
        // Safety: we guarantee that `fut` will not be moved after this call by overwriting the
        // `fut` binding itself.
        let mut fut = unsafe { Pin::new_unchecked(&mut fut) };
        let tasks = &self.inner.tasks;
        loop {
            tasks.made_progress.store(true, Ordering::Relaxed);
            tasks.currently_polling.fetch_add(1, Ordering::Relaxed);
            let result = fut.as_mut().poll(&mut Context::from_waker(&waker));
            tasks.currently_polling.fetch_sub(1, Ordering::Relaxed);
            if result.is_ready() {
                break;
            }
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

    /// Returns whether there are any blocking tasks in the pool.
    pub fn has_blocking_tasks(&self) -> bool {
        let blocking_tasks_ready = !self.inner.blocking_tasks.tasks.lock().is_empty();
        let blocking_tasks_running = self
            .inner
            .blocking_tasks
            .currently_running
            .load(Ordering::Relaxed)
            > 0;
        let made_progress = self
            .inner
            .blocking_tasks
            .made_progress_last
            .load(Ordering::Relaxed);
        blocking_tasks_ready || blocking_tasks_running || made_progress
    }

    /// Returns whether there are any tasks currently polling.
    pub fn has_polling_tasks(&self) -> bool {
        self.inner.tasks.currently_polling.load(Ordering::Relaxed) > 0
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

    fn pool_worker(&self, worker_queue: WorkerQueue) {
        // Safety: the cell provides a valid pointer.
        WORKER_QUEUE.with(|r| *(unsafe { r.get().as_mut() }.unwrap()) = Some(worker_queue));
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
        let mut inactive_intervals = 0;
        while !self.inner.shutdown() {
            // Blocking pool resize logic
            {
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
            // Inactivity logic
            if let Some(cb) = &self.inner.on_inactivity {
                let tasks_made_progress = self
                    .inner
                    .tasks
                    .made_progress
                    .swap(false, Ordering::Relaxed);
                {
                    let blocking_tasks_made_progress = self
                        .inner
                        .blocking_tasks
                        .made_progress
                        .swap(false, Ordering::Relaxed);
                    self.inner
                        .blocking_tasks
                        .made_progress_last
                        .store(blocking_tasks_made_progress, Ordering::Relaxed);
                }
                let tasks_polling = self.has_polling_tasks();
                let has_blocking_tasks = self.has_blocking_tasks();
                if tasks_made_progress || tasks_polling || has_blocking_tasks {
                    inactive_intervals = 0;
                } else {
                    inactive_intervals += 1;
                    if inactive_intervals >= INACTIVITY_INTERVALS {
                        (*cb.lock())();
                        inactive_intervals = 0;
                    }
                }
            }
            // General maintenance callback
            if let Some(cb) = &self.inner.on_maintenance {
                (*cb.lock())(self);
            }
            self.inner.maintenance.wait_for(MAINTENANCE_INTERVAL);
        }
    }
}

fn worker_queue<R, F: FnOnce(Option<&mut WorkerQueue>) -> R>(f: F) -> R {
    // Safety: the cell provides a valid pointer
    WORKER_QUEUE.with(|r| f(unsafe { r.get().as_mut() }.unwrap().as_mut()))
}

impl TaskPool {
    pub fn new_task(self: &Arc<Self>, id: u64, priority: u32, future: BoxFuture<'static, ()>) {
        let task = Arc::new(Task {
            id,
            priority,
            future: TaskFuture::new(future),
            pool: Arc::downgrade(self),
            state: AtomicU8::new(READY),
        });
        log::trace!("task {}: READY (created)", id);
        if let Some(task) = self.priorities.ready(task) {
            self.push(task);
        }
    }

    fn push(&self, task: Arc<Task>) {
        worker_queue(|wq| {
            if let Some(wq) = wq {
                wq.push(task);
            } else {
                self.queue.push(task);
            }
        });
        self.waiting.notify_one();
    }

    fn push_yield(&self, task: Arc<Task>) {
        worker_queue(|wq| {
            if let Some(wq) = wq {
                wq.push_yield(task);
            } else {
                self.queue.push(task);
            }
        });
        self.waiting.notify_one();
    }

    pub fn is_ready(&self, task: Arc<Task>) {
        if let Some(task) = self.priorities.ready(task) {
            self.push_yield(task);
        }
    }

    fn pop_priority_task(&self) -> Option<Arc<Task>> {
        loop {
            let task = worker_queue(|wq| wq.unwrap().pop())?; // unwrap because this should only be called from a worker thread
            let ret = self.priorities.verify(task);
            if ret.is_some() {
                break ret;
            }
        }
    }

    // Return an Option to allow spurious wakeup for shutdown
    fn next_ready_task(&self, inner: &Inner) -> Option<Arc<Task>> {
        let mut task = self.pop_priority_task();
        let mut attempt = 0;
        while task.is_none() {
            if attempt < 10 {
                std::thread::sleep(std::time::Duration::from_millis(1));
                attempt += 1;
            } else {
                self.waiting.wait();
            }
            if inner.shutdown() {
                return None;
            }
            task = self.pop_priority_task();
        }
        task
    }

    pub fn try_run_task(&self, inner: &Inner) {
        if let Some(task) = self.next_ready_task(inner) {
            use std::task::Context;
            match task.state.compare_exchange(
                READY,
                TRANSITIONING,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    log::trace!("task {}: READY -> TRANSITIONING", task.id);
                    for t in self.priorities.pending(&task) {
                        self.push_yield(t);
                    }
                    task.state.store(PENDING, Ordering::Relaxed);
                    log::trace!("task {}: PENDING", task.id);
                    self.made_progress.store(true, Ordering::Relaxed);
                    self.currently_polling.fetch_add(1, Ordering::Relaxed);
                    if task
                        .future
                        .poll(&mut Context::from_waker(&task.clone().into()))
                    {
                        if task.state.swap(COMPLETE, Ordering::Relaxed) != READY {
                            log::trace!("task {}: COMPLETE", task.id);
                        }
                        // Otherwise next_ready_task will filter out the task when it comes up again.
                    }
                    self.currently_polling.fetch_sub(1, Ordering::Relaxed);
                }
                Err(other) if other == COMPLETE => {
                    // This can only happen if the task had been READY when made COMPLETE, which
                    // means we must indicate it as pending now to remove it's representation in
                    // the shared state. It's difficult to do this exactly when we transition to
                    // COMPLETE because there are no guarantees that the READY task is actually
                    // queued yet (thus no guarantees that it is represented in the shared state),
                    // whereas at this point it must have been queued.
                    for t in self.priorities.pending(&task) {
                        self.push_yield(t);
                    }
                    log::trace!("task {}: COMPLETE", task.id);
                }
                _ => (),
            }
        }
    }
}

impl BlockingTaskPool {
    pub fn new_task(&self, id: u64, f: BlockingTaskFn) {
        let task = BlockingTask { id, f };
        self.tasks.lock().push_back(task);
        self.waiting.notify_one();
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
            self.waiting.wait_for(&mut guard, MAINTENANCE_INTERVAL);
            return None;
        }
        let ret = guard.pop_front();
        debug_assert!(ret.is_some());
        ret
    }

    pub fn try_run_task(&self) {
        if let Some(BlockingTask { f, .. }) = self.next_task() {
            self.made_progress.store(true, Ordering::Relaxed);
            self.currently_running.fetch_add(1, Ordering::Relaxed);
            f.call();
            self.currently_running.fetch_sub(1, Ordering::Relaxed);
        }
    }
}

impl Inner {
    pub fn indicate_shutdown(&self) {
        self.shutdown.store(true, Ordering::Relaxed);
        self.tasks.waiting.notify_all();
        self.blocking_tasks.waiting.notify_all();
        self.maintenance.notify_all();
    }

    pub fn shutdown(&self) -> bool {
        self.shutdown.load(Ordering::Relaxed)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use futures::{channel::oneshot::channel, future::FutureExt};

    async fn spawn<F: Future + Send + 'static>(
        rt: RuntimeHandle,
        priority: u32,
        fut: F,
    ) -> F::Output
    where
        F::Output: Send + 'static,
    {
        let (fut, handle) = fut.remote_handle();
        rt.spawn(priority, BoxFuture::new(fut));
        handle.await
    }

    async fn spawn_blocking<R: Send + 'static, F: FnOnce() -> R + Send + Sync + 'static>(
        rt: RuntimeHandle,
        f: F,
    ) -> R {
        let (snd, rcv) = channel();
        rt.spawn_blocking(
            (move || {
                snd.send(f()).map_err(|_| ()).unwrap();
                ()
            })
            .into(),
        );
        rcv.await.unwrap()
    }

    fn block<F: Future + Send>(rt: &RuntimeHandle, fut: F) -> F::Output {
        let (snd, mut rcv) = channel();
        rt.block_on(async move { snd.send(fut.await).map_err(|_| ()).unwrap() });
        rcv.try_recv().unwrap().unwrap()
    }

    fn rt(pool_size: usize) -> Runtime {
        Runtime::builder().pool_size(pool_size).build().unwrap()
    }

    #[test]
    fn basic() {
        let rt = rt(2);
        let a = async { 42 };
        let f = spawn(rt.handle(), 100, a);
        let result = block(&rt, f);
        assert_eq!(result, 42);
    }

    #[test]
    fn multiple() {
        let rt = rt(2);
        let a = spawn(rt.handle(), 100, async { 1 });
        let b = spawn(rt.handle(), 100, async { 2 });
        let c = spawn(rt.handle(), 100, async { 3 });
        let d = spawn(rt.handle(), 100, async { 4 });
        let e = spawn(rt.handle(), 100, async { 5 });
        let result = block(&rt, async { futures::join!(a, b, c, d, e) });
        assert_eq!(result, (1, 2, 3, 4, 5));
    }

    #[test]
    fn priorities() {
        let rt = rt(2);
        let a = spawn(rt.handle(), 100, async { 1 });
        let b = spawn(rt.handle(), 100, async { a.await });
        let c = spawn(rt.handle(), 80, async { 3 });
        let d = spawn(rt.handle(), 80, async { 4 });
        let e = spawn(rt.handle(), 80, async { futures::join!(c, d) });
        let result = block(&rt, async { futures::join!(e, b) });
        assert_eq!(result, ((3, 4), 1));
    }

    #[test]
    fn many() {
        let rt = rt(2);
        let a = {
            let hdl = rt.handle();
            spawn(rt.handle(), 100, async move {
                for _ in 0..1000 {
                    spawn(hdl.clone(), 100, async { () }).await;
                }
            })
        }
        .shared();
        let b = {
            let hdl = rt.handle();
            spawn(rt.handle(), 100, async move {
                for _ in 0..1000 {
                    spawn(hdl.clone(), 80, async { () }).await;
                }
            })
        }
        .shared();
        let c = {
            let hdl = rt.handle();
            spawn(rt.handle(), 100, async move {
                for _ in 0..10 {
                    spawn(hdl.clone(), 50, a.clone()).await;
                }
            })
        };
        let d = {
            let hdl = rt.handle();
            spawn(rt.handle(), 100, async move {
                for _ in 0..10 {
                    spawn(hdl.clone(), 100, b.clone()).await;
                }
            })
        };
        let result = block(&rt, async { futures::join!(c, d) });
        assert_eq!(result, ((), ()));
    }

    #[test]
    fn blocking() {
        let rt = rt(2);
        let mut g = vec![];
        for n in 2..100 {
            g.push(
                spawn_blocking(rt.handle(), move || {
                    std::thread::sleep(std::time::Duration::from_millis(n / 2))
                })
                .boxed(),
            );
        }
        block(&rt, futures::future::join_all(g));
    }

    #[test]
    fn priority_order() {
        let rt = rt(1);
        let hdl = rt.handle();
        let v = Arc::new(AtomicU64::new(0));
        let v2 = v.clone();
        let f = spawn(hdl.clone(), 100, async move {
            let v = v2.clone();
            let a = spawn(hdl.clone(), 5, async move {
                drop(v.compare_exchange(4, 5, Ordering::Relaxed, Ordering::Relaxed));
            });
            let v = v2.clone();
            let b = spawn(hdl.clone(), 2, async move {
                drop(v.compare_exchange(1, 2, Ordering::Relaxed, Ordering::Relaxed));
            });
            let v = v2.clone();
            let c = spawn(hdl.clone(), 6, async move {
                drop(v.compare_exchange(5, 6, Ordering::Relaxed, Ordering::Relaxed));
            });
            let v = v2.clone();
            let d = spawn(hdl.clone(), 1, async move {
                drop(v.compare_exchange(0, 1, Ordering::Relaxed, Ordering::Relaxed));
            });
            let v = v2.clone();
            let e = spawn(hdl.clone(), 4, async move {
                drop(v.compare_exchange(3, 4, Ordering::Relaxed, Ordering::Relaxed));
            });
            let v = v2.clone();
            let f = spawn(hdl.clone(), 3, async move {
                drop(v.compare_exchange(2, 3, Ordering::Relaxed, Ordering::Relaxed));
            });
            futures::join!(a, b, c, d, e, f);
        });
        block(&rt, f);
        assert_eq!(v.load(Ordering::Relaxed), 6);
    }
}
