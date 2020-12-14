//! Concurrent task creation.

use abi_stable::{
    external_types::RMutex,
    std_types::{RArc, RString},
    StableAbi,
};
use ergo_runtime::{ergo_function, types, ContextExt};
use grease::error::UniqueErrorSources;
use grease::future::eager::Eager;
use grease::runtime::{LogTask, RecordingWork, ScopeTaskLocal, TaskLocal, TaskLocalRef, TaskPermit};
use grease::task_local_key;
use grease::value::{Errored, Value};
use std::str::FromStr;
use std::sync::{Arc, Mutex};

pub fn function() -> Value {
    ergo_function!(independent std::task,
    r"Run the given value as a concurrent task.

Arguments: <description: String> <value>

Keyword Arguments:
* <task-count: String>: A numeric string indicating the number of task slots the task should use.
  Defaults to 0.
* <track-work-by>: A value the identity of which is used as the identifier for work tracking. If unset, the task value
  identity is used. For instance, you might use this to indicate the values which affect a task's runtime, so that
  progress estimation may be able to predict how long the task will take based on prior runs.

Returns a value identical to `value` (including the identity), however evaluating the value will run it in a concurrent
task (which may run on a separate thread) that is described by `description`.

When a value running in a task waits on another task, it is considered inactive. A task is considered a single unit of
work in the runtime progress tracking. An active task takes up `task-count` slots out of the total permitted number of
concurrent tasks at a single time (as configured in the runtime). If there are not enough slots, the task will wait
until more become available. If the requested number is greater than the maximum, it will be limited to the maximum.",
    |ctx| {
        let desc = ctx.args.next().ok_or("no task description")?;
        let val = ctx.args.next().ok_or("no argument to task")?.unwrap();

        let task_count = if let Some(v) = ctx.args.kw("task-count") {
            let v = ctx.source_value_as::<types::String>(v);
            let v = v.await?.await.transpose_ok()?;
            v.map(|v| u32::from_str(v.as_ref()).map_err(|_| "expected unsigned integer")).transpose_err()
                .map_err(|e| e.into_grease_error())?
        } else {
            0
        };

        let work_id = if let Some(v) = ctx.args.kw("track-work-by") {
            v.id()
        } else {
            val.id()
        };

        ctx.unused_arguments()?;

        let desc = ctx.source_value_as::<types::String>(desc);
        let desc = desc.await?.unwrap();

        let observed_errors = Arc::new(Mutex::new(ObservedErrors::default()));
        let observed_errors_each = observed_errors.clone();
        let task = ctx.task.clone();
        let log = ctx.log.sublog("task");
        let work = log.work(format!("{:x}", work_id));
        val.map_data(move |inner| Eager::Pending(async move {
            let task_inner = task.clone();

            task.spawn(Errored::observe(move |e| {
                if let Ok(mut oe) = observed_errors.lock() { oe.call(e) }
            }, async move {
                let s = desc.await?;
                let work = RArc::new(RMutex::new(work));
                let parent_task = ParentTask::new(s.clone().owned().0, task_count, log.clone(), work.clone(), task_inner.clone());
                log.info(format!("starting: {}", s.clone()));
                let ret = parent_task.run_scoped(inner.into_future()).await;
                let errored = ret.is_err();
                if errored {
                    work.lock().err();
                }
                log.info(format!("complete{}: {}", if errored { " (failed)" } else { "" }, s));
                ret
            }))
            .await
        })).for_each(|| {
            match ParentTask::task_local() {
                None => None,
                Some(v) => Some(&*v as *const ParentTask as usize)
            }
        }, move |res| {
            let observed_errors_each = observed_errors_each.clone();
            async move {
                if let Some(e) = Errored::task_local() {
                    if let Ok(mut oe) = observed_errors_each.lock() {
                        oe.register(e);
                    }
                }
                res.await
            }
        })
    })
    .into()
}

#[derive(Default)]
struct ObservedErrors {
    errors: UniqueErrorSources,
    observers: Vec<grease::runtime::TaskLocalRef<Errored>>,
}

impl ObservedErrors {
    pub fn call(&mut self, err: grease::Error) {
        if self.errors.insert(err.clone()) {
            for o in self.observers.iter() {
                o.call(err.clone());
            }
        }
    }

    pub fn register(&mut self, observer: grease::runtime::TaskLocalRef<Errored>) {
        for e in self.errors.iter() {
            observer.call(e.clone());
        }
        self.observers.push(observer);
    }
}

#[derive(StableAbi)]
#[repr(C)]
pub struct ParentTask {
    pub description: RString,
    task_count: u32,
    log: grease::runtime::Log,
    work: RArc<RMutex<grease::runtime::Work>>,
    task_manager: grease::runtime::TaskManager,
    state: RMutex<ParentTaskState>,
}

impl TaskLocal for ParentTask {
    fn task_local_key() -> u128 {
        task_local_key!(ergo_std::parent_task)
    }
}

impl Drop for ParentTask {
    fn drop(&mut self) {
        // Change state to inactive to record any final values bound to dropping (like that of
        // RecordingWork). It should already be inactive generally.
        *self.state.lock() = ParentTaskState::Inactive;
    }
}

#[derive(StableAbi)]
#[repr(C)]
enum ParentTaskState {
    Active(usize, LogTask, RecordingWork, TaskPermit),
    ActivePending(grease::future::BoxFuture<'static, grease::runtime::TaskPermit>),
    Inactive,
}

/// A handle to an active task.
///
/// While this is retained, the attributed task is considered active.
pub struct ActiveTask {
    task: TaskLocalRef<ParentTask>
}

impl Drop for ActiveTask {
    fn drop(&mut self) {
        let mut guard = self.task.state.lock();
        match &mut *guard {
            ParentTaskState::Active(0, _, _, _) => *guard = ParentTaskState::Inactive,
            ParentTaskState::Active(n, _, _, _) => *n -= 1,
            _ => panic!("invalid task state"),
        }
    }
}

pub struct Run<Fut>(Fut);

impl<Fut: std::future::Future> std::future::Future for Run<Fut> {
    type Output = Fut::Output;

    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context) -> std::task::Poll<Fut::Output> {
        let task = ParentTask::task_local().expect("ParentTask not set");
        let _active = {
            {
                let mut guard = task.state.lock();
                loop {
                    match &mut *guard {
                        ParentTaskState::Active(n, _, _, _) => {
                            *n += 1;
                            break
                        }
                        ParentTaskState::ActivePending(p) => {
                            // Safety: p is a BoxFuture, it will not move
                            match std::future::Future::poll(unsafe { std::pin::Pin::new_unchecked(p) }, cx) {
                                std::task::Poll::Pending => return std::task::Poll::Pending,
                                std::task::Poll::Ready(task_permit) => {
                                    *guard = ParentTaskState::Active(
                                        0,
                                        task.log.task(task.description.clone()),
                                        task.work.lock().start(),
                                        task_permit,
                                    );
                                    break
                                }
                            }
                        }
                        ParentTaskState::Inactive => {
                            *guard = ParentTaskState::ActivePending(
                                // Safety: the inner state is cleared prior to being dropped, and holds the reference to the task manager.
                                unsafe {
                                    std::mem::transmute::<grease::future::BoxFuture<'_, _>, grease::future::BoxFuture<'static, _>>(
                                        grease::future::BoxFuture::new(task.task_manager.task_acquire(task.task_count))
                                    )
                                }
                            );
                        }
                    }
                }
            }

            ActiveTask { task }
        };

        unsafe { self.map_unchecked_mut(|p| &mut p.0) }.poll(cx)
    }
}

impl ParentTask {
    /// Create a new (inactive) ParentTask.
    pub fn new(
        description: RString,
        task_count: u32,
        log: grease::runtime::Log,
        work: RArc<RMutex<grease::runtime::Work>>,
        task_manager: grease::runtime::TaskManager,
    ) -> Self {
        ParentTask {
            description,
            task_count,
            log,
            work,
            task_manager,
            state: RMutex::new(ParentTaskState::Inactive)
        }
    }

    /// Consider the current task active until the returned guard is dropped.
    ///
    /// Returns None if there is no active task.
    pub fn remain_active() -> Option<ActiveTask> {
        let task = match Self::task_local() {
            None => return None,
            Some(task) => task
        };
        {
            let mut guard = task.state.lock();
            match &mut *guard {
                ParentTaskState::Active(n, _, _, _) => *n += 1,
                _ => panic!("invalid task state"),
            }
        }
        Some(ActiveTask { task })
    }

    /// Run the given future with this ParentTask.
    ///
    /// This must be used for `remain_active` to work.
    pub fn run_scoped<Fut: std::future::Future>(self, fut: Fut) -> ScopeTaskLocal<Run<Fut>> {
        self.scoped(Run(fut))
    }
}
