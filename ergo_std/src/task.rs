//! Concurrent task creation.

use ergo_runtime::abi_stable::{
    external_types::RMutex,
    std_types::{RArc, RString},
    StableAbi,
};
use ergo_runtime::context::{
    DynamicScopeKey, Log, LogTask, RecordingWork, TaskManager, TaskPermit, Work,
};
use ergo_runtime::{metadata::Source, nsid, traits, try_result, types, Value};

pub const SCRIPT_TASK_PRIORITY_OFFSET: u32 = 1000;

#[types::ergo_fn]
/// Evaluate the given value as a concurrent task.
///
/// Arguments: `(String :description) :value`
///
/// Keyed Arguments:
/// * `Into<Number> :count`: An integer indicating the number of task slots the task should use.
/// Defaults to 0.
/// * `:track-work-by`: A value the identity of which is used as the identifier for work tracking.
/// If unset, the task value identity is used. For instance, you might use this to indicate the
/// values which affect a task's runtime, so that progress estimation may be able to predict how
/// long the task will take based on prior runs.
/// * `Into<Number> :priority`: An integer from 1 to 1000 indicating the priority of the task. Lower
/// numbers have higher priority (i.e. will run earlier). Default 500.
///
/// Returns the result of evaluating `value` by running it in a concurrent task (which may run on a
/// separate thread) that is described by `description`.
///
/// When a value running in a task waits on another task, it is considered inactive. A task is considered a single unit of
/// work in the runtime progress tracking. An active task takes up `task-count` slots out of the total permitted number of
/// concurrent tasks at a single time (as configured in the runtime). If there are not enough slots, the task will wait
/// until more become available. If the requested number is greater than the maximum, it will be limited to the maximum.
pub async fn function(
    description: types::String,
    value: _,
    (count): [_],
    (track_work_by): [_],
    (priority): [_],
) -> Value {
    let count = match count {
        Some(v) => {
            let n = try_result!(traits::into::<types::Number>(CONTEXT, v).await);
            try_result!(Source::extract(n)
                .map(|n| n.as_ref().to_u32().ok_or("expected unsigned integer"))
                .transpose_err()
                .map_err(|e| e.into_error()))
        }
        None => 0,
    };

    let mut priority = match priority {
        Some(v) => {
            let src = Source::get(&v);
            let priority = {
                let n = try_result!(traits::into::<types::Number>(CONTEXT, v).await);
                try_result!(Source::extract(n)
                    .map(|n| n.as_ref().to_u32().ok_or("expected unsigned integer"))
                    .transpose_err()
                    .map_err(|e| e.into_error()))
            };
            if priority < 1 || priority > 1000 {
                return src
                    .with("priority must fall in [1,1000]")
                    .into_error()
                    .into();
            }
            priority
        }
        None => 500,
    };

    priority += SCRIPT_TASK_PRIORITY_OFFSET;

    let work_id = match track_work_by {
        Some(v) => v.id(),
        None => value.id(),
    };

    let description = description.to_owned().0;

    // XXX if more than one task relies on a single task, the task state of only one of them will
    // be correct (the others will not correctly be suspended because only one will actually
    // execute the `task ...` function)
    let log = CONTEXT.log.sublog("task");
    let work = RArc::new(RMutex::new(log.work(format!("{:x}", work_id))));

    let fut = async {
        let parent_task = ParentTask::new(
            description.clone(),
            count,
            log.clone(),
            work.clone(),
            &CONTEXT.task,
        )
        .await;
        let mut value = value;
        CONTEXT
            .spawn(
                priority,
                |ctx| {
                    ctx.dynamic_scope
                        .set(&ARGS_SOURCE.with(ParentTaskKey), parent_task)
                },
                move |ctx| async move {
                    log.info(format!("starting: {}", &description));
                    let ret = ctx.eval(&mut value).await;
                    let errored = ret.is_err();
                    if errored {
                        work.lock().err();
                    }
                    log.info(format!(
                        "complete{}: {}",
                        if errored { " (failed)" } else { "" },
                        description
                    ));
                    ret?;
                    Ok(value)
                },
            )
            .await
    };

    let res = match CONTEXT.dynamic_scope.get(&ParentTaskKey) {
        Some(p) => p.suspend(&CONTEXT.task, fut).await,
        None => fut.await,
    };
    try_result!(res)
}

pub struct ParentTaskKey;

impl DynamicScopeKey for ParentTaskKey {
    type Value = ParentTask;

    fn id(&self) -> u128 {
        nsid!(std::task::key).as_u128()
    }
}

#[derive(StableAbi)]
#[repr(C)]
pub struct ParentTask {
    pub description: RString,
    count: u32,
    log: Log,
    work: RArc<RMutex<Work>>,
    state: RMutex<ParentTaskState>,
}

impl Drop for ParentTask {
    fn drop(&mut self) {
        // Change state to inactive to record any final values bound to dropping (like that of
        // RecordingWork). It should already be inactive generally.
        *self.state.lock() = ParentTaskState::Suspended(0);
    }
}

#[derive(StableAbi)]
#[repr(C)]
enum ParentTaskState {
    Active(LogTask, RecordingWork, TaskPermit),
    Suspended(usize),
}

impl ParentTask {
    /// Create a new ParentTask and start it.
    pub async fn new(
        description: RString,
        count: u32,
        log: Log,
        work: RArc<RMutex<Work>>,
        task_manager: &TaskManager,
    ) -> Self {
        let pt = ParentTask {
            description,
            count,
            log,
            work,
            state: RMutex::new(ParentTaskState::Suspended(0)),
        };
        pt.start(task_manager).await;
        pt
    }

    /// Suspend the task while running the given future.
    pub async fn suspend<Fut: std::future::Future>(
        &self,
        task_manager: &TaskManager,
        f: Fut,
    ) -> Fut::Output {
        {
            let mut state = self.state.lock();
            match &mut *state {
                ParentTaskState::Suspended(n) => *n += 1,
                _ => *state = ParentTaskState::Suspended(1),
            }
        }
        let ret = f.await;
        let start = {
            let mut guard = self.state.lock();
            match &mut *guard {
                ParentTaskState::Suspended(n) => {
                    *n -= 1;
                    *n == 0
                }
                _ => panic!("invalid parent task state"),
            }
        };
        if start {
            self.start(task_manager).await;
        }
        ret
    }

    /// Start the task if applicable.
    async fn start(&self, task_manager: &TaskManager) {
        let task_permit = task_manager.task_acquire(self.count).await;
        let mut guard = self.state.lock();
        match &mut *guard {
            ParentTaskState::Suspended(0) => {
                *guard = ParentTaskState::Active(
                    self.log.task(self.description.clone()),
                    self.work.lock().start(),
                    task_permit,
                );
            }
            _ => drop(task_permit),
        }
    }
}
