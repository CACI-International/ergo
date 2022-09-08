//! Concurrent task creation.

use ergo_runtime::abi_stable::{
    external_types::RMutex,
    std_types::{RArc, ROption, RString},
    StableAbi,
};
use ergo_runtime::context::{DynamicScopeKey, Log, LogTask, RecordingWork, TaskPermit, Work};
use ergo_runtime::{error::DiagnosticInfo, metadata::Source, nsid, traits, types, Context, Value};

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
            let n = traits::into::<types::Number>(v).await?;
            n.as_ref().to_u32().add_primary_label(
                Source::get(&n).with("expected this to be an unsigned integer"),
            )?
        }
        None => 0,
    };

    let mut priority = match priority {
        Some(v) => {
            let src = Source::get(&v);
            let priority = {
                let n = traits::into::<types::Number>(v).await?;
                n.as_ref().to_u32().add_primary_label(
                    Source::get(&n).with("expected this to be an unsigned integer"),
                )?
            };
            if priority < 1 || priority > 1000 {
                Err(
                    ergo_runtime::error::Diagnostic::from("priority must fall in [1,1000]")
                        .add_primary_label(src.with(""))
                        .add_note(format_args!("priority was {}", priority)),
                )?;
            }
            priority
        }
        None => 500,
    };

    priority += SCRIPT_TASK_PRIORITY_OFFSET;

    let work_id = match track_work_by {
        Some(v) => v.id().await,
        None => value.id().await,
    };

    let description = description.into_owned().0;

    let log = Context::global().log.sublog("task");
    let work = RArc::new(RMutex::new(ROption::RSome(
        log.work(format!("{:x}", work_id)),
    )));

    let do_task = ergo_runtime::lazy_value! {
        #![contains(value)]
        let parent_task =
            ParentTask::new(description.clone(), count, log.clone(), work.clone()).await;
        let mut value = value;
        Context::spawn(
            priority,
            |ctx| {
                ctx.dynamic_scope
                    .set(&ARGS_SOURCE.with(ParentTaskKey), parent_task)
            },
            async move {
                log.info(format!("starting: {}", &description));
                let ret = Context::eval(&mut value).await;
                let errored = ret.is_err();
                let mut work = work.lock().take();
                if errored {
                    if let ROption::RSome(w) = &mut work {
                        w.err();
                    }
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
        .await?
    };

    let call_deps = CALL_DEPENDS.clone();

    // Return an impure value, so that it will always be evaluated (without caching results). This
    // allows multiple tasks to properly suspend when relying on the same task.
    let mut value = ergo_runtime::lazy_value! {
        #![contains(do_task)]
        #![depends(nsid!(std::task::parent), ^CALL_DEPENDS)]

        // There's a bit of subtlety as to why we return a value here rather than immediately
        // awaiting the task. If we have concurrent values awaiting the result of a `task` call, we
        // want them both to suspend the calling task. However due to the way evaluation of values
        // works, one will start evaluating this value (and suspend) whereas the other will be
        // descheduled but won't suspend the calling task (as it will be waiting for the value to
        // be evaluated). So, instead we immediately return the appropriate (unique/new) value in
        // both cases, which won't conflict with each other (as this value is impure and always
        // produces new values) and will allow each to proceed to properly suspend while the task
        // runs.
        match Context::with(|ctx| ctx.dynamic_scope.get(&ParentTaskKey)) {
            Some(p) => {
                let p = p.clone();
                ergo_runtime::lazy_value! {
                    #![contains(do_task)]
                    #![depends(nsid!(std::task::result), ^call_deps)]
                    let mut do_task = do_task;
                    p.suspend(async move { Context::eval(&mut do_task).await?; ergo_runtime::Result::Ok(do_task) }).await?
                }
            }
            None => ergo_runtime::lazy_value! {
                #![contains(do_task)]
                #![depends(nsid!(std::task::result), ^call_deps)]
                let mut do_task = do_task;
                Context::eval(&mut do_task).await?;
                do_task
            },
        }
    };
    value.impure(true);
    value
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
    work: RArc<RMutex<ROption<Work>>>,
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
        work: RArc<RMutex<ROption<Work>>>,
    ) -> Self {
        let pt = ParentTask {
            description,
            count,
            log,
            work,
            state: RMutex::new(ParentTaskState::Suspended(0)),
        };
        pt.start().await;
        pt
    }

    /// Suspend the task while running the given future.
    pub async fn suspend<Fut: std::future::Future>(&self, f: Fut) -> Fut::Output {
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
            self.start().await;
        }
        ret
    }

    /// Start the task if applicable.
    async fn start(&self) {
        let task_permit = Context::global().task.task_acquire(self.count).await;
        let mut guard = self.state.lock();
        match &mut *guard {
            ParentTaskState::Suspended(0) => {
                *guard = ParentTaskState::Active(
                    self.log.task(self.description.clone()),
                    self.work.lock().as_mut().unwrap().start(),
                    task_permit,
                );
            }
            _ => drop(task_permit),
        }
    }
}
