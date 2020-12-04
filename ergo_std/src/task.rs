//! Concurrent task creation.

use abi_stable::{
    external_types::RMutex,
    std_types::{RArc, RString},
    StableAbi,
};
use ergo_runtime::{ergo_function, types, ContextExt};
use grease::error::UniqueErrorSources;
use grease::future::eager::Eager;
use grease::runtime::{LogTask, RecordingWork, TaskLocal, TaskPermit};
use grease::task_local_key;
use grease::value::{Errored, Value};
use std::str::FromStr;
use std::sync::{Arc, Mutex};

pub fn function() -> Value {
    ergo_function!(independent std::task, |ctx| {
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
        val.map_data(move |inner| Eager::Pending(async move {
            let task_inner = task.clone();

            task.spawn(Errored::observe(move |e| {
                if let Ok(mut oe) = observed_errors.lock() { oe.call(e) }
            }, async move {
                let s = desc.await?;
                log.info(format!("starting: {}", s.clone()));
                let work = RArc::new(RMutex::new(log.work(format!("{:x}", work_id))));
                let parent_task = ParentTask::new(s.clone().owned().0, task_count, log.clone(), work.clone(), task_inner.clone()).await;
                let ret = parent_task.scoped(inner.into_future()).await;
                let errored = ret.is_err();
                if errored {
                    work.lock().err();
                }
                log.info(format!("complete{}: {}", if errored { " (failed)" } else { "" }, s));
                ret
            }))
            .await
        })).for_each(move |res| {
            let observed_errors_each = observed_errors_each.clone();
            async move {
                let parent = ParentTask::task_local();
                if let Some(e) = Errored::task_local() {
                    if let Ok(mut oe) = observed_errors_each.lock() {
                        oe.register(e);
                    }
                }
                match parent {
                    Some(v) => v.run_child(res).await,
                    None => res.await
                }
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
struct ParentTask {
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
        // RecordingWork).
        *self.state.lock() = ParentTaskState::Inactive(0);
    }
}

#[derive(StableAbi)]
#[repr(C)]
enum ParentTaskState {
    Active(LogTask, RecordingWork, TaskPermit),
    Inactive(usize),
}

impl ParentTask {
    pub async fn new(
        description: RString,
        task_count: u32,
        log: grease::runtime::Log,
        work: RArc<RMutex<grease::runtime::Work>>,
        task_manager: grease::runtime::TaskManager,
    ) -> Self {
        let recording = work.lock().start();
        let state = RMutex::new(ParentTaskState::Active(
            log.task(description.clone()),
            recording,
            task_manager.task_acquire(task_count).await,
        ));
        ParentTask {
            description,
            task_count,
            log,
            work,
            task_manager,
            state,
        }
    }

    pub async fn run_child<Fut: std::future::Future>(&self, fut: Fut) -> Fut::Output {
        {
            let mut guard = self.state.lock();
            let make_inactive = match &*guard {
                ParentTaskState::Active(_, _, _) => true,
                _ => false,
            };

            if make_inactive {
                *guard = ParentTaskState::Inactive(0);
            }
            match &mut *guard {
                ParentTaskState::Active(_, _, _) => panic!("logic error"),
                ParentTaskState::Inactive(n) => *n += 1,
            }
        }
        // Safety: it is fairly safe to AssertUnwindSafe here since we will resume the unwind later
        // (without accessing `fut`), so the unwind safety of fut will depend on calling code once
        // the unwind is resumed.
        let ret = futures::future::FutureExt::catch_unwind(std::panic::AssertUnwindSafe(fut)).await;
        {
            let make_active = {
                let mut guard = self.state.lock();
                match &mut *guard {
                    ParentTaskState::Active(_, _, _) => panic!("logic error"),
                    ParentTaskState::Inactive(n) => *n -= 1,
                }

                if let ParentTaskState::Inactive(0) = &*guard {
                    true
                } else {
                    false
                }

                // Guard is dropped prior to await to acquire task.
            };

            if make_active {
                // Must await without holding guard or other locals.
                let task_permit = self.task_manager.task_acquire(self.task_count).await;
                let mut guard = self.state.lock();
                // Only set to Active state if still in the inactive and empty state (another child
                // may have come since the guard was last released).
                if let ParentTaskState::Inactive(0) = &*guard {
                    *guard = ParentTaskState::Active(
                        self.log.task(self.description.clone()),
                        self.work.lock().start(),
                        task_permit,
                    );
                }
            }
        }
        match ret {
            Ok(v) => v,
            Err(e) => std::panic::resume_unwind(e),
        }
    }
}
