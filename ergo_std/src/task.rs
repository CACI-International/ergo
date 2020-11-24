//! Concurrent task creation.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::future::eager::Eager;
use grease::runtime::TaskDescription;
use grease::value::Value;

pub fn function() -> Value {
    ergo_function!(independent std::task, |ctx| {
        let desc = ctx.args.next().ok_or("no task description")?;
        let val = ctx.args.next().ok_or("no argument to task")?.unwrap();

        ctx.unused_arguments()?;

        let desc = ctx.source_value_as::<types::String>(desc);
        let desc = desc.await?.unwrap();

        let task = ctx.task.clone();
        let log = ctx.log.sublog("task");
        val.map_data(|inner| Eager::Pending(async move {
            let task_inner = task.clone();
            task.spawn(async move {
                let s = desc.await?;
                let task_description = TaskDescription { description: s.clone().owned() };
                task_inner.clone().scope_task_local(task_description, async move {
                    log.info(format!("starting: {}", s.clone()));
                    let ret = {
                        let _key = log.task(s.clone());
                        let _counted = task_inner.task_acquire().await;
                        inner.into_future().await
                    };
                    log.info(format!("complete{}: {}", if ret.is_err() { " (failed)" } else { "" }, s));
                    ret
                }).await
            })
            .await
        }))
    })
    .into()
}
