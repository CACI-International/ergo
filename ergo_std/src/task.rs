//! Concurrent task creation.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::future::eager::Eager;
use grease::runtime::TaskDescription;
use grease::value::Value;
use std::str::FromStr;

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

        ctx.unused_arguments()?;

        let desc = ctx.source_value_as::<types::String>(desc);
        let desc = desc.await?.unwrap();

        let task = ctx.task.clone();
        let log = ctx.log.sublog("task");
        val.map_data(move |inner| Eager::Pending(async move {
            let task_inner = task.clone();
            task.spawn(async move {
                let s = desc.await?;
                let task_description = TaskDescription { description: s.clone().owned() };
                task_inner.clone().scope_task_local(task_description, async move {
                    log.info(format!("starting: {}", s.clone()));
                    let ret = {
                        let _key = log.task(s.clone());
                        let _counted = task_inner.task_acquire(task_count).await;
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
