//! Concurrent task creation.

use ergo_runtime::{ergo_function, types, ContextExt};
use grease::future::eager::Eager;
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
            task.spawn(async move {
                let s = desc.await?;
                log.info(s.clone());
                let ret = inner.into_future().await;
                log.info(format!("{}: complete{}", s, if ret.is_err() { " (failed)" } else { "" }));
                log.end_stream();
                ret
            })
            .await
        }))
    })
    .into()
}
