//! Concurrent task creation.

use ergo_runtime::{ergo_function, types};
use grease::value::Value;

pub fn function() -> Value {
    ergo_function!(independent std::task, |ctx| {
        let desc = ctx.args.next().ok_or("no task description")?;
        let val = ctx.args.next().ok_or("no argument to task")?.unwrap();

        ctx.unused_arguments()?;

        let desc = ergo_runtime::source_value_as!(desc, types::String, ctx)?.unwrap();

        let task = ctx.task.clone();
        let log = ctx.log.sublog("task");
        let tp = val.grease_type();
        let id = val.id();
        Value::with_id(
            tp,
            async move {
                task.spawn(async move {
                    log.info(desc.await?);
                    val.await
                })
                .await
            },
            id,
        )
    })
    .into()
}
