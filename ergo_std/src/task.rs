//! Concurrent task creation.

use ergo_runtime::{apply_value, types};
use futures::future::FutureExt;
use grease::value::Value;

pub fn function() -> Value {
    types::Function::new(|ctx| async move {
        let desc = ctx.args.next().ok_or("no task description")?;
        let v = ctx.args.next().ok_or("no argument to task")?;
        let args = std::mem::take(&mut ctx.args);

        let desc = ergo_runtime::source_value_as!(desc, types::String, ctx)?.unwrap();

        let val = apply_value(ctx, v, args.unchecked(), true).await?.unwrap();

        let task = ctx.task.clone();
        let log = ctx.log.sublog("task");
        let tp = val.grease_type();
        let id = val.id();
        Ok(ctx.call_site.clone().with(Value::with_id(
            tp,
            async move {
                task.spawn(async move {
                    log.info(desc.await?);
                    val.await
                })
                .await
            },
            id,
        )))
    }.boxed())
    .into()
}
