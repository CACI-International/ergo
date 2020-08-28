//! Concurrent task creation.

use grease::{runtime::SplitInto, value::Value};
use so_runtime::{apply_value, types};

pub fn function() -> Value {
    types::Function::new(|ctx| {
        let desc = ctx.args.next().ok_or("no task description")?;
        let v = ctx.args.next().ok_or("no argument to task")?;
        let args = std::mem::take(&mut ctx.args);

        let desc =
            so_runtime::script_value_as!(desc, types::String, "task description must be a string")?;

        let val = ctx
            .split_map(move |ctx| apply_value(ctx, v, args.unchecked(), true))?
            .unwrap();

        let task = ctx.task.clone();
        let log = ctx.log.sublog("task");
        let tp = val.grease_type();
        let id = val.id();
        Ok(ctx.call_site.clone().with(Value::with_id(
            tp,
            async move {
                task.spawn(async move {
                    log.info(desc);
                    val.await
                })
                .await
            },
            id,
        )))
    })
    .into()
}
