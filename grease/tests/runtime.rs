use abi_stable::rvec;
use futures::{executor::block_on, future::TryFutureExt};
use grease::{make_value, runtime::Context, value::TypedValue};

#[test]
fn runtime_tasks() -> Result<(), String> {
    let ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

    let result = block_on({
        let a = TypedValue::constant(rvec![0]);
        let tsk = ctx.task.clone();
        let b = make_value!([vec![1]] {
            tsk.spawn(async move {
                std::thread::sleep(std::time::Duration::from_millis(200));
                Ok(rvec![1u8])
            }).await
        });

        let tsk = ctx.task.clone();
        make_value!((a,b) {
            tsk.spawn_basic(tsk.join(a,b).map_ok(|(av,bv)| {
                let mut o = (*av).clone();
                o.extend_from_slice(&bv);
                o
            })).await?
        })
    })
    .map_err(|e| e.to_string())?;

    assert!(*result == rvec![0, 1]);
    Ok(())
}
