use abi_stable::{rvec, std_types::RVec};
use futures::future::TryFutureExt;
use grease::{
    make_value,
    runtime::{Context, Plan},
    value::TypedValue,
};

struct TestRuntimePlan;

impl Plan for TestRuntimePlan {
    type Output = TypedValue<RVec<u8>>;

    fn plan(self, ctx: &mut Context) -> Self::Output {
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
    }
}

#[test]
fn runtime_tasks() -> Result<(), String> {
    let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

    let result = TestRuntimePlan
        .plan(&mut ctx)
        .get()
        .map_err(|e| e.to_string())?;
    assert!(*result == rvec![0, 1]);
    Ok(())
}

#[test]
fn commands() -> Result<(), String> {
    let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

    let mut echo = ctx.cmd.create("echo");
    echo.arg("-n");
    echo.arg("hello, world");
    let tsk = ctx.task.clone();
    let output = make_value!(
        tsk.spawn(async move {
            match echo.output() {
                Err(e) => Err(format!("failed to run echo: {}", e).into()),
                Ok(output) => {
                    if output.status.success() {
                        Ok(RVec::from(output.stdout))
                    } else {
                        Err(format!("echo exited with status {}", output.status).into())
                    }
                }
            }
        })
        .await
    );

    let s = String::from_utf8(
        (*output.get().map_err(|e| e.to_string())?)
            .clone()
            .into_vec(),
    )
    .map_err(|_| "failed to get utf8 output".to_owned())?;
    assert!("hello, world" == s);
    Ok(())
}
