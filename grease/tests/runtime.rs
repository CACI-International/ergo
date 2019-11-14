use grease::*;
use grease::future::{try_join, TryFutureExt};
use uuid::Uuid;

fn vec_type() -> ValueType {
    ValueType::new(Uuid::from_bytes([
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ]))
}

struct TestRuntimePlan;

impl Plan for TestRuntimePlan {
    type Output = Value;

    fn plan(&self, ctx: &mut Context) -> Value {
        let a = Value::constant(vec_type(), vec![0]);
        let tsk = ctx.task.clone();
        let b = make_value!(vec_type(), [vec![1]] {
            tsk.spawn(async move {
                std::thread::sleep(std::time::Duration::from_millis(200));
                Ok(vec![1])
            }).await
        });

        let tsk = ctx.task.clone();
        make_value!(vec_type(), (a,b) {
            tsk.spawn(try_join(a,b).map_ok(|(av,bv)| {
                let mut o = (*av).clone();
                o.extend_from_slice(&bv);
                o
            })).await
        })
    }
}

#[test]
fn runtime_tasks() -> Result<(), String> {
    let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

    let output = TestRuntimePlan.plan(&mut ctx);
    let result = futures::executor::block_on(output)?;
    assert!(*result == vec![0, 1]);
    Ok(())
}

/*
#[test]
fn commands() -> Result<(), String> {
    let mut ctx = Context::builder().build().map_err(|e| format!("{}", e))?;

    let mut echo = ctx.cmd.create("echo");
    echo.arg("-n");
    echo.arg("hello, world");
    let tsk = ctx.task.clone();
    let mut output = make_value!(vec_type(), tsk.spawn(async move {
        match echo.output() {
            Err(e) => Err(format!("failed to run echo: {}", e)),
            Ok(output) => {
                if output.status.success() {
                    Ok(output.stdout)
                } else {
                    Err(format!("echo exited with status {}", output.status))
                }
            }
        }
    }).await);

    let s = String::from_utf8((*output.get()?).clone()).map_err(|_| "failed to get utf8 output".to_owned())?;
    assert!("hello, world" == s);
    Ok(())
}
*/
