//! Execution logic for plans.

use std::collections::HashMap;

use crate::plan::{Procedure, Value};

mod command;
mod task_manager;

pub use command::Commands;
pub use task_manager::TaskManager;

/// Runtime context.
#[derive(Debug)]
pub struct Context {
    /// The task manager.
    pub task: TaskManager,
    pub cmd: Commands,
}

impl Context {
    /// Create a new, default context.
    pub fn new() -> Result<Context, futures::io::Error> {
        Ok(Context {
            task: TaskManager::new()?,
            cmd: Commands::new(),
        })
    }
}

/// A function used to resolve procedures.
pub type ProcedureFunc = fn(&mut Context, Vec<Value>) -> Result<Vec<Value>, String>;

/// The plan execution runtime.
pub struct Runtime {
    procedures: HashMap<Procedure, ProcedureFunc>,
    pub context: Context,
}

impl Runtime {
    /// Create a new runtime with the given context.
    pub fn new(ctx: Context) -> Self {
        Runtime {
            procedures: HashMap::new(),
            context: ctx,
        }
    }

    /// Add a procedure handler to the runtime.
    pub fn add_procedure(&mut self, proc: Procedure, resolve: ProcedureFunc) {
        self.procedures.insert(proc, resolve);
    }

    /// Return a suitable resolver function for `PlanBuilder::build`.
    pub fn resolve<'a>(
        &'a mut self,
    ) -> impl FnMut(&Procedure, Vec<Value>) -> Result<Vec<Value>, String> + 'a {
        move |proc, val| match self.procedures.get(&proc) {
            None => Err(format!("could not resolve procedure {}", proc)),
            Some(f) => f(&mut self.context, val),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::*;
    use futures::future::{try_join, TryFutureExt};
    use uuid::Uuid;

    fn a_resolve(ctx: &mut Context, _inputs: Vec<Value>) -> Result<Vec<Value>, String> {
        let out = Value::new(vec![0], ctx.task.delayed(futures::future::ok(vec![0])));
        Ok(vec![out])
    }

    fn b_resolve(ctx: &mut Context, _inputs: Vec<Value>) -> Result<Vec<Value>, String> {
        let out = Value::new(
            vec![0],
            ctx.task.delayed_fn(|| {
                std::thread::sleep(std::time::Duration::from_millis(200));
                Ok(vec![1])
            }),
        );
        Ok(vec![out])
    }

    fn c_resolve(ctx: &mut Context, inputs: Vec<Value>) -> Result<Vec<Value>, String> {
        match inputs[..] {
            [ref a, ref b] => {
                let out = Value::new(
                    vec![0],
                    ctx.task
                        .delayed(try_join(a.clone(), b.clone()).map_ok(|(ad, bd)| {
                            let mut o = (*ad).clone();
                            o.extend_from_slice(&*bd);
                            o
                        })),
                );
                Ok(vec![out])
            }
            _ => Err("not enough inputs".to_owned()),
        }
    }

    #[test]
    fn test_runtime_tasks() -> Result<(), String> {
        let mut builder = PlanBuilder::new();
        let proca = Uuid::from_bytes([1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let procb = Uuid::from_bytes([2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let procc = Uuid::from_bytes([3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let a = builder.add_task(proca);
        let b = builder.add_task(procb);
        let c = builder.add_task(procc);

        builder.link(a, 0, c, 0);
        builder.link(b, 0, c, 1);

        let mut runtime =
            Runtime::new(Context::new().map_err(|_| "failed to create context".to_owned())?);
        runtime.add_procedure(proca, a_resolve);
        runtime.add_procedure(procb, b_resolve);
        runtime.add_procedure(procc, c_resolve);
        let plan = builder.build(c, runtime.resolve())?;
        let outs = plan.outputs().ok_or("failed to get outputs")?;
        let result = futures::executor::block_on(outs.get(0).unwrap().clone())?;
        assert!(*result == vec![0, 1]);
        Ok(())
    }

    #[test]
    fn test_missing_task() {
        let mut builder = PlanBuilder::new();
        let proca = Uuid::from_bytes([1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let a = builder.add_task(proca);

        let mut runtime = Runtime::new(Context::new().expect("failed to create context"));
        builder
            .build(a, runtime.resolve())
            .expect_err("should not find procedure");
    }

    fn a_run_ls(ctx: &mut Context, _inputs: Vec<Value>) -> Result<Vec<Value>, String> {
        let mut ls = ctx.cmd.create("ls");

        let out = ctx.task.delayed_fn(move || match ls.output() {
            Err(e) => Err(format!("failed to run ls: {}", e)),
            Ok(output) => {
                if output.status.success() {
                    Ok(output.stdout)
                } else {
                    Err(format!("ls exited with status {}", output.status))
                }
            }
        });

        Ok(vec![Value::new(vec![0],out)])
    }

    #[test]
    fn test_commands() {
        let mut builder = PlanBuilder::new();
        let proca = Uuid::from_bytes([1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let a = builder.add_task(proca);

        let mut runtime = Runtime::new(Context::new().expect("failed to create context"));
        runtime.add_procedure(proca, a_run_ls);
        let plan = builder.build(a, runtime.resolve()).expect("failed to build plan");
        print!("{}", runtime.context.cmd);
        let outs = plan.outputs().expect("failed to get outputs");
        let _result = futures::executor::block_on(outs.get(0).unwrap().clone()).expect("failed to get result");
    }
}
