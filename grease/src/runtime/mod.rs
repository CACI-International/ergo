//! Execution logic for plans.

use std::fmt;

mod command;
mod log;
mod task_manager;

use self::log::EmptyLogTarget;
pub use self::log::{logger_ref, Log, LogEntry, LogLevel, LogTarget, LoggerRef};
pub use command::Commands;
pub use task_manager::TaskManager;
pub use futures::future;
pub use futures::future::{FutureExt,TryFutureExt};

/// Runtime context.
#[derive(Debug)]
pub struct Context {
    /// The task manager.
    pub task: TaskManager,
    /// The command interface.
    pub cmd: Commands,
    /// The logging interface.
    pub log: Log,
}

#[derive(Default)]
pub struct ContextBuilder {
    logger: Option<LoggerRef>,
}

#[derive(Debug)]
pub enum BuilderError {
    TaskManagerError(futures::io::Error),
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TaskManagerError(e) => write!(f, "task manager error: {}", e),
        }
    }
}

impl ContextBuilder {
    pub fn logger<T: LogTarget + Send + 'static>(mut self, logger: T) -> Self {
        self.logger = Some(logger_ref(logger));
        self
    }

    pub fn logger_ref(mut self, logger: LoggerRef) -> Self {
        self.logger = Some(logger);
        self
    }

    pub fn build(self) -> Result<Context, BuilderError> {
        Ok(Context {
            task: TaskManager::new().map_err(BuilderError::TaskManagerError)?,
            cmd: Commands::new(),
            log: Log::new(self.logger.unwrap_or_else(|| logger_ref(EmptyLogTarget))),
        })
    }
}

impl Context {
    pub fn builder() -> ContextBuilder {
        Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::*;
    use futures::future::{try_join, TryFutureExt};
    use uuid::Uuid;

    fn vec_type() -> ValueType {
        ValueType::new(Uuid::from_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ]))
    }

    struct TestRuntimePlan;

    impl Plan<Context> for TestRuntimePlan {
        type Output = Value;

        fn plan(&self, ctx: &mut Context) -> Value {
            let val_a = Value::constant(vec_type(), vec![0]);
            let val_b = Value::future(
                vec_type(),
                ctx.task.delayed_fn(|| {
                    std::thread::sleep(std::time::Duration::from_millis(200));
                    Ok(vec![1])
                }),
            );
            crate::derived_value!(vec_type(), (a=val_a, b=val_b) {
                ctx.task.delayed(try_join(a,b).map_ok(|(av,bv)| {
                    let mut o = (*av).clone();
                    o.extend_from_slice(&bv);
                    o
                }))
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

    #[test]
    fn commands() -> Result<(), String> {
        let mut ctx = Context::builder()
            .build()
            .map_err(|e| format!("{}", e))?;

        let mut ls = ctx.cmd.create("ls");

        let output = Value::future(vec_type(),ctx.task.delayed_fn(move || match ls.output() {
            Err(e) => Err(format!("failed to run ls: {}", e)),
            Ok(output) => {
                if output.status.success() {
                    Ok(output.stdout)
                } else {
                    Err(format!("ls exited with status {}", output.status))
                }
            }
        }));

        futures::executor::block_on(output).map(|_| ())
    }
}
