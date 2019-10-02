//! Plan creation.
//!
//! A plan is a graph of tasks, where each task represents an instance of a procedure. Plans
//! use a description of task dependencies (provided by [`PlanBuilder`](struct.PlanBuilder.html)) to create
//! an asynchronous call flow to get the outputs of a 'root' task.

use futures::future::{Future,FutureExt,TryFutureExt,BoxFuture,Shared};
use futures::task::{Poll,Context};
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;
use uuid::Uuid;

/**
 * Procedures are identified by a UUID.
 *
 * UUIDs should generally be v5 based on the root UUID namespace.
 */
pub type Procedure = Uuid;

/// An instance of a procedure
#[derive(Debug)]
struct Task(Procedure);

/// An identifier for a particular task in a plan.
pub type TaskId = usize;

/// A value shared amongst tasks.
///
/// Values have a type and a future value.
#[derive(Clone,Debug)]
pub struct Value {
    tp: Arc<Vec<u8>>,
    value: Shared<BoxFuture<'static, Result<Arc<Vec<u8>>,String>>>,
}

impl Value {
    /// Create a value with the given type and future.
    pub fn new<F>(tp: Vec<u8>, value: F) -> Value
        where F: Future<Output=Result<Vec<u8>,String>> + Send + 'static
    {
        Value {
            tp: Arc::new(tp),
            value: value.map_ok(Arc::new).boxed().shared()
        }
    }

    /// Get the type of the contained value.
    pub fn get_type(&self) -> &[u8] { &*self.tp }
}

impl Future for Value {
    type Output = Result<Arc<Vec<u8>>,String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(Pin::new(&mut self.get_mut().value), cx)
    }
}

/// A plan, composed of tasks and IO links between them.
///
/// The plan is a representation of an asynchronous call graph among tasks.
#[derive(Debug, Default)]
pub struct Plan {
    outputs: HashMap<TaskId, Vec<Value>>,
    root: TaskId,
}

impl Plan {
    /// Get the root outputs of the plan.
    pub fn outputs(&self) -> Option<&[Value]> {
        self.outputs.get(&self.root).map(|v| v.as_ref())
    }
}

/// A plan builder.
#[derive(Debug, Default)]
pub struct PlanBuilder {
    tasks: Vec<Task>,
    links: HashMap<TaskId, Vec<Option<(TaskId, usize)>>>,
}

impl PlanBuilder {
    /// Create a new plan builder.
    pub fn new() -> PlanBuilder {
        Self::default()
    }

    /**
     * Add a task with the given procedure.
     * Returns a task id that can be used to link inputs and outputs.
     */
    pub fn add_task(&mut self, proc: Procedure) -> TaskId {
        self.tasks.push(Task(proc));
        self.tasks.len() - 1
    }

    /**
     * Link an output from one task to an input of another.
     *
     * Returns whether the link was made successfully. This will return false if a link already
     * exists.
     */
    pub fn link(&mut self, from: TaskId, from_index: usize, to: TaskId, to_index: usize) -> bool {
        let e = self.links.entry(to).or_default();
        if to_index >= e.len() {
            e.resize_with(to_index + 1, Default::default);
        }
        let val = e.get_mut(to_index).unwrap();
        match val {
            None => {
                *val = Some((from, from_index));
                true
            }
            Some(_) => false,
        }
    }

    /// Build and return the underlying plan.
    ///
    /// The given output TaskId is the root output in the plan.
    /// The resolver will be called for all applicable tasks in the plan.
    pub fn build<F>(self, output: TaskId, mut resolver: F) -> Result<Plan, String>
    where
        F: FnMut(&Procedure, Vec<Value>) -> Result<Vec<Value>, String>,
    {
        let mut plan = Plan::default();
        plan.root = output;
        self.resolve_task(&mut resolver, &mut plan.outputs, output)?;
        Ok(plan)
    }

    fn resolve_task<'a, F>(
        &self,
        resolver: &mut F,
        outputs: &'a mut HashMap<TaskId, Vec<Value>>,
        id: TaskId,
    ) -> Result<&'a Vec<Value>, String>
    where
        F: FnMut(&Procedure, Vec<Value>) -> Result<Vec<Value>, String>,
    {
        if !outputs.contains_key(&id) {
            let Task(ref proc) = self.tasks.get(id).unwrap();
            let inputs = match self.links.get(&id) {
                None => Ok(vec![]),
                Some(ref v) => v
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let (from_id, from_index) =
                            a.ok_or_else(|| format!("input {} for task {} unset", i, id))?;
                        let outs = self.resolve_task(resolver, outputs, from_id)?;
                        outs.get(from_index).map(|v| v.clone()).ok_or_else(|| {
                            format!(
                                "output {} for task {} unset (needed for task {})",
                                from_index, from_id, id
                            )
                        })
                    })
                    .collect(),
            }?;
            outputs.insert(id, resolver(proc, inputs)?);
        }
        Ok(outputs.get(&id).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plan_builder() -> Result<(),String> {
        let mut builder = PlanBuilder::new();
        let proca = Uuid::from_bytes([1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let procb = Uuid::from_bytes([2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let a = builder.add_task(proca);
        let b = builder.add_task(procb);
        builder.link(a, 0, b, 0);
        let plan = builder.build(b, |proc,inp| {
            if proc == &proca {
                Ok(vec![Value::new(vec![0],futures::future::ready(Ok(vec![0])))])
            } else if proc == &procb {
                match inp.get(0) {
                    None => Err("not enough inputs".to_owned()),
                    Some(v) => Ok(vec![Value::new(vec![0],v.clone().map_ok(|data| {
                        let mut o = (*data).clone();
                        o.push(1);
                        o
                    }))])
                }
            } else {
                Err("unhandled".to_owned())
            }
        })?;
        let outs = plan.outputs().ok_or("failed to get outputs")?;
        assert!(outs.len() == 1);
        let v = outs.get(0).unwrap().clone();
        assert!(v.get_type() == vec![0].as_slice());
        let result = futures::executor::block_on(v)?;
        assert!(*result == vec![0,1]);
        Ok(())
    }
}
