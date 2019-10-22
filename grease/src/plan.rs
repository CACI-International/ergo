//! Plan creation.
//!
//! A plan is a graph of tasks, where each task represents an instance of a procedure. Plans
//! use a description of task dependencies (provided by [`PlanBuilder`](struct.PlanBuilder.html)) to create
//! an asynchronous call flow to get the outputs of a 'root' task.

use crate::prelude::*;
use futures::future;
use futures::future::{BoxFuture, Future, FutureExt, Shared, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::HashMap;
use std::fmt;
use std::pin::Pin;
use std::sync::Arc;
use uuid::Uuid;

/// Procedures are identified by a UUID.
///
/// UUIDs should generally be v5 based on the root UUID namespace.
pub type Procedure = Uuid;

/// An instance of a procedure
#[derive(Debug)]
struct Task(Procedure);

/// An identifier for a particular task in a plan.
#[derive(Clone, Copy, Debug, Default, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct TaskId(usize);

impl fmt::Display for TaskId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "task {}", self.0)
    }
}

/// An identifier for a constant in a plan.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct ConstantId(usize);

impl fmt::Display for ConstantId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "constant {}", self.0)
    }
}

/// A procedure resolver.
///
/// Error is the error type returned by the resolver.
pub trait Resolver {
    type Error;
    /// Resolve the given
    fn resolve(
        &mut self,
        id: TaskId,
        proc: &Procedure,
        inputs: Vec<Value>,
    ) -> Result<Vec<Value>, Self::Error>;
}

impl<T, Error> Resolver for T
where
    T: FnMut(TaskId, &Procedure, Vec<Value>) -> Result<Vec<Value>, Error>,
{
    type Error = Error;

    fn resolve(
        &mut self,
        id: TaskId,
        proc: &Procedure,
        inputs: Vec<Value>,
    ) -> Result<Vec<Value>, Error> {
        self(id, proc, inputs)
    }
}

/// A value type.
///
/// The type is an id and type-specific data.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueType {
    pub id: Uuid,
    pub data: Vec<u8>,
}

impl ValueType {
    /// Create a new ValueType.
    pub fn new(id: Uuid) -> Self {
        Self::with_data(id, vec![])
    }

    pub fn with_data(id: Uuid, data: Vec<u8>) -> Self {
        ValueType { id, data }
    }
}

/// A value shared amongst tasks.
///
/// Values have a type and a future value.
#[derive(Clone, Debug)]
pub struct Value {
    tp: Arc<ValueType>,
    value: Shared<BoxFuture<'static, Result<Arc<Vec<u8>>, String>>>,
}

impl Value {
    /// Create a value with the given type and future.
    pub fn new<F>(tp: ValueType, value: F) -> Value
    where
        F: Future<Output = Result<Vec<u8>, String>> + Send + 'static,
    {
        Value {
            tp: Arc::new(tp),
            value: value.map_ok(Arc::new).boxed().shared(),
        }
    }

    /// Get the type of the contained value.
    pub fn get_type(&self) -> &ValueType {
        &*self.tp
    }
}

impl Future for Value {
    type Output = Result<Arc<Vec<u8>>, String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(Pin::new(&mut self.get_mut().value), cx)
    }
}

/// A plan, composed of tasks and IO links between them.
///
/// The plan is a representation of an asynchronous call graph among tasks.
#[derive(Debug)]
pub struct Plan {
    outputs: HashMap<TaskId, Vec<Value>>,
    root: TaskId,
}

impl Plan {
    /// Create a new plan builder.
    pub fn builder() -> PlanBuilder {
        PlanBuilder::default()
    }

    /// Get the root outputs of the plan.
    pub fn outputs(&self) -> &[Value] {
        self.outputs.get(&self.root).unwrap().as_ref()
    }

    /// Run the outputs values to completion, returning the results of each.
    pub fn run(&mut self) -> Vec<Result<Arc<Vec<u8>>, String>> {
        futures::executor::block_on(future::join_all(self.outputs.get_mut(&self.root).unwrap()))
    }
}

/// Plan builder errors.
#[derive(Debug)]
pub enum Error<E> {
    /// A task is missing an input.
    MissingInput(TaskId, usize),
    /// A task is missing a required output.
    ///
    /// The last TaskId is that of the task which needs the output.
    MissingOutput(TaskId, usize, TaskId),
    /// A loop in the task graph was detected.
    ///
    /// The list of tasks involved in the loop is provided, where the first task follows the last.
    LoopDetected(Vec<TaskId>),
    /// The task id provided as the output id was not valid.
    InvalidOutput,
    /// A resolver error occurred.
    Resolver(E),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Error::MissingInput(task, index) => write!(f, "missing input {} of {}", index, task),
            Error::MissingOutput(task, index, for_task) => write!(
                f,
                "missing output {} of {} (needed for {})",
                index, task, for_task
            ),
            Error::LoopDetected(lp) => {
                write!(f, "task loop detected")?;
                if lp.len() == 0 {
                    Ok(())
                } else {
                    write!(f, ": ")?;
                    for id in lp {
                        write!(f, "{} -> ", id)?;
                    }
                    write!(f, "{}", lp.first().unwrap())
                }
            }
            Error::InvalidOutput => write!(f, "invalid output task id"),
            Error::Resolver(e) => write!(f, "resolver error: {}", e),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OutputReference {
    Task(TaskId, usize),
    Constant(ConstantId),
}

impl fmt::Display for OutputReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            OutputReference::Task(ref id, ref ind) => write!(f, "{} output {}", id, ind),
            OutputReference::Constant(ref id) => write!(f, "{}", id),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InputReference {
    id: TaskId,
    index: usize
}

impl fmt::Display for InputReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} input {}", self.id, self.index)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TaskReference {
    id: TaskId
}

impl TaskReference {
    /// Create a reference to the given output index.
    pub fn output(&self, index: usize) -> OutputReference {
        OutputReference::Task(self.id, index)
    }

    /// Create a reference to the given input index.
    pub fn input(&self, index: usize) -> InputReference {
        InputReference { id: self.id, index }
    }
}

/// A plan builder.
#[derive(Debug, Default)]
pub struct PlanBuilder {
    tasks: Vec<Task>,
    constants: Vec<Value>,
    links: HashMap<TaskId, Vec<Option<OutputReference>>>,
}

impl PlanBuilder {
    /// Add a task with the given procedure.
    /// Returns a task reference that can be used to link inputs and outputs.
    pub fn add_task(&mut self, proc: Procedure) -> TaskReference {
        self.tasks.push(Task(proc));
        let ret = TaskId(self.tasks.len() - 1);
        debug!(target: "plan_builder", "added {} = {}", ret, proc);
        trace!(target: "plan_builder", "plan builder state = {:?}", self);
        TaskReference { id: ret }
    }

    /// Add a constant value to the plan.
    /// Returns an output reference that can be use to link to task inputs.
    pub fn add_constant(&mut self, tp: ValueType, value: Vec<u8>) -> OutputReference {
        self.constants.push(Value::new(tp, future::ok(value)));
        let ret = ConstantId(self.constants.len() - 1);
        debug!(target: "plan_builder", "added {}", ret);
        trace!(target: "plan_builder", "plan builder state = {:?}", self);
        OutputReference::Constant(ret)
    }

    /// Link an output to an input.
    ///
    /// Returns whether the link was made successfully. This will return false if a link already
    /// exists.
    pub fn link(&mut self, from: OutputReference, to: InputReference) -> bool {
        let e = self.links.entry(to.id).or_default();
        if to.index >= e.len() {
            e.resize_with(to.index + 1, Default::default);
        }
        let val = e.get_mut(to.index).unwrap();
        match val {
            None => {
                debug!(target: "plan_builder", "added link {} -> {}", from, to);
                *val = Some(from);
                trace!(target: "plan_builder", "plan builder state = {:?}", self);
                true
            }
            Some(ref f) => {
                warn!(target: "plan_builder", "link to {} already exists: {}", to, f);
                false
            }
        }
    }

    /// Build and return the underlying plan.
    ///
    /// The given output TaskId is the root output in the plan.
    /// The resolver will be called for all applicable tasks in the plan.
    pub fn build<F: Resolver>(
        self,
        output: TaskReference,
        mut resolver: F,
    ) -> Result<Plan, Error<F::Error>> {
        if output.id.0 >= self.tasks.len() {
            return Err(Error::InvalidOutput);
        }

        let mut outputs = HashMap::new();
        let mut processing = Vec::new();
        self.resolve_task(&mut resolver, &mut outputs, &mut processing, output.id)?;
        let p = Plan {
            outputs,
            root: output.id,
        };
        trace!(target: "plan_builder", "built plan {:?}", p);
        Ok(p)
    }

    fn resolve_task<'a, F: Resolver>(
        &self,
        resolver: &mut F,
        outputs: &'a mut HashMap<TaskId, Vec<Value>>,
        processing: &mut Vec<TaskId>,
        id: TaskId,
    ) -> Result<&'a Vec<Value>, Error<F::Error>> {
        if let Some(i) = processing.iter().position(|task| *task == id) {
            return Err(Error::LoopDetected(processing[i..].to_vec()));
        }
        processing.push(id);
        if !outputs.contains_key(&id) {
            debug!(target: "plan_builder", "getting outputs for task id {}", id);
            let Task(ref proc) = self.tasks.get(id.0).unwrap();
            let inputs = match self.links.get(&id) {
                None => Ok(vec![]),
                Some(ref v) => v
                    .iter()
                    .enumerate()
                    .map(|(i, a)| match a.ok_or(Error::MissingInput(id, i))? {
                        OutputReference::Task(from_id, from_index) => {
                            let outs = self.resolve_task(resolver, outputs, processing, from_id)?;
                            outs.get(from_index)
                                .map(|v| v.clone())
                                .ok_or(Error::MissingOutput(from_id, from_index, id))
                        }
                        OutputReference::Constant(id) => {
                            Ok(self.constants.get(id.0).unwrap().clone())
                        }
                    })
                    .collect(),
            }?;
            debug!(target: "plan_builder", "calling resolver for procedure {} with {} inputs", proc, inputs.len());
            outputs.insert(
                id,
                resolver
                    .resolve(id, proc, inputs)
                    .map_err(Error::Resolver)?,
            );
        }
        processing.pop();
        Ok(outputs.get(&id).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vec_type() -> ValueType {
        ValueType::new(Uuid::from_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ]))
    }

    #[test]
    fn test_plan_builder() -> Result<(), String> {
        let mut builder = Plan::builder();
        let proca = Uuid::from_bytes([1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let procb = Uuid::from_bytes([2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let a = builder.add_task(proca);
        let constv = builder.add_constant(vec_type(), vec![1]);
        let b = builder.add_task(procb);
        assert!(builder.link(a.output(0), b.input(0)));
        assert!(builder.link(constv, b.input(1)));
        let plan = builder
            .build(b, |_id: TaskId, proc: &Procedure, inp: Vec<Value>| {
                if proc == &proca {
                    Ok(vec![Value::new(vec_type(), futures::future::ok(vec![0]))])
                } else if proc == &procb {
                    match inp.as_slice() {
                        [av, cv] => {
                            let av_ = av.clone();
                            let cv_ = cv.clone();
                            Ok(vec![Value::new(
                                vec_type(),
                                av_.and_then(move |adata| {
                                    cv_.map_ok(move |cdata| {
                                        let mut o = (*adata).clone();
                                        o.extend(cdata.iter());
                                        o.push(2);
                                        o
                                    })
                                }),
                            )])
                        }
                        _ => Err("incorrect number of inputs".to_owned()),
                    }
                } else {
                    Err("unhandled".to_owned())
                }
            })
            .map_err(|e| format!("{}", e))?;
        let outs = plan.outputs();
        assert!(outs.len() == 1);
        let v = outs.get(0).unwrap().clone();
        assert!(*v.get_type() == vec_type());
        let result = futures::executor::block_on(v)?;
        assert!(*result == vec![0, 1, 2]);
        Ok(())
    }
}
