//! Plan types.
//!
//! A plan is a graph of tasks, where each task represents an instance of a procedure. Plans
//! describe task dependencies, but are not responsible for anything like verification or
//! execution.
//!
//! Plans should be built using `PlanBuilder` rather than directly creating `Plan` instances.

use uuid::Uuid;

/**
 * Procedures are identified by a UUID.
 *
 * UUIDs should generally be v5 based on the root UUID namespace.
 */
pub type Procedure = Uuid;

/// An instance of a procedure
#[derive(Debug)]
pub struct Task(Procedure);

/// An identifier for a particular task in a plan.
pub type TaskId = usize;

/// An input or output parameter for a task.
#[derive(Debug)]
pub struct Param {
    pub task: TaskId,
    pub index: usize,
}

/// A plan, composed of tasks and IO links between them.
#[derive(Debug, Default)]
pub struct Plan {
    pub tasks: Vec<Task>,
    pub links: Vec<(Param, Param)>,
}

/// A plan builder.
#[derive(Debug, Default)]
pub struct PlanBuilder {
    plan: Plan,
}

impl<'a> PlanBuilder {
    pub fn new() -> PlanBuilder {
        Default::default()
    }

    /**
     * Add a task with the given procedure.
     * Returns a task id that can be used to link inputs and outputs.
     */
    pub fn add_task(&mut self, proc: Procedure) -> TaskId {
        self.plan.tasks.push(Task(proc));
        self.plan.tasks.len() - 1
    }

    /**
     * Link an output from one task to an input of another.
     */
    pub fn link(&mut self, from: TaskId, from_index: usize, to: TaskId, to_index: usize) {
        self.plan.links.push((
            Param {
                task: from,
                index: from_index,
            },
            Param {
                task: to,
                index: to_index,
            },
        ));
    }

    /// Build and return the underlying plan.
    pub fn build(self) -> Plan {
        self.plan
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plan_builder() {
        let mut builder = PlanBuilder::new();
        let proca = Uuid::from_bytes([1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let procb = Uuid::from_bytes([2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        let a = builder.add_task(proca);
        let b = builder.add_task(procb);
        builder.link(a, 0, b, 1);
        let plan = builder.build();
        assert!(plan.tasks.len() == 2);
        assert!(plan.links.len() == 1);
        let l = &plan.links[0];
        assert!(plan.tasks[l.0.task].0 == proca);
        assert!(plan.tasks[l.1.task].0 == procb);
        assert!(l.0.index == 0);
        assert!(l.1.index == 1);
    }
}
