//! Procedure evaluation.
//!
//! Procedures have related UUIDs and map an input to an output. They can be used to allow certain
//! functionality to behave differently. For instance, looking up a local set of procedures versus
//! looking up procedures on remote workers.

use crate::plan::Value;
use std::collections::HashMap;
use std::fmt;
use uuid::Uuid;

/// A procedure identifier.
pub type Procedure = Uuid;

/// A type which can be input to a particular procedure.
pub trait ProcedureInput: Into<Value> {
    fn procedure() -> Procedure;
}

/// A type used to resolve procedures.
pub trait Resolver {
    fn resolve(
        &self,
        proc: Procedure,
        ctx: &mut super::Context,
        input: Value,
    ) -> Result<Value, String>;
}

/// A resolver which always returns error.
pub struct EmptyResolver;

impl Resolver for EmptyResolver {
    fn resolve(
        &self,
        _proc: Procedure,
        _ctx: &mut super::Context,
        _input: Value,
    ) -> Result<Value, String> {
        Err("no procedures registered".to_owned())
    }
}

pub type ProcedureFunc = fn(&mut super::Context, Value) -> Result<Value, String>;

impl<S: std::hash::BuildHasher> Resolver for HashMap<Procedure,ProcedureFunc,S>
{
    fn resolve(
        &self,
        proc: Procedure,
        ctx: &mut super::Context,
        input: Value,
    ) -> Result<Value, String> {
        self.get(&proc)
            .ok_or(format!("could not find procedure {}", proc))
            .and_then(move |f| f(ctx,input))
    }
}

/// Procedure-related logic.
pub struct Proc {
    resolver: Box<dyn Resolver>,
}

impl fmt::Debug for Proc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Proc {{}}")
    }
}

impl Proc {
    pub(crate) fn new(resolver: Box<dyn Resolver>) -> Self {
        Proc { resolver }
    }

    /// Run the given input using the resolver.
    pub fn run<Input: ProcedureInput>(
        &self,
        ctx: &mut super::Context,
        input: Input,
    ) -> Result<Value, String> {
        self.resolver.resolve(Input::procedure(), ctx, input.into())
    }
}
