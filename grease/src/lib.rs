//! Functionality for building graphs of computations from configuration.
//!
//! This crate offers a veneer over building _plans_, where a plan is a directed acyclic graph of
//! values and dependencies. Values have runtime type information rather than compile time types.
//!
//! The main feature the crate offers is a runtime context, which contains common functionality for
//! plans to utilize. Functionality includes logging, progress reporting/recording, external command
//! execution, and scheduling of tasks on a thread pool.

use proc_macro_hack::proc_macro_hack;

mod prelude;
mod runtime;
mod uuid;
mod value;

pub use futures::future;
pub use futures::future::{FutureExt, TryFutureExt};
pub use ::uuid::Uuid;

pub use self::{runtime::*, uuid::*, value::*};

#[proc_macro_hack]
pub use grease_macro::item_name;
