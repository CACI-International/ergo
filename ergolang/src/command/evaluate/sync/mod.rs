//! Thread syncronization primitives.

mod scoped;
mod threaded_mut;

pub use scoped::{Scoped, ScopedRef, ScopedRefGuard};
pub use threaded_mut::{Mutator, ThreadedMut};
