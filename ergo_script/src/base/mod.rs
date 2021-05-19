//! Base environment.

use ergo_runtime::{traits, types, Value};

mod doc;
mod load;

pub use doc::*;
pub use load::*;

#[types::ergo_pat]
/// The 'fn' binding function, which takes all PatternArgs and returns an Args to be bound.
pub async fn pat_args_to_args(...) -> Value {
    types::Args { args: REST }.into()
}

#[types::ergo_pat]
/// The 'pat' binding function, which takes all PatternArgs and returns a PatternArgs to be bound.
pub async fn pat_args_to_pat_args(...) -> Value {
    types::PatternArgs { args: REST }.into()
}

#[types::ergo_pat]
/// The 'index' binding function, which takes a single argument and returns an Index to be bound.
pub async fn pat_args_to_index(ind: _) -> Value {
    types::Index(ind).into()
}

#[types::ergo_fn]
/// Bind the first argument using the value of the second argument.
pub async fn bind(to: _, from: _) -> Value {
    traits::bind(CONTEXT, to, from).await.unwrap()
}
