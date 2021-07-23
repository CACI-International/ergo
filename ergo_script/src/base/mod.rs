//! Base environment.

use ergo_runtime::{
    depends, metadata::Source, nsid, traits, try_result, types, value::match_value, Value,
};

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

/// The 'index' function, which supports binding to Index values and indexing values explicitly.
pub fn index() -> Value {
    types::Unbound::new(
        move |v| async move {
            match_value! {v,
                types::Args { mut args } => {
                    let v = try_result!(args.next().ok_or("no value to index"));
                    let i = try_result!(args.next().ok_or("no index"));
                    try_result!(args.unused_arguments());

                    traits::bind(v, Source::imbue(Source::get(&i).with(types::Index(i).into()))).await
                }
                types::PatternArgs { mut args } => {
                    let i = try_result!(args.next().ok_or("no index binding"));
                    try_result!(args.unused_arguments());

                    types::Index(i).into()
                }
                v => traits::type_error(v, "function call or pattern function call").into()
            }
        },
        depends![nsid!(ergo::index)],
        "Match an index binding or index a value.

Arguments: `:value :index`

Get the `index` value for `value`. This may be useful when you want to index a captured value
without the index being captured.


Pattern Arguments: `:index`

Match an index binding."
    ).into()
}

#[types::ergo_fn]
/// Bind the first argument using the value of the second argument.
pub async fn bind(to: _, from: _) -> Value {
    traits::bind(to, from).await
}

/// An `Unset` value.
pub fn unset() -> Value {
    types::Unset.into()
}
