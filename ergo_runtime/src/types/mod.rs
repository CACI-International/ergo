//! Script types.

use crate as ergo_runtime;
use crate::type_system::ergo_traits_fn;

pub mod args;
mod array;
mod bind_rest;
mod bool;
pub mod byte_stream;
mod error;
mod index;
mod iter;
mod map;
mod map_entry;
mod number;
mod path;
mod string;
mod unbound;
mod unit;
mod unset;

pub use ergo_runtime_macro::{ergo_fn, ergo_fn_value, ergo_pat, ergo_pat_value};

pub use self::bool::Bool;
pub use args::{Args, PatternArgs};
pub use array::Array;
pub use bind_rest::{BindRest, BindRestKey};
pub use byte_stream::ByteStream;
pub use error::Error;
pub use index::Index;
pub use iter::Iter;
pub use map::Map;
pub use map_entry::MapEntry;
pub use number::Number;
pub use path::Path;
pub use string::String;
pub use unbound::Unbound;
pub use unit::Unit;
pub use unset::Unset;

ergo_traits_fn! {
    unset::ergo_traits(traits);
    error::ergo_traits(traits);
    unit::ergo_traits(traits);
    self::bool::ergo_traits(traits);
    string::ergo_traits(traits);
    path::ergo_traits(traits);
    array::ergo_traits(traits);
    map::ergo_traits(traits);
    map_entry::ergo_traits(traits);
    byte_stream::ergo_traits(traits);
    unbound::ergo_traits(traits);
    iter::ergo_traits(traits);
    args::ergo_traits(traits);
    index::ergo_traits(traits);
    bind_rest::ergo_traits(traits);
    number::ergo_traits(traits);
}
