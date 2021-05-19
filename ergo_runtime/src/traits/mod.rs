//! Common ergo traits used in the runtime.

mod bind;
mod display;
mod into;
mod nested;
mod stored;
mod type_name;
mod value_by_content;

use crate as ergo_runtime;
pub use bind::{bind, bind_error, Bind, BindImpl};
pub(crate) use bind::{bind_array, bind_map};
pub use display::{display, Display, DisplayImpl, Formatter};
pub use into::{into, into_sourced, IntoTyped, IntoTypedImpl};
pub use nested::{eval_nested, Nested, NestedImpl, NestedValues};
pub use stored::{
    present_in_store, read_from_store, write_to_store, Stored, StoredContext, StoredImpl,
};
pub use type_name::{type_error, type_error_for, type_name, type_name_for, TypeName, TypeNameImpl};
pub use value_by_content::{value_by_content, ValueByContent, ValueByContentImpl};

// Add trait implementations to the runtime.
crate::type_system::ergo_traits_fn! {
    bind::ergo_traits(traits);
    into::ergo_traits(traits);
}
