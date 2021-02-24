//! Common grease traits used within ergo.

mod bind;
mod display;
mod into;
mod nested;
mod stored;
mod type_name;
mod value_by_content;

pub use bind::{bind, bind_error, delay_bind, Bind, BindImpl};
pub use display::{display, Display, DisplayImpl};
pub use into::{into, into_sourced, IntoTyped, IntoTypedImpl};
pub use nested::{force_value_nested, Nested, NestedImpl};
pub use stored::{
    present_in_store, read_from_store, write_to_store, Stored, StoredContext, StoredImpl,
};
pub use type_name::{type_error, type_name, TypeName, TypeNameImpl};
pub use value_by_content::{value_by_content, ValueByContent, ValueByContentImpl};

/// Add trait implementations to the runtime.
pub fn traits(traits: &mut grease::runtime::Traits) {
    bind::traits(traits);
    display::traits(traits);
    into::traits(traits);
    nested::traits(traits);
    stored::traits(traits);
    type_name::traits(traits);
    value_by_content::traits(traits);
    crate::types::byte_stream::traits(traits);
}
