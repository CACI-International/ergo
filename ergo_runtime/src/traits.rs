//! Common grease traits used within ergo.

mod display;
mod into;
mod nested;
mod stored;
mod type_name;
mod value_by_content;

pub use display::{display, try_display, Display, GreaseDisplay};
pub use into::IntoTyped;
pub use nested::{force_value_nested, GreaseNested, Nested};
pub use stored::{read_from_store, write_to_store, GreaseStored, Stored, StoredContext};
pub use type_name::{type_name, GreaseTypeName, TypeName};
pub use value_by_content::ValueByContent;

/// Add trait implementations to the runtime.
pub fn traits(traits: &mut grease::runtime::Traits) {
    display::traits(traits);
    into::traits(traits);
    nested::traits(traits);
    stored::traits(traits);
    type_name::traits(traits);
    value_by_content::traits(traits);
}
