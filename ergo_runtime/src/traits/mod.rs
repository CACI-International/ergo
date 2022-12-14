//! Common ergo traits used in the runtime.

mod bind;
mod display;
mod functor;
mod into;
mod nested;
mod stored;
mod type_name;

use crate as ergo_runtime;
pub use bind::{bind, bind_error, bind_no_error, Bind, BindImpl};
pub(crate) use bind::{bind_array, bind_map};
pub use display::{display, display_any, to_string, Display, DisplayImpl, Formatter};
pub use functor::{deep_eval, map, Functor, FunctorImpl};
pub use into::{into, into_for, into_trait, IntoTyped, IntoTypedImpl};
pub use nested::{eval_nested, Nested, NestedImpl, NestedValues};
pub use stored::{GetData, GetDataInterface, PutData, PutDataInterface, Stored, StoredImpl};
pub use type_name::{
    type_error, type_error_for, type_error_for_t, type_name, type_name_for, TypeName, TypeNameImpl,
};

// Add trait implementations to the runtime.
crate::type_system::ergo_traits_fn! {
    bind::ergo_traits(traits);
    into::ergo_traits(traits);
}
