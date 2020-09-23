//! Base environment.

use super::runtime::load_script;
use ergo_runtime::{namespace_id, types};
use grease::{depends, value::Value};

/// Return the load function.
pub fn load() -> Value {
    types::Function::new(load_script, depends![namespace_id!(load)]).into()
}
