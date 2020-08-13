//! Base environment.

use super::runtime::load_script;
use grease::value::Value;
use so_runtime::types;

/// Return the load function.
pub fn load() -> Value {
    types::Function::new(load_script).into()
}
