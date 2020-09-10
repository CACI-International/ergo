//! Base environment.

use super::runtime::load_script;
use ergo_runtime::types;
use grease::value::Value;

/// Return the load function.
pub fn load() -> Value {
    types::Function::new(load_script).into()
}
