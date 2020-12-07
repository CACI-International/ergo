//! Base environment.

use super::runtime::load_script;
use ergo_runtime::{namespace_id, types};
use grease::{
    depends,
    value::{TypedValue, Value},
};

/// Documentation for the load function.
pub const LOAD_DOCUMENTATION: &'static str = r"Load a script, with optional additional arguments to call the result with.

The first argument, if present, must be a string or path.

## Script resolution
When loading a script, the following resolution process occurs for the first argument (if present):
1. Filesystem Name Resolution
   a. If the passed script is an existing path in one of the load path directories, it is used.
   b. If the passed script with the `.ergo` extension appended is an existing path in one of the
      load path directories, it is used.

   The load path is checked from first to last, and is determined by the location of `ergo` and the currently-executing
   script. By default, the load path contains the directory containing the currently-executing script (or if there is no
   script, the current working directory), followed by user and system directories.
2. Filesystem Directory Resolution
   a. If the name-resolved script exists as a file, it is used.
   b. If the name-resolved script exists as a directory, and the directory contains `dir.ergo`,
      that path is used and step (2) is repeated.
   c. If the name-resolved script exists as a directory, and the directory contains `workspace.ergo`,
      that path is used and step (2) is repeated.

If the directory-resolved script exists as a file, it is loaded. If additional arguments were provided, they are applied
to the resulting value.

Otherwise, if an ancestor directory contains a `workspace.ergo` path, resolution occurs as if
`ergo /path/to/ancestor/workspace.ergo command |> [ARGS...]` was run. That is, a string literal `command` is used to
get a command-handler from the workspace and the arguments are passed to it. If there was no first argument, this
workspace resolution occurs immediately and the result of `command` is returned as-is (without application).

## Script Prelude
When loading a script, if an ancestor directory contains a `workspace.ergo` path, the script loads as if it contains
`^ergo /path/to/ancestor/workspace.ergo prelude` as the first line. That is, a string literal `prelude` is applied to
the loaded workspace and the resulting value (which must be a map) is merged into the top-level block. If the workspace
value fails to apply `prelude`, the error is ignored and nothing is merged into the top-level block.

Note that workspaces do not load preludes (though one could still explicitly load from a parent workspace).";

/// Return the load function.
pub fn load() -> Value {
    types::Function::new(
        load_script,
        depends![namespace_id!(load)],
        Some(TypedValue::constant(LOAD_DOCUMENTATION.into())),
    )
    .into()
}
