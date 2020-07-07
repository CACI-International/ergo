//! Load external files.

use super::{builtin_function_prelude::*, load_script, Source};

def_builtin!(ctx => load_script(ctx).map(|v| v.map(Source::unwrap)));
