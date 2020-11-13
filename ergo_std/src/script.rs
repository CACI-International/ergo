//! Script runtime module.

use ergo_runtime::{ergo_function, types, ResultIterator};
use grease::value::Value;

pub fn module() -> Value {
    crate::grease_string_map! {
        "bindings" = bindings_fn()
    }
}

fn bindings_fn() -> Value {
    ergo_function!(independent std::script::bindings, |ctx| {
        ctx.unused_arguments()?;

        let env = ctx.env_flatten();

        types::Map(env.into_iter()
            .map(|(k,v)| Ok((k,v.into_result()?.unwrap())))
            .collect_result()?
        ).into()
    })
    .into()
}
