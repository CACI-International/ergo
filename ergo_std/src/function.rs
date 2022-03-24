//! Function type.

use ergo_runtime::{type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Unbound::ergo_type(),
        index: types::Map(Default::default()).into(),
    }
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn function(t) {
            t.assert_success("self:Function _ = :a -> $a");
            t.assert_fail("self:Function _ = ()");
        }
    }
}
