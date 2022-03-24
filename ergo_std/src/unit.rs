//! Unit functions.

use ergo_runtime::{type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Unit::ergo_type(),
        index: types::Map(Default::default()).into(),
    }
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn unit(t) {
            t.assert_success("self:Unit _ = ()");
            t.assert_fail("self:Unit _ = str");
        }
    }
}
