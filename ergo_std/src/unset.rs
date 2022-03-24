//! Unset functions.

use ergo_runtime::{type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Unset::ergo_type(),
        index: types::Map(Default::default()).into(),
    }
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn unset(t) {
            t.assert_success("self:Unset _ = {}:key");
            t.assert_success("self:Unset _ = $unset");
            t.assert_fail("self:Unset _ = str");
        }
    }
}
