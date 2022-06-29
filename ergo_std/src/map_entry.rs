//! MapEntry functions.

use ergo_runtime::{nsid, traits, type_system::ErgoType, types, Context, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::MapEntry::ergo_type(),
        index: crate::make_string_map! {
            "new" = new(),
            "@" = bind()
        },
    }
    .into()
}

#[types::ergo_fn]
/// Create a new MapEntry.
///
/// Arguments: `:key :value`
async fn new(key: _, value: _) -> Value {
    types::MapEntry { key, value }.into()
}

#[types::ergo_fn]
/// Bind to a MapEntry.
///
/// Arguments: `:key :value`
///
/// Binds `key` to the MapEntry key and `value` to the MapEntry value.
async fn bind(key: _, value: _) -> Value {
    types::unbound_value! {
        #![depends(const nsid!(std::MapEntry::bind))]
        #![contains(key,value)]
        let entry = Context::eval_as::<types::MapEntry>(ARG).await?.into_owned();
        traits::bind_no_error(key, entry.key).await?;
        traits::bind_no_error(value, entry.value).await?;
        types::Unit.into()
    }
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn map_entry(t) {
            t.assert_script_success("self:MapEntry:new k v");
            t.assert_eq("(self:MapEntry:new k v):key","k");
            t.assert_eq("(self:MapEntry:new k v):value","v");
            t.assert_eq("self:MapEntry:@ :k :v = self:MapEntry:new key value; $k", "key");
            t.assert_eq("self:MapEntry:@ :k :v = self:MapEntry:new key value; $v", "value");
        }
    }
}
