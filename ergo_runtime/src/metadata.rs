//! Common value metadata keys.

use crate as ergo_runtime;
use crate::abi_stable::uuid::Uuid;
use crate::value::{match_value, MetadataKey, Value};

/// Documentation metadata key.
pub struct Doc;

impl MetadataKey for Doc {
    type Value = Value;

    fn id(&self) -> u128 {
        crate::nsid!(meta::doc).as_u128()
    }
}

impl Doc {
    /// Get the documentation value for the given value.
    ///
    /// This retrieves the documentation.
    ///
    /// If no documentation is available, a string indicating the (possibly dynamic) type will be returned.
    pub async fn get(ctx: &crate::Context, value: &Value) -> crate::Result<String> {
        let mut value = value.clone();
        while value.get_metadata(&Doc).is_none() && !value.is_evaluated() {
            ctx.eval_once(&mut value).await;
        }

        if let Some(v) = value.get_metadata(&Doc) {
            let mut v = v.owned();
            ctx.eval(&mut v).await?;

            match_value! {v,
                crate::types::String(s) => return Ok(s.into()),
                _ => ()
            }
        }
        Ok(format!(
            "value with type '{}'",
            crate::traits::type_name(ctx, &value)
        ))
    }

    /// Set a documentation string for the given value.
    pub fn set_string<S: Into<crate::types::String>>(v: &mut Value, s: S) {
        Self::set(v, crate::types::String::new_no_doc(s.into()).into());
    }

    /// Set documentation for the given value.
    pub fn set(v: &mut Value, doc: Value) {
        v.set_metadata(&Doc, doc);
    }
}

/// Metadata key for any runtime value.
pub struct Runtime {
    /// The id to use as the metadata key.
    pub key: u128,
}

impl MetadataKey for Runtime {
    type Value = Value;

    fn id(&self) -> u128 {
        let mut id = crate::nsid!(meta::any);
        id = Uuid::new_v5(&id, self.key.to_string().as_bytes());
        id.as_u128()
    }
}
