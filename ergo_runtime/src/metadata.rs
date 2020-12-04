//! Common value metadata keys.

use futures::future::FutureExt;
use grease::make_value;
use grease::uuid::{grease_uuid, Uuid};
use grease::value::{MetadataKey, TypedValue};

/// Documentation metadata key.
pub struct Doc;

impl MetadataKey for Doc {
    type Value = TypedValue<crate::types::String>;

    fn id(&self) -> u128 {
        grease_uuid(b"doc").as_u128()
    }
}

impl Doc {
    /// Get a documentation string for the given value.
    ///
    /// If no documentation is available, a string indicating the (possibly dynamic) type will be returned.
    pub fn get(
        ctx: &grease::runtime::Context,
        value: &grease::Value,
    ) -> TypedValue<crate::types::String> {
        match value.get_metadata(&Doc) {
            Some(v) => v.as_ref().clone(),
            None => {
                let tp = match value.grease_type_immediate() {
                    Some(tp) => {
                        let ctx = ctx.clone();
                        let tp = tp.clone();
                        async move { crate::traits::type_name(&ctx, &tp).await }.boxed()
                    }
                    None => async move { Ok("<dynamic>".to_owned()) }.boxed(),
                };
                make_value!(tp.await.map(|s| format!("value with type '{}'", s).into()))
            }
        }
    }
}

/// Metadata key for any runtime value.
pub struct Runtime {
    /// The id to use as the metadata key.
    pub key: u128,
}

impl MetadataKey for Runtime {
    type Value = grease::Value;

    fn id(&self) -> u128 {
        let mut id = grease_uuid(b"any");
        id = Uuid::new_v5(&id, self.key.to_string().as_bytes());
        id.as_u128()
    }
}
