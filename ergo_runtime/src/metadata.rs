//! Common value metadata keys.

use crate::ContextExt;
use futures::future::FutureExt;
use grease::uuid::{grease_uuid, Uuid};
use grease::value::{MetadataKey, TypedValue};
use grease::{depends, make_value};

/// Documentation metadata key.
///
/// Documentation is an unbound value so that the `self` value can be bound without documentation
/// accidentally making a reference loop.
pub struct Doc;

impl MetadataKey for Doc {
    type Value = TypedValue<crate::types::Unbound>;

    fn id(&self) -> u128 {
        grease_uuid(b"doc").as_u128()
    }
}

impl Doc {
    /// Get the documentation value for the given value.
    ///
    /// This retrieves the documentation and binds it to the value.
    ///
    /// If no documentation is available, a string indicating the (possibly dynamic) type will be returned.
    pub async fn get(
        ctx: &mut crate::Runtime,
        value: &grease::Value,
    ) -> crate::Result<TypedValue<crate::types::String>> {
        match value.get_metadata(&Doc) {
            Some(v) => {
                let val = crate::traits::bind(
                    ctx,
                    crate::Source::builtin(v.as_ref().clone().into()),
                    crate::Source::builtin(value.clone()),
                )
                .await?;
                Ok(ctx
                    .source_value_as::<crate::types::String>(val)
                    .await?
                    .unwrap())
            }
            None => {
                let tp = match value.grease_type_immediate() {
                    Some(tp) => {
                        let ctx = ctx.clone();
                        let tp = tp.clone();
                        async move { crate::traits::type_name(&ctx, &tp).await }.boxed()
                    }
                    None => async move { Ok("<dynamic>".to_owned()) }.boxed(),
                };
                Ok(make_value!(tp.await.map(|s| crate::types::String::from(
                    format!("value with type '{}'", s)
                ))))
            }
        }
    }

    /// Set a documentation string for the given value.
    pub fn set_string<S: Into<crate::types::String>>(v: &mut grease::Value, s: S) {
        let s = s.into();
        Self::set_value(v, make_value!(Ok(s)));
    }

    /// Set a documentation value for the given value.
    pub fn set_value(v: &mut grease::Value, doc: TypedValue<crate::types::String>) {
        let deps = depends![doc];
        v.set_metadata(
            &Doc,
            crate::types::Unbound::new(
                move |_, _| {
                    let doc = doc.clone();
                    async move { Ok(doc.into()) }.boxed()
                },
                deps,
                (),
            ),
        );
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
