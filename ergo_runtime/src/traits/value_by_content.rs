//! The ValueByContent grease trait.

use crate::{types, ResultIterator};
use abi_stable::StableAbi;
use grease::{
    depends, grease_trait, grease_trait_impl, grease_traits_fn, path::PathBuf, runtime::Traits,
    types::GreaseType, value::Value, Error,
};

/// A grease trait which identifies a value by its content.
#[grease_trait]
pub trait ValueByContent {
    async fn value_by_content(self) -> Value;
}

impl ValueByContent {
    /// Implement ValueByContent for the given type.
    pub fn add_impl<T: GreaseType + std::hash::Hash + Send + Sync + 'static>(traits: &mut Traits) {
        traits.add_impl_for_type::<T, ValueByContent>(grease_trait_impl! {
            impl<T: GreaseType + Send + Sync + std::hash::Hash + 'static> ValueByContent for T {
                async fn value_by_content(self) -> Value {
                    let mut v = self;
                    let data = (&mut v).await?;
                    let deps = depends![data.as_ref()];
                    Value::from(v).set_dependencies(deps)
                }
            }
        });
    }
}

grease_traits_fn! {
    ValueByContent::add_impl::<types::Unit>(traits);
    ValueByContent::add_impl::<types::String>(traits);
    ValueByContent::add_impl::<bool>(traits);
    ValueByContent::add_impl::<PathBuf>(traits);

    impl ValueByContent for types::Array {
        async fn value_by_content(self) -> Value {
            let data = self.await?;
            let types::Array(vals) = data.as_ref();
            let mut inner_vals = Vec::new();
            let mut errs: Vec<Error> = Vec::new();
            for v in vals.iter() {
                match CONTEXT.get_trait::<ValueByContent>(v) {
                    Some(mut t) => inner_vals.push(async move { Ok(t.value_by_content(v.clone()).await?) }),
                    None => errs.push(format!(
                        "ValueByContent not implemented for {}",
                        super::type_name(CONTEXT, v.clone().grease_type().await?).await?
                    )
                    .into()),
                }
            }
            if !errs.is_empty() {
                Err(errs.into_iter().collect::<Error>())?
            }
            futures::future::join_all(inner_vals)
                .await
                .into_iter()
                .collect_result()
                .map(|v| types::Array(v).into())?
        }
    }

    impl ValueByContent for types::Map {
        async fn value_by_content(self) -> Value {
            let data = self.await?;
            let types::Map(vals) = data.as_ref();
            let mut inner_vals = Vec::new();
            let mut errs: Vec<Error> = Vec::new();
            for (k,v) in vals.iter() {
                match CONTEXT.get_trait::<ValueByContent>(v) {
                    Some(mut t) => inner_vals.push(async move {
                        Ok((k.clone(), t.value_by_content(v.clone()).await?))
                    }),
                    None => errs.push(format!(
                        "ValueByContent not implemented for {}",
                        super::type_name(CONTEXT, v.clone().grease_type().await?).await?
                    )
                    .into()),
                }
            }
            if !errs.is_empty() {
                Err(errs.into_iter().collect::<Error>())?
            }
            futures::future::join_all(inner_vals)
                .await
                .into_iter()
                .collect_result()
                .map(|vals| types::Map(vals).into())?
        }
    }
}
