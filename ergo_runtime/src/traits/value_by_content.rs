//! The ValueByContent grease trait.

use crate::types;
use abi_stable::StableAbi;
use grease::{
    depends, grease_trait, grease_trait_impl, grease_traits_fn, path::PathBuf, runtime::Traits,
    types::GreaseType, value::Value, Error,
};

/// A grease trait which identifies a value by its content.
#[grease_trait]
pub trait ValueByContent {
    async fn value_by_content(self, deep: bool) -> Value;
}

impl ValueByContent {
    /// Implement ValueByContent for the given type.
    pub fn add_impl<T: GreaseType + std::hash::Hash + Send + Sync + 'static>(traits: &mut Traits) {
        traits.add_impl_for_type::<T, ValueByContent>(grease_trait_impl! {
            impl<T: GreaseType + Send + Sync + std::hash::Hash + 'static> ValueByContent for T {
                async fn value_by_content(self, _deep: bool) -> Value {
                    let data = self.clone().await?;
                    let deps = depends![data.as_ref()];
                    Value::from(self).set_dependencies(deps)
                }
            }
        });
    }
}

pub async fn value_by_content(
    ctx: &grease::runtime::Context,
    v: Value,
    deep: bool,
) -> grease::Result<Value> {
    let t_ctx = ctx.clone();
    let mut t = ctx
        .get_trait::<ValueByContent, _, _>(&v, move |t| {
            let t = t.clone();
            let ctx = t_ctx.clone();
            async move {
                let name = super::type_name(&ctx, &t).await?;
                Err(format!("no value by content trait for {}", name).into())
            }
        })
        .await?;

    t.value_by_content(v, deep).await
}

grease_traits_fn! {
    ValueByContent::add_impl::<()>(traits);
    ValueByContent::add_impl::<types::Unit>(traits);
    ValueByContent::add_impl::<types::String>(traits);
    ValueByContent::add_impl::<bool>(traits);
    ValueByContent::add_impl::<PathBuf>(traits);

    impl ValueByContent for types::Array {
        async fn value_by_content(self, deep: bool) -> Value {
            let data = self.await?;
            let types::Array(vals) = data.as_ref();
            let mut inner_vals = Vec::new();
            let mut errs: Vec<Error> = Vec::new();
            if deep {
                for v in vals.iter() {
                    match super::value_by_content(CONTEXT, v.clone(), true).await {
                        Ok(v) => inner_vals.push(v),
                        Err(e) => errs.push(e)
                    }
                }
            } else {
                inner_vals = vals.iter().cloned().collect();
            }
            if !errs.is_empty() {
                Err(errs.into_iter().collect::<Error>())?
            }
            types::Array(inner_vals.into()).into()
        }
    }

    impl ValueByContent for types::Map {
        async fn value_by_content(self, deep: bool) -> Value {
            let data = self.await?;
            let types::Map(vals) = data.as_ref();
            let mut inner_vals = Vec::new();
            let mut errs: Vec<Error> = Vec::new();
            if deep {
                for (k,v) in vals.iter() {
                    match super::value_by_content(CONTEXT, v.clone(), true).await {
                        Ok(v) => inner_vals.push((k.clone(), v)),
                        Err(e) => errs.push(e)
                    }
                }
            } else {
                inner_vals = vals.iter().map(|(k,v)| (k.clone(), v.clone())).collect();
            }
            if !errs.is_empty() {
                Err(errs.into_iter().collect::<Error>())?
            }
            types::Map(inner_vals.into_iter().collect()).into_value(CONTEXT)
        }
    }
}
