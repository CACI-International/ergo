//! The Display grease trait and helper utilities.
//!
//! TODO: Change this trait to use something that supports Write rather than putting everything
//! into a String.

use super::type_name;
use crate::types;
use abi_stable::{std_types::RString, StableAbi};
use grease::path::PathBuf;
use grease::runtime::Context;
use grease::value::Value;
use grease::{grease_trait, grease_traits_fn};

/// The Display grease trait.
#[grease_trait]
pub trait Display {
    async fn fmt(&self) -> RString;
}

/// Return a string representing the display of the given value.
pub async fn display(ctx: &Context, v: Value) -> grease::Result<String> {
    let trt = ctx
        .get_trait::<Display, _, _>(&v, |_| async {
            Ok(grease::grease_trait_impl! {
                impl Display for _ {
                    async fn fmt(&self) -> RString {
                        type_name(CONTEXT, SELF_TYPE)
                            .await
                            .map(|name| format!("<cannot display values of type {}>", name))?
                            .into()
                    }
                }
            })
        })
        .await;
    match trt {
        Err(e) => Err(e),
        Ok(mut t) => Ok(t.fmt(v).await?.into()),
    }
}

#[macro_export]
macro_rules! grease_display_basic {
    ( $traits:expr, $t:ty ) => {
        $traits.add_impl_for_type::<$t, $crate::traits::Display>(grease::grease_trait_impl! {
            impl $crate::traits::Display for $t {
                async fn fmt(&self) -> abi_stable::std_types::RString {
                    self.to_string().into()
                }
            }
        });
    };
}

grease_traits_fn! {
    impl Display for types::Unit {
        async fn fmt(&self) -> RString {
            Default::default()
        }
    }

    impl Display for PathBuf {
        async fn fmt(&self) -> RString {
            format!("{}", self.as_ref().display()).into()
        }
    }

    fn to_empty(s: String) -> String {
        if s.is_empty() {
            "(empty)".to_owned()
        } else {
            s
        }
    }

    impl Display for types::Array {
        async fn fmt(&self) -> RString {
            let mut strs = Vec::new();
            for v in self.0.iter() {
                strs.push(to_empty(display(CONTEXT, v.clone()).await?));
            }
            strs.join("\n").into()
        }
    }

    impl Display for types::Map {
        async fn fmt(&self) -> RString {
            let mut strs = Vec::new();
            for (k,v) in self.0.iter() {
                strs.push(format!("{} -> {}", to_empty(display(CONTEXT, k.clone()).await?), to_empty(display(CONTEXT, v.clone()).await?)));
            }
            strs.join("\n").into()
        }
    }

    impl Display for types::MapEntry {
        async fn fmt(&self) -> RString {
            format!(
                "{} -> {}",
                to_empty(display(CONTEXT, self.key.clone()).await?),
                to_empty(display(CONTEXT,self.value.clone()).await?),
            ).into()
        }
    }

    grease_display_basic!(traits, types::Bool);
    grease_display_basic!(traits, types::String);
}
