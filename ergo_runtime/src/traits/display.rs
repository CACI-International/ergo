//! The Display grease trait and helper utilities.

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

/// Return a type that implements `std::fmt::Display`, that will display the given value using the
/// grease trait Display.
///
/// The value (and any internal values) must already be evaluated.
pub async fn display(ctx: &Context, v: Value) -> grease::Result<String> {
    let t_ctx = ctx.clone();
    let trt = ctx
        .get_trait::<Display, _, _>(&v, move |t| {
            let ctx = t_ctx.clone();
            let t = t.clone();
            async move {
                type_name(&ctx, &t)
                    .await
                    .map(|name| format!("<cannot display values of type {}>", name).into())
                    .unwrap_or_else(|e| e)
            }
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
    impl Display for () {
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

    grease_display_basic!(traits, u8);
    grease_display_basic!(traits, i8);
    grease_display_basic!(traits, u16);
    grease_display_basic!(traits, i16);
    grease_display_basic!(traits, u32);
    grease_display_basic!(traits, i32);
    grease_display_basic!(traits, u64);
    grease_display_basic!(traits, i64);
    grease_display_basic!(traits, usize);
    grease_display_basic!(traits, isize);
    grease_display_basic!(traits, char);
    grease_display_basic!(traits, bool);
    grease_display_basic!(traits, RString);
}
