//! The TypeName grease trait and helper utilities.

use crate::types;
use abi_stable::{std_types::RString, StableAbi};
use grease::path::PathBuf;
use grease::types::Type;
use grease::{grease_trait, grease_traits_fn};

/// A grease trait describing the name of a type.
#[grease_trait]
pub trait TypeName {
    async fn type_name() -> RString;
}

/// Get a type name for the given type.
pub async fn type_name(ctx: &grease::runtime::Context, tp: &Type) -> grease::Result<String> {
    if let Some(mut t) = ctx.get_trait_for_type::<TypeName>(tp) {
        t.type_name().await.map(|s| s.into())
    } else {
        Ok(format!("<{}>", tp.id))
    }
}

/// Define the GreaseTypeName trait for the given rust type.
///
/// One must still add the trait implementation to the runtime using `impl_type_name`.
#[macro_export]
macro_rules! grease_type_name {
    ( $traits:expr, $t:ty, $n:expr ) => {
        $traits.add_impl_for_type::<$t, $crate::traits::TypeName>(grease::grease_trait_impl! {
        impl $crate::traits::TypeName for $t {
            async fn type_name() -> abi_stable::std_types::RString {
                $n.into()
            }
        }});
    };
    ( $traits:expr, $t:ty ) => {
        $crate::grease_type_name!($traits, $t, stringify!($t));
    };
}

grease_traits_fn! {
    grease_type_name!(traits, ());
    grease_type_name!(traits, u8);
    grease_type_name!(traits, i8);
    grease_type_name!(traits, u16);
    grease_type_name!(traits, i16);
    grease_type_name!(traits, u32);
    grease_type_name!(traits, i32);
    grease_type_name!(traits, u64);
    grease_type_name!(traits, i64);
    grease_type_name!(traits, usize);
    grease_type_name!(traits, isize);
    grease_type_name!(traits, char);
    grease_type_name!(traits, bool);
    grease_type_name!(traits, PathBuf, "Path");
    grease_type_name!(traits, types::Unit, "Unit");
    grease_type_name!(traits, types::String, "String");
    grease_type_name!(traits, types::Array, "Array");
    grease_type_name!(traits, types::Map, "Map");
    grease_type_name!(traits, types::Unbound, "Function");
    grease_type_name!(traits, types::Merge, "Merge");
    grease_type_name!(traits, types::BindRest, "BindRest");
    grease_type_name!(traits, types::Args, "Args");
    grease_type_name!(traits, types::BindArgs, "BindArgs");
}
