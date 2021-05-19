//! The TypeName ergo trait and helper utilities.

use crate as ergo_runtime;
use crate::abi_stable::std_types::RString;
use crate::type_system::{ergo_trait, ErgoType, Type};
use crate::{Source, Value};

/// An ergo trait describing the name of a type.
#[ergo_trait]
pub trait TypeName {
    fn type_name() -> RString;
}

/// Get a type name for the given type.
pub fn type_name_for(ctx: &crate::Context, tp: &Type) -> String {
    if let Some(t) = ctx.get_trait_for_type::<TypeName>(tp) {
        t.type_name().into()
    } else {
        format!("<{}>", tp.id)
    }
}

/// Get a type name for the given value.
pub fn type_name(ctx: &crate::Context, value: &Value) -> String {
    match value.ergo_type() {
        Some(tp) => type_name_for(ctx, tp),
        None => "<dynamic>".into(),
    }
}

/// Create a type error with the value's type mentioned.
pub fn type_error(ctx: &crate::Context, v: Source<crate::Value>, expected: &str) -> crate::Error {
    let (src, v) = v.take();
    let name = type_name(ctx, &v);
    src.with(format!("type error: expected {}, got {}", expected, name))
        .into_error()
}

/// Create a type error with the value's type mentioned.
pub fn type_error_for<T: ErgoType>(ctx: &crate::Context, v: Source<crate::Value>) -> crate::Error {
    type_error(ctx, v, type_name_for(ctx, &T::ergo_type()).as_str())
}

/// Define the TypeName trait for the given rust type.
///
/// One must still add the trait implementation to the runtime.
#[macro_export]
macro_rules! ergo_type_name {
    ( $traits:expr, $t:ty, $n:expr ) => {
        $traits.add_impl_for_type::<$t, $crate::traits::TypeName>(
            $crate::type_system::ergo_trait_impl! {
                impl $crate::traits::TypeName for $t {
                    fn type_name() -> abi_stable::std_types::RString {
                        $n.into()
                    }
                }
            },
        );
    };
    ( $traits:expr, $t:ty ) => {
        $crate::ergo_type_name!($traits, $t, stringify!($t));
    };
}
