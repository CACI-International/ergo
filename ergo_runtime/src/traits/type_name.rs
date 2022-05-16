//! The TypeName ergo trait and helper utilities.

use crate as ergo_runtime;
use crate::abi_stable::std_types::RString;
use crate::error::{Diagnostic, DiagnosticInfo, ErrorOrDiagnostic};
use crate::type_system::{ergo_trait, ErgoType, Type};
use crate::Value;

/// An ergo trait describing the name of a type.
#[ergo_trait]
pub trait TypeName {
    fn type_name() -> RString;
}

/// Get a type name for the given type.
pub fn type_name_for(tp: &Type) -> String {
    if let Some(t) = crate::Context::get_trait_for_type::<TypeName>(tp) {
        t.type_name().into()
    } else {
        format!("<{}>", tp.id)
    }
}

/// Get a type name for the given value.
pub fn type_name(value: &Value) -> String {
    match value.ergo_type() {
        Some(tp) => type_name_for(tp),
        None => "<dynamic>".into(),
    }
}

/// Create a type error with the value's type mentioned.
pub fn type_error(v: Value, expected: &str) -> ErrorOrDiagnostic {
    let v = match v.as_type::<crate::types::Error>() {
        Ok(e) => return ErrorOrDiagnostic::Error(e.to_owned()),
        Err(v) => v,
    };
    let name = type_name(&v);
    ErrorOrDiagnostic::Diagnostic(
        Diagnostic::from(format!("type error: expected {}, got {}", expected, name))
            .add_value_sources("value", &v),
    )
}

/// Create a type error with the value's type mentioned.
pub fn type_error_for<T: ErgoType>(v: Value) -> ErrorOrDiagnostic {
    type_error_for_t(v, &T::ergo_type())
}

/// Create a type error with the value's type mentioned.
pub fn type_error_for_t(v: Value, tp: &Type) -> ErrorOrDiagnostic {
    type_error(v, type_name_for(tp).as_str())
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
