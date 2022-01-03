//! The Error type.

use crate as ergo_runtime;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{
    depends, metadata::Source, traits, DependenciesConstant, GetDependenciesConstant, TypedValue,
};

/// Script error type.
pub use crate::Error;

impl GetDependenciesConstant for Error {
    fn get_depends(&self) -> DependenciesConstant {
        depends![Error::ergo_type(), self.to_string()]
    }
}

impl From<Error> for TypedValue<Error> {
    fn from(v: Error) -> Self {
        Self::constant(v)
    }
}

impl From<Error> for super::Bool {
    fn from(_: Error) -> Self {
        super::Bool(false)
    }
}

/// Return any error as an Error-typed Value.
#[macro_export]
macro_rules! try_result {
    ( $e:expr ) => {
        match $e {
            Ok(v) => v,
            Err(e) => {
                let e: $crate::Error = e.into();
                return $crate::Value::from(e);
            }
        }
    };
}

/// Return the value if it is an Error.
#[macro_export]
macro_rules! return_if_error {
    ( $v:ident $($t:tt)* ) => {
        if $v.is_type::<$crate::types::Error>() {
            return $v $($t)*;
        }
    };
}

/// Return the value as a Result::Err if it is an Error.
#[macro_export]
macro_rules! try_value {
    ( $v:expr ) => {
        match $v.as_type::<$crate::types::Error>() {
            Ok(v) => return Err(v.to_owned().into()),
            Err(v) => v,
        }
    };
}

impl<T, E> From<Result<T, E>> for crate::Value
where
    T: Into<crate::Value>,
    E: Into<crate::Error>,
{
    /// Convert a result to a value.
    fn from(result: Result<T, E>) -> Self {
        try_result!(result).into()
    }
}

ergo_traits_fn! {
    impl traits::Stored for Error {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            // Never store aborted errors.
            if self.is_aborted() {
                return crate::RResult::ROk(());
            }

            crate::error_info!(
                labels: [
                    primary(Source::get(SELF_VALUE).with("while storing this value"))
                ],
                { bincode::serialize_into(item, self) }
            ).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<crate::abi_stable::type_erase::Erased> {
            crate::error_info!({
                bincode::deserialize_from(item).map(|e: Error| crate::abi_stable::type_erase::Erased::new(e))
            }).into()
        }
    }

    traits::IntoTyped::<super::Bool>::add_impl::<Error>(traits);

    crate::ergo_display_basic!(traits, Error);
    crate::ergo_type_name!(traits, Error);
}
