//! The ergo runtime support crate.
//!
//! All types (where necessary) are ABI-stable.

use std::iter::FromIterator;

pub use abi_stable_ext as abi_stable;
pub use futures::future;

/// The root namespace UUID, for use with v3/v5 UUIDs.
pub const NAMESPACE_ERGO: abi_stable::uuid::Uuid = abi_stable::uuid::Uuid::from_bytes([
    0x66, 0xfc, 0xed, 0x11, 0x59, 0x91, 0x4d, 0x46, 0xae, 0xd6, 0x70, 0x3c, 0xfd, 0x03, 0x70, 0x66,
]);

/// Create a uuid from the given namespaced name.
///
/// Example usage:
/// ```
/// # #[macro_use] extern crate ergo_runtime;
/// nsid!(string::format);
/// let val: &[u8] = &[1,2,3];
/// nsid!(something::other, val, val);
/// ```
#[macro_export]
macro_rules! nsid {
    ( $( $l:ident )::+ ) => {
        {
            static NSID_ONCE: std::sync::Once = std::sync::Once::new();
            static mut NSID: $crate::abi_stable::uuid::Uuid = $crate::NAMESPACE_ERGO;
            NSID_ONCE.call_once(|| unsafe {
                $( NSID = $crate::abi_stable::uuid::Uuid::new_v5(&NSID, stringify!($l).as_bytes()); )+
            });
            unsafe { NSID }
        }
    };
    ( $( $l:ident )::+, $( $e:expr ),+ ) => {
        {
            let mut id = $crate::NAMESPACE_ERGO;
            $( id = $crate::abi_stable::uuid::Uuid::new_v5(&id, stringify!($l).as_bytes()); )+
            $( id = $crate::abi_stable::uuid::Uuid::new_v5(&id, $e); )+
            id
        }
    };
}

pub mod context;
pub mod dependency;
pub mod error;
pub mod hash;
pub mod io;
pub mod metadata;
pub mod source;
pub mod traits;
pub mod type_system;
pub mod types;
pub mod value;

pub use context::Context;
pub use dependency::Dependencies;
pub use ergo_runtime_macro::plugin_entry;
pub use error::{Error, RResult, Result};
pub use source::Source;
pub use value::{TypedValue, Value};

pub trait ResultIterator<T> {
    /// Collect values into a Result, where errors will be aggregated.
    fn collect_result<R: FromIterator<T>>(self) -> Result<R>;
}

pub mod plugin {
    pub use plugin_tls::Context;
}

impl<T, I> ResultIterator<T> for I
where
    I: IntoIterator<Item = Result<T>>,
{
    fn collect_result<R: FromIterator<T>>(self) -> Result<R> {
        let mut errs = Vec::new();
        let r = R::from_iter(self.into_iter().filter_map(|v| match v {
            Err(e) => {
                errs.push(e);
                None
            }
            Ok(v) => Some(v),
        }));
        if errs.is_empty() {
            Ok(r)
        } else {
            Err(Error::aggregate(errs))
        }
    }
}

use crate as ergo_runtime;
type_system::ergo_traits_fn! {
    traits::ergo_traits(traits);
    types::ergo_traits(traits);
}
