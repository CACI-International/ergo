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
    ( $( $l:ident )::+, const $( $e:expr ),+ ) => {
        {
            static NSID_ONCE: std::sync::Once = std::sync::Once::new();
            static mut NSID: $crate::abi_stable::uuid::Uuid = $crate::NAMESPACE_ERGO;
            NSID_ONCE.call_once(|| unsafe {
                $( NSID = $crate::abi_stable::uuid::Uuid::new_v5(&NSID, stringify!($l).as_bytes()); )+
                $( NSID = $crate::abi_stable::uuid::Uuid::new_v5(&NSID, $e); )+
            });
            unsafe { NSID }
        }
    };
    ( $( $l:ident )::+, $( $e:expr ),+ ) => {
        {
            static NSID_ONCE: std::sync::Once = std::sync::Once::new();
            static mut NSID: $crate::abi_stable::uuid::Uuid = $crate::NAMESPACE_ERGO;
            NSID_ONCE.call_once(|| unsafe {
                $( NSID = $crate::abi_stable::uuid::Uuid::new_v5(&NSID, stringify!($l).as_bytes()); )+
            });
            let mut ret = unsafe { NSID }.clone();
            $( ret = $crate::abi_stable::uuid::Uuid::new_v5(&ret, $e); )+
            ret
        }
    };
}

pub mod context;
pub mod dependency;
pub mod error;
pub mod gc;
pub mod hash;
pub mod io;
pub mod metadata;
pub mod source;
pub mod traits;
pub mod type_system;
pub mod types;
pub mod value;

pub use context::Context;
pub use dependency::{
    Dependencies, DependenciesConstant, GetDependencies, GetDependenciesConstant,
};
pub use ergo_runtime_macro::{lazy_value, plugin_entry};
pub use error::{Error, RResult, Result};
pub use source::Source;
pub use value::{EvaluatedValue, IdentifiedValue, TypedValue, Value};

pub use context::task::runtime as async_executor;

pub trait ResultIterator<T> {
    /// Collect values into a Result, where errors will be aggregated.
    fn collect_result<R: FromIterator<T>>(self) -> Result<R>;
}

pub mod plugin {
    use abi_stable_ext::{log, std_types::ROption};

    // This struct _must_ be abi-stable, but `tls` and `log_level` (while abi-stable) don't derive
    // StableAbi, so we don't use the derivation here.
    #[repr(C)]
    pub struct Context {
        tls: plugin_tls::Context,
        log: ROption<log::Log>,
        log_level: log::LevelFilter,
        source: crate::Source<()>,
    }

    impl Context {
        pub fn get(source: crate::Source<()>) -> Self {
            Context {
                tls: plugin_tls::Context::get(),
                log: log::logger().into(),
                log_level: log::max_level(),
                source,
            }
        }

        pub unsafe fn initialize(self) -> crate::Source<()> {
            self.tls.initialize();
            if let ROption::RSome(log) = self.log {
                if let Err(e) = log::set_boxed_logger(Box::new(log)) {
                    eprintln!("error while setting logger: {}", e);
                }
            }
            log::set_max_level(self.log_level);
            self.source
        }

        pub fn reset() {
            plugin_tls::Context::reset();
        }
    }
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
