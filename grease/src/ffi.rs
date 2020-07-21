//! ABI-stable ffi types.
//!
//! The implementation only supports converting to/from `std::ffi::OsString`.

#[cfg(unix)]
mod unix {
    use abi_stable::{std_types::RVec, StableAbi};
    use std::os::unix::ffi::OsStringExt;

    #[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, StableAbi)]
    #[repr(C)]
    pub struct OsString {
        bytes: RVec<u8>,
    }

    impl OsString {
        pub fn into_os_string(self) -> std::ffi::OsString {
            self.into()
        }
    }

    impl From<std::ffi::OsString> for OsString {
        fn from(s: std::ffi::OsString) -> Self {
            OsString {
                bytes: s.into_vec().into(),
            }
        }
    }

    impl From<OsString> for std::ffi::OsString {
        fn from(s: OsString) -> Self {
            std::ffi::OsString::from_vec(s.bytes.into())
        }
    }
}

#[cfg(unix)]
pub use unix::OsString;

#[cfg(windows)]
mod windows {
    use abi_stable::{std_types::RVec, StableAbi};
    use std::os::windows::ffi::OsStrExt;
    use std::os::windows::ffi::OsStringExt;

    #[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, StableAbi)]
    #[repr(C)]
    pub struct OsString {
        wide_chars: RVec<u16>,
    }

    impl OsString {
        pub fn into_os_string(self) -> std::ffi::OsString {
            self.into()
        }
    }

    impl From<std::ffi::OsString> for OsString {
        fn from(s: std::ffi::OsString) -> Self {
            OsString {
                wide_chars: s.encode_wide().collect(),
            }
        }
    }

    impl From<OsString> for std::ffi::OsString {
        fn from(s: OsString) -> Self {
            std::ffi::OsString::from_wide(&s.wide_chars)
        }
    }
}

#[cfg(windows)]
pub use windows::OsString;
