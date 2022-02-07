#[cfg_attr(unix, path = "unix.rs")]
#[cfg_attr(not(unix), path = "generic.rs")]
mod os;

pub use os::*;
