//! UUID namespaces and utilities.

use abi_stable::{std_types::RBoxError, StableAbi};

pub type Bytes = uuid::Bytes;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, StableAbi)]
#[repr(C)]
pub struct Uuid(Bytes);

impl Uuid {
    pub const fn from_bytes(bytes: Bytes) -> Self {
        Uuid(bytes)
    }

    pub fn as_bytes(&self) -> &Bytes {
        &self.0
    }

    pub fn new_v5(namespace: &Uuid, name: &[u8]) -> Self {
        Self::from(uuid::Uuid::new_v5(
            &uuid::Uuid::from_bytes(*namespace.as_bytes()),
            name,
        ))
    }
}

impl From<uuid::Uuid> for Uuid {
    fn from(u: uuid::Uuid) -> Self {
        Uuid::from_bytes(*u.as_bytes())
    }
}

impl From<Uuid> for uuid::Uuid {
    fn from(u: Uuid) -> Self {
        uuid::Uuid::from_bytes(u.0)
    }
}

impl std::fmt::Display for Uuid {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", uuid::Uuid::from(*self))
    }
}

impl Default for Uuid {
    fn default() -> Self {
        Uuid::from(uuid::Uuid::default())
    }
}

impl std::str::FromStr for Uuid {
    type Err = RBoxError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        uuid::Uuid::from_str(s)
            .map(Uuid::from)
            .map_err(|e| Self::Err::from_box(e.into()))
    }
}

/// The root namespace UUID, for use with v3/v5 UUIDs.
pub const NAMESPACE_GREASE: Uuid = Uuid::from_bytes([
    0x66, 0xfc, 0xed, 0x11, 0x59, 0x91, 0x4d, 0x46, 0xae, 0xd6, 0x70, 0x3c, 0xfd, 0x03, 0x70, 0x66,
]);

/// Create a v5 UUID within the grease namespace.
pub fn grease_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&NAMESPACE_GREASE, name)
}
