//! ABI-stable UUIDs.

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

    pub fn as_u128(&self) -> u128 {
        uuid::Uuid::from_bytes(*self.as_bytes()).as_u128()
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
