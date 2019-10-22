//! UUID namespaces and utilities.

use crate::prelude::*;
use uuid::Uuid;

lazy_static! {
    /// The root namespace UUID, for use with v3/v5 UUIDs.
    pub static ref ROOT_NS: Uuid = Uuid::from_bytes(
        [0x66,0xfc,0xed,0x11,0x59,0x91,0x4d,0x46,
        0xae,0xd6,0x70,0x3c,0xfd,0x03,0x70,0x66]);

    /// The procedure namespace UUID.
    pub static ref PROCEDURE_NS: Uuid = Uuid::new_v5(&*ROOT_NS, b"procedure");

    /// The type namespace UUID.
    pub static ref TYPE_NS: Uuid = Uuid::new_v5(&*ROOT_NS, b"type");
}

/// Create a new procedure Uuid with the given string digest.
pub fn proc_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*PROCEDURE_NS, name)
}

/// Create a new type Uuid with the given string digest.
pub fn type_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*TYPE_NS, name)
}
