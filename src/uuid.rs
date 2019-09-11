//! UUID namespaces and utilities.

use crate::prelude::*;
use uuid::Uuid;

lazy_static! {
    /// The root namespace UUID, for use with v3/v5 UUIDs.
    pub static ref ROOT_NS: Uuid = Uuid::from_bytes(
        [0x66,0xfc,0xed,0x11,0x59,0x91,0x4d,0x46,
        0xae,0xd6,0x70,0x3c,0xfd,0x03,0x70,0x66]);

    pub static ref PLUGIN_NS: Uuid = Uuid::new_v5(&*ROOT_NS, b"plugin");

    pub static ref PROCEDURE_NS: Uuid = Uuid::new_v5(&*ROOT_NS, b"procedure");

    pub static ref TYPE_NS: Uuid = Uuid::new_v5(&*ROOT_NS, b"type");
}

/// A trait for rust types that have an associated UUID.
pub trait UniqueType {
    fn type_id() -> Uuid;
}

