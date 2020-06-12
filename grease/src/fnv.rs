//! FNV-1a 128-bit Hasher.

use std::hash::Hasher;

const FNV_OFFSET_BASIS: u128 = 0x6c62272e07bb014262b821756295c58du128;
const FNV_PRIME: u128 = 0x0000000001000000000000000000013Bu128;

pub struct Fnv1a {
    state: u128,
}

impl Default for Fnv1a {
    fn default() -> Self {
        Fnv1a {
            state: FNV_OFFSET_BASIS,
        }
    }
}

impl Fnv1a {
    pub fn finish_ext(&self) -> u128 {
        self.state
    }
}

impl Hasher for Fnv1a {
    fn write(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.state ^= u128::from(*byte);
            self.state = self.state.wrapping_mul(FNV_PRIME);
        }
    }

    fn finish(&self) -> u64 {
        self.state as u64
    }
}
