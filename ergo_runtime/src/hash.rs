//! Hashing used within the ergo runtime.
//!
//! The implementation uses a FNV-1a 128-bit Hasher.

use std::hash::Hasher;
use std::io::{BufRead, BufReader, Read};

/// The hash function, supporting `Default` and `Hasher`.
pub type HashFn = Fnv1a;

/// Hash the contents of a type implementing Read.
pub fn hash_read<R: Read>(read: R) -> std::io::Result<u128> {
    let mut hfn = HashFn::default();
    let mut br = BufReader::new(read);
    loop {
        let slice = br.fill_buf()?;
        if slice.len() == 0 {
            break;
        }
        hfn.write(slice);
        let len = slice.len();
        br.consume(len);
    }
    Ok(hfn.finish_ext())
}

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
    /// Get a 64-bit hash digest.
    pub fn finish(&self) -> u64 {
        self.state as u64
    }

    /// Get a 128-bit hash digest.
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
