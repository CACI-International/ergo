//! ABI-stable u128.

use abi_stable::StableAbi;

/// A u128 wrapper.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, StableAbi)]
#[repr(C, align(16))]
pub struct U128([u64; 2]);

impl U128 {
    /// Create a new U128.
    pub fn new(v: u128) -> Self {
        U128(unsafe { std::mem::transmute(v) })
    }

    /// Get the numeric value.
    pub fn value(&self) -> u128 {
        unsafe { std::mem::transmute(self.clone().0) }
    }
}

impl std::fmt::Display for U128 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl From<u128> for U128 {
    fn from(v: u128) -> Self {
        Self::new(v)
    }
}

impl From<U128> for u128 {
    fn from(v: U128) -> Self {
        v.value()
    }
}

impl std::ops::Deref for U128 {
    type Target = u128;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute(&self.0) }
    }
}

impl std::ops::DerefMut for U128 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::mem::transmute(&mut self.0) }
    }
}

impl AsRef<u128> for U128 {
    fn as_ref(&self) -> &u128 {
        &**self
    }
}

impl AsMut<u128> for U128 {
    fn as_mut(&mut self) -> &mut u128 {
        &mut **self
    }
}

impl std::hash::Hash for U128 {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        h.write_u128(self.value())
    }
}
