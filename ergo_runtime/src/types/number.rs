//! The Number type.

use crate as ergo_runtime;
use crate::abi_stable::{std_types::RVec, type_erase::Erased, StableAbi};
use crate::metadata::Source;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::{depends, Dependencies, TypedValue};
use bincode;
use num::{bigint::BigInt, rational::Ratio, BigRational, FromPrimitive, ToPrimitive};

pub use num::Zero;

/// Script number type.
///
/// Script numbers are rational bigints.
#[derive(Clone, Debug, Default, ErgoType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Number {
    numer: RVec<u32>,
    denom: RVec<u32>,
    neg: bool,
}

crate::HashAsDependency!(Number);

impl From<BigRational> for Number {
    fn from(n: BigRational) -> Self {
        let (numer, denom) = n.into();
        let (n_sign, numer) = numer.into_parts();
        let (d_sign, denom) = denom.into_parts();
        Number {
            numer: numer.iter_u32_digits().collect(),
            denom: denom.iter_u32_digits().collect(),
            neg: n_sign * d_sign == num::bigint::Sign::Minus,
        }
    }
}

impl From<Number> for BigRational {
    fn from(n: Number) -> Self {
        Ratio::new_raw(
            BigInt::new(
                if n.neg {
                    num::bigint::Sign::Minus
                } else {
                    num::bigint::Sign::Plus
                },
                n.numer.to_vec(),
            ),
            BigInt::new(num::bigint::Sign::Plus, n.denom.to_vec()),
        )
    }
}

macro_rules! from_int {
    ( $t:ty, $f:ident ) => {
        pub fn $f(n: $t) -> Self {
            BigRational::$f(n).unwrap().into()
        }
    };
}

macro_rules! to_type {
    ( $t:ty, $f:ident ) => {
        pub fn $f(&self) -> Option<$t> {
            self.num().$f()
        }
    };
}

impl Number {
    /// Get the Number as a BigRational.
    pub fn num(&self) -> BigRational {
        self.clone().into()
    }

    from_int!(i8, from_i8);
    from_int!(u8, from_u8);
    from_int!(i16, from_i16);
    from_int!(u16, from_u16);
    from_int!(i32, from_i32);
    from_int!(u32, from_u32);
    from_int!(i64, from_i64);
    from_int!(u64, from_u64);
    from_int!(i128, from_i128);
    from_int!(u128, from_u128);
    from_int!(isize, from_isize);
    from_int!(usize, from_usize);

    pub fn from_f32(n: f32) -> Option<Self> {
        BigRational::from_f32(n).map(|n| n.into())
    }

    pub fn from_f64(n: f64) -> Option<Self> {
        BigRational::from_f64(n).map(|n| n.into())
    }

    to_type!(i8, to_i8);
    to_type!(u8, to_u8);
    to_type!(i16, to_i16);
    to_type!(u16, to_u16);
    to_type!(i32, to_i32);
    to_type!(u32, to_u32);
    to_type!(i64, to_i64);
    to_type!(u64, to_u64);
    to_type!(i128, to_i128);
    to_type!(u128, to_u128);
    to_type!(isize, to_isize);
    to_type!(usize, to_usize);
    to_type!(f32, to_f32);
    to_type!(f64, to_f64);
}

macro_rules! from_impl {
    ( $t:ty, $f:ident ) => {
        impl From<$t> for Number {
            fn from(n: $t) -> Self {
                Self::$f(n)
            }
        }
    };
}

from_impl!(i8, from_i8);
from_impl!(u8, from_u8);
from_impl!(i16, from_i16);
from_impl!(u16, from_u16);
from_impl!(i32, from_i32);
from_impl!(u32, from_u32);
from_impl!(i64, from_i64);
from_impl!(u64, from_u64);
from_impl!(i128, from_i128);
from_impl!(u128, from_u128);
from_impl!(isize, from_isize);
from_impl!(usize, from_usize);

impl std::convert::TryFrom<f32> for Number {
    type Error = &'static str;
    fn try_from(value: f32) -> Result<Self, Self::Error> {
        Self::from_f32(value).ok_or("irrational float")
    }
}

impl std::convert::TryFrom<f64> for Number {
    type Error = &'static str;
    fn try_from(value: f64) -> Result<Self, Self::Error> {
        Self::from_f64(value).ok_or("irrational float")
    }
}

macro_rules! into_impl {
    ( $t:ty, $f:ident ) => {
        impl std::convert::TryFrom<Number> for $t {
            type Error = &'static str;
            fn try_from(n: Number) -> Result<Self, Self::Error> {
                n.$f().ok_or("numeric overflow")
            }
        }
    };
}

into_impl!(i8, to_i8);
into_impl!(u8, to_u8);
into_impl!(i16, to_i16);
into_impl!(u16, to_u16);
into_impl!(i32, to_i32);
into_impl!(u32, to_u32);
into_impl!(i64, to_i64);
into_impl!(u64, to_u64);
into_impl!(i128, to_i128);
into_impl!(u128, to_u128);
into_impl!(isize, to_isize);
into_impl!(usize, to_usize);
into_impl!(f32, to_f32);
into_impl!(f64, to_f64);

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.num().fmt(f)
    }
}

impl std::str::FromStr for Number {
    type Err = num::rational::ParseRatioError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(d) = s.find('.') {
            // Convert the floating-point into a rational
            let (a, b) = s.split_at(d);
            let b = &b[1..];
            format!("{}{}/1{}", a, b, "0".repeat(b.len())).parse::<BigRational>()
        } else {
            s.parse::<BigRational>()
        }
        .map(|v| v.into())
    }
}

impl From<Number> for TypedValue<Number> {
    fn from(v: Number) -> Self {
        Self::constant(v)
    }
}

impl From<&'_ Number> for Dependencies {
    fn from(n: &'_ Number) -> Self {
        depends![Number::ergo_type(), n]
    }
}

impl From<Number> for super::String {
    fn from(s: Number) -> Self {
        super::String::from(s.to_string())
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.num().cmp(&other.num())
    }
}

ergo_traits_fn! {
    crate::ergo_display_basic!(traits, Number);
    crate::ergo_type_name!(traits, Number);

    impl traits::IntoTyped<Number> for super::String {
        async fn into_typed(self) -> crate::Value {
            self.as_ref().0.as_str().parse::<Number>().map_err(|e| crate::error! {
                labels: [ primary(Source::get(&self).with("while converting this String into a Number")) ],
                notes: [ "valid numbers include rational (`22/7`), integral (`-3`), and floating-point (`3.14`)" ],
                error: e
            }).into()
        }
    }

    traits::IntoTyped::<super::String>::add_impl::<Number>(traits);

    impl traits::Stored for Number {
        async fn put(&self, _stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<()> {
            crate::error_info!(
                labels: [ primary(Source::get(SELF_VALUE).with("while storing this value")) ],
                { bincode::serialize_into(item, &self.num()) }
            ).into()
        }

        async fn get(_stored_ctx: &traits::StoredContext, item: crate::context::ItemContent) -> crate::RResult<Erased> {
            crate::error_info!(
                { bincode::deserialize_from(item).map(|n: BigRational| Erased::new(Number::from(n))) }
            ).into()
        }
    }

    traits::ValueByContent::add_impl::<Number>(traits);
}
