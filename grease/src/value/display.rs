//! The Display and DisplayType grease traits.

use super::{GetValueType, ValueData, ValueType};
use crate::traits::{Trait, TraitImpl, TraitImplRef, TraitRef, TraitType};
use crate::uuid::*;
use std::fmt;

/// The TypeName grease trait storage struct.
#[derive(Clone)]
pub struct TypeNameTrait {
    name: String,
}

/// The TypeName grease trait reference.
pub type TypeName = TraitRef<TypeNameTrait>;

/// The rust trait for the TypeName grease trait.
pub trait GreaseTypeName {
    fn type_name() -> String;
}

pub fn impl_type_name<T: GreaseTypeName>() -> TraitImpl {
    TraitImpl::for_trait::<TypeName>(TypeNameTrait {
        name: T::type_name(),
    })
}

/// The Display grease trait storage struct.
#[derive(Clone)]
pub struct DisplayTrait {
    pub fmt: fn(&ValueData, &mut fmt::Formatter) -> fmt::Result,
}

/// The Display grease trait reference.
pub type Display = TraitRef<DisplayTrait>;

pub fn impl_display<T>() -> TraitImpl
where
    T: std::fmt::Display + Sync,
{
    TraitImpl::for_trait::<Display>(DisplayTrait {
        fmt: |data: &ValueData, f: &mut fmt::Formatter| {
            write!(f, "{}", unsafe { data.as_ref::<T>() })
        },
    })
}

pub(crate) fn trait_generator(v: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    let mut impls = display_trait_generator(v.clone());
    impls.extend(typename_trait_generator(v.clone()));
    impls.extend(crate::match_value_type!(*v => {
        () => vec![TraitImpl::for_trait::<Display>(DisplayTrait {
            fmt: |_, _| Ok(())
        })],
        std::path::PathBuf => vec![TraitImpl::for_trait::<Display>(DisplayTrait {
            fmt: |d, f| write!(f, "{}", unsafe{ d.as_ref::<std::path::PathBuf>() }.display())
        })],
        Vec<u8> => vec![TraitImpl::for_trait::<Display>(DisplayTrait {
            fmt: |d, f| {
                let v = unsafe { d.as_ref::<Vec<u8>>() };
                match std::str::from_utf8(v) {
                    Ok(s) => f.write_str(s),
                    Err(_) => write!(f, "[bytes]")
                }
            }
        })]
        => vec![]
    }));
    impls
}

#[macro_export]
macro_rules! grease_type_name {
    ( $t:ty, $n:expr ) => {
        impl $crate::GreaseTypeName for $t {
            fn type_name() -> String {
                $n.into()
            }
        }
    };
    ( $t:ty ) => {
        $crate::grease_type_name!($t, stringify!($t));
    };
}

grease_type_name!((), "unit");
grease_type_name!(u8);
grease_type_name!(i8);
grease_type_name!(u16);
grease_type_name!(i16);
grease_type_name!(u32);
grease_type_name!(i32);
grease_type_name!(u64);
grease_type_name!(i64);
grease_type_name!(u128);
grease_type_name!(i128);
grease_type_name!(usize);
grease_type_name!(isize);
grease_type_name!(char);
grease_type_name!(bool);
grease_type_name!(Vec<u8>, "bytes");
grease_type_name!(String, "string");
grease_type_name!(std::path::PathBuf, "path");
grease_type_name!(std::process::ExitStatus, "exit_status");

crate::trait_generator!(
    typename_trait_generator
    impl_type_name((), u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, char, bool,
        Vec<u8>, String, std::path::PathBuf, std::process::ExitStatus)
);

crate::trait_generator!(
    display_trait_generator
    impl_display(u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, char, bool, String, std::process::ExitStatus)
);

impl TypeNameTrait {
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Trait for TypeNameTrait {
    type Impl = Self;

    fn trait_type() -> TraitType {
        TraitType::new(type_uuid(b"grease::TypeName"))
    }

    fn create(imp: TraitImplRef<Self::Impl>) -> Self {
        imp.clone()
    }
}

impl DisplayTrait {
    pub fn fmt(&self, data: &ValueData, f: &mut fmt::Formatter) -> fmt::Result {
        (self.fmt)(data, f)
    }
}

impl Trait for DisplayTrait {
    type Impl = DisplayTrait;

    fn trait_type() -> TraitType {
        TraitType::new(type_uuid(b"grease::Display"))
    }

    fn create(imp: TraitImplRef<Self::Impl>) -> Self {
        imp.clone()
    }
}
