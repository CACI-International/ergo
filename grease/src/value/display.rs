//! The Display and DisplayType grease traits.

use super::{Value, ValueData, ValueType};
use crate::runtime::Traits;
use crate::traits::{Trait, TraitImpl, TraitType};
use crate::uuid::*;
use std::fmt;

/// The TypeName grease trait storage struct.
#[derive(Clone)]
pub struct TypeNameTrait {
    name: String,
}

crate::TraitRef! {
    /// The TypeName grease trait reference.
    pub struct TypeName(TypeNameTrait);
}

/// The rust trait for the TypeName grease trait.
pub trait GreaseTypeName {
    fn type_name() -> String;
}

pub fn impl_type_name<T: GreaseTypeName>() -> TraitImpl {
    TraitImpl::for_trait::<TypeName>(TypeNameTrait {
        name: T::type_name(),
    })
}

pub fn type_name(traits: &Traits, tp: &std::sync::Arc<ValueType>) -> String {
    if let Some(t) = traits.get_type::<TypeName>(tp.clone()) {
        t.storage.name.clone()
    } else {
        format!("<{}>", tp.id)
    }
}

/// The Display grease trait storage struct.
#[derive(Clone)]
pub struct DisplayTrait {
    fmt: fn(&Traits, &ValueData, &mut fmt::Formatter) -> fmt::Result,
}

crate::TraitRef! {
    /// The Display grease trait reference.
    pub struct Display(DisplayTrait);
}

/// The rust trait for the Display grease trait.
pub trait GreaseDisplay {
    fn fmt(&self, traits: &Traits, f: &mut fmt::Formatter) -> fmt::Result;
}

pub fn impl_display<T>() -> TraitImpl
where
    T: GreaseDisplay + Sync,
{
    TraitImpl::for_trait::<Display>(DisplayTrait {
        fmt: |t, data, f| unsafe { data.as_ref::<T>() }.fmt(t, f),
    })
}

pub struct Displayed<'t, 'v>(&'t Traits, &'v Value);

impl<'t, 'v> std::fmt::Display for Displayed<'t, 'v> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(t) = self.0.get::<Display>(self.1) {
            t.fmt(self.1.forced_value(), f)
        } else {
            write!(
                f,
                "<cannot display values of type {}>",
                type_name(self.0, &self.1.value_type())
            )
        }
    }
}

/// Return a type that implements the rust trait Display, that will display the given value using
/// the grease trait Display.
///
/// The value (and any internal values) must already be evaluated.
pub fn display<'t, 'v>(traits: &'t Traits, v: &'v crate::Value) -> Displayed<'t, 'v> {
    Displayed(traits, v)
}

/// Like `display`, but check for the grease Display trait immediately and return a suitable error
/// string if it is not found.
pub fn try_display<'t, 'v>(
    traits: &'t Traits,
    v: &'v crate::Value,
) -> Result<Displayed<'t, 'v>, String> {
    if traits.get::<Display>(v).is_some() {
        Ok(Displayed(traits, v))
    } else {
        Err(format!(
            "<cannot display values of type {}>",
            type_name(traits, &v.value_type())
        ))
    }
}

pub(crate) fn trait_generator(v: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    let mut impls = display_trait_generator(v.clone());
    impls.extend(typename_trait_generator(v.clone()));
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

#[macro_export]
macro_rules! grease_display_basic {
    ( $t:ty ) => {
        impl $crate::GreaseDisplay for $t {
            fn fmt(&self, _: &$crate::Traits, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                std::fmt::Display::fmt(self, f)
            }
        }
    };
}

grease_display_basic!(u8);
grease_display_basic!(i8);
grease_display_basic!(u16);
grease_display_basic!(i16);
grease_display_basic!(u32);
grease_display_basic!(i32);
grease_display_basic!(u64);
grease_display_basic!(i64);
grease_display_basic!(u128);
grease_display_basic!(i128);
grease_display_basic!(usize);
grease_display_basic!(isize);
grease_display_basic!(char);
grease_display_basic!(bool);
grease_display_basic!(String);
grease_display_basic!(std::process::ExitStatus);

impl GreaseDisplay for () {
    fn fmt(&self, _: &Traits, _f: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}

impl GreaseDisplay for Vec<u8> {
    fn fmt(&self, _: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        match std::str::from_utf8(self) {
            Ok(s) => f.write_str(s),
            Err(_) => write!(f, "[bytes]"),
        }
    }
}

impl GreaseDisplay for std::path::PathBuf {
    fn fmt(&self, _: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}

crate::trait_generator!(
    typename_trait_generator
    impl_type_name((), u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, char, bool,
        Vec<u8>, String, std::path::PathBuf, std::process::ExitStatus)
);

crate::trait_generator!(
    display_trait_generator
    impl_display((), u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, char, bool,
        Vec<u8>, String, std::path::PathBuf, std::process::ExitStatus)
);

impl TypeNameTrait {
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Trait for TypeNameTrait {
    fn trait_type() -> TraitType {
        TraitType::new(type_uuid(b"grease::TypeName"))
    }
}

impl DisplayTrait {
    pub fn fmt(&self, traits: &Traits, data: &ValueData, f: &mut fmt::Formatter) -> fmt::Result {
        (self.fmt)(traits, data, f)
    }
}

impl Display {
    pub fn fmt(&self, data: &ValueData, f: &mut fmt::Formatter) -> fmt::Result {
        self.storage.fmt(&self.traits, data, f)
    }
}

impl Trait for DisplayTrait {
    fn trait_type() -> TraitType {
        TraitType::new(type_uuid(b"grease::Display"))
    }
}
