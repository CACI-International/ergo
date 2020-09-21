//! The Display grease trait and helper utilities.

use super::type_name;
use crate::types;
use abi_stable::{
    std_types::{ROption, RString},
    StableAbi,
};
use grease::path::PathBuf;
use grease::runtime::Traits;
use grease::traits::*;
use grease::type_erase::Erased;
use grease::types::GreaseType;
use grease::value::Value;
use std::fmt;

/// The Display grease trait.
#[derive(Clone, GreaseTrait, StableAbi)]
#[repr(C)]
pub struct Display {
    fmt: extern "C" fn(&Traits, &Erased) -> RString,
}

impl Display {
    /// Call format for the Value data.
    pub fn fmt(&self, traits: &Traits, data: &Erased) -> String {
        (self.fmt)(traits, data).into()
    }

    /// Implement the display grease trait for the given type.
    pub fn add_impl<T: GreaseDisplay + GreaseType + Sync>(traits: &mut Traits) {
        extern "C" fn fmt<T: GreaseDisplay>(t: &Traits, data: &Erased) -> RString {
            unsafe { data.as_ref::<T>() }.fmt(t).into()
        }

        traits.add_impl_for_type::<T, Display>(Display { fmt: fmt::<T> });
    }
}

/// The rust trait for the Display grease trait.
pub trait GreaseDisplay {
    fn fmt(&self, traits: &Traits) -> String;
}

/// A `std::fmt::Display` proxy.
pub struct Displayed<'t, 'v>(&'t Traits, &'v Value);

impl<'t, 'v> std::fmt::Display for Displayed<'t, 'v> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(t) = self.0.get::<Display>(self.1) {
            write!(f, "{}", t.fmt(&self.0, self.1.forced_value().as_ref()))
        } else {
            write!(
                f,
                "<cannot display values of type {}>",
                type_name(&self.0, &self.1.grease_type())
            )
        }
    }
}

/// Return a type that implements `std::fmt::Display`, that will display the given value using the
/// grease trait Display.
///
/// The value (and any internal values) must already be evaluated.
pub fn display<'t, 'v>(traits: &'t Traits, v: &'v Value) -> Displayed<'t, 'v> {
    Displayed(traits, v)
}

/// Like `display`, but check for the grease Display trait immediately and return a suitable error
/// string if it is not found.
pub fn try_display<'t, 'v>(traits: &'t Traits, v: &'v Value) -> Result<Displayed<'t, 'v>, String> {
    if traits.get::<Display>(v).is_some() {
        Ok(Displayed(traits, v))
    } else {
        Err(format!(
            "<cannot display values of type {}>",
            type_name(&traits, &v.grease_type())
        ))
    }
}

#[macro_export]
macro_rules! grease_display_basic {
    ( $t:ty ) => {
        impl $crate::traits::GreaseDisplay for $t {
            fn fmt(&self, _: &grease::runtime::Traits) -> String {
                format!("{}", self)
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
grease_display_basic!(usize);
grease_display_basic!(isize);
grease_display_basic!(char);
grease_display_basic!(bool);
grease_display_basic!(RString);
//TODO grease_display_basic!(std::process::ExitStatus);

impl GreaseDisplay for () {
    fn fmt(&self, _: &Traits) -> String {
        Default::default()
    }
}

impl GreaseDisplay for PathBuf {
    fn fmt(&self, _: &Traits) -> String {
        format!("{}", self.as_ref().display())
    }
}

struct Empty<'a, 'b>(Displayed<'a, 'b>);

impl<'a, 'b> fmt::Display for Empty<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self.0.to_string();
        if s.is_empty() {
            write!(f, "(empty)")
        } else {
            write!(f, "{}", s)
        }
    }
}

impl GreaseDisplay for types::Array {
    fn fmt(&self, traits: &Traits) -> String {
        self.0
            .iter()
            .map(|v| format!("{}", Empty(display(traits, v)),))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl GreaseDisplay for types::Map {
    fn fmt(&self, traits: &Traits) -> String {
        self.0
            .iter()
            .map(|(k, v)| format!("{}:\n{}", k, Empty(display(traits, v))))
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

impl GreaseDisplay for types::Either {
    fn fmt(&self, traits: &Traits) -> String {
        format!(
            "{}",
            display(traits, &self.value())
        )
    }
}

pub fn traits(traits: &mut Traits) {
    Display::add_impl::<()>(traits);
    Display::add_impl::<u8>(traits);
    Display::add_impl::<i8>(traits);
    Display::add_impl::<u16>(traits);
    Display::add_impl::<i16>(traits);
    Display::add_impl::<u32>(traits);
    Display::add_impl::<i32>(traits);
    Display::add_impl::<u64>(traits);
    Display::add_impl::<i64>(traits);
    Display::add_impl::<usize>(traits);
    Display::add_impl::<isize>(traits);
    Display::add_impl::<char>(traits);
    Display::add_impl::<bool>(traits);
    Display::add_impl::<RString>(traits);
    Display::add_impl::<PathBuf>(traits);
    Display::add_impl::<types::Array>(traits);
    Display::add_impl::<types::Map>(traits);

    // types::Either
    {
        extern "C" fn fmt(t: &Traits, data: &Erased) -> RString {
            let either = unsafe { data.as_ref::<types::Either>() };
            format!(
                "either({}): {}",
                either.index(),
                display(t, &either.value())
            )
            .into()
        }

        traits.add_generator_by_trait_for_trait(|_traits, tp| {
            if !types::Either::matches_grease_type(tp) {
                return ROption::RNone;
            }

            ROption::RSome(Display { fmt })
        });
    }
}
