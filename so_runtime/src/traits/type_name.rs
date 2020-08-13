//! The TypeName grease trait and helper utilities.

use crate::types;
use abi_stable::{
    std_types::{ROption, RString, RVec},
    StableAbi,
};
use grease::path::PathBuf;
use grease::runtime::Traits;
use grease::traits::*;
use grease::types::{GreaseType, Type};

/// A grease trait with a name for a grease type.
#[derive(Clone, Debug, GreaseTrait, StableAbi)]
#[repr(C)]
pub struct TypeName {
    name: RString,
}

/// The rust trait for the TypeName grease trait on rust types.
pub trait GreaseTypeName {
    /// The grease type name.
    fn grease_type_name() -> String;
}

/// Get a type name for the given type.
pub fn type_name(traits: &Traits, tp: &Type) -> String {
    if let Some(t) = traits.get_type::<TypeName>(tp) {
        t.name.clone().into()
    } else {
        format!("<{}>", tp.id)
    }
}

/// Define the GreaseTypeName trait for the given rust type.
///
/// One must still add the trait implementation to the runtime using `impl_type_name`.
#[macro_export]
macro_rules! grease_type_name {
    ( $t:ty, $n:expr ) => {
        impl $crate::traits::GreaseTypeName for $t {
            fn grease_type_name() -> String {
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
grease_type_name!(RString, "String");
grease_type_name!(PathBuf, "Path");
grease_type_name!(types::Array, "Array");
grease_type_name!(types::Map, "Map");
grease_type_name!(types::Function, "Function");
grease_type_name!(types::Either, "Either");
//TODO grease_type_name!(std::process::ExitStatus, "exit_status");

impl<T: GreaseTypeName> GreaseTypeName for RVec<T> {
    fn grease_type_name() -> String {
        format!("Vec<{}>", T::grease_type_name())
    }
}

pub fn traits(traits: &mut Traits) {
    TypeName::add_impl::<()>(traits);
    TypeName::add_impl::<u8>(traits);
    TypeName::add_impl::<i8>(traits);
    TypeName::add_impl::<u16>(traits);
    TypeName::add_impl::<i16>(traits);
    TypeName::add_impl::<u32>(traits);
    TypeName::add_impl::<i32>(traits);
    TypeName::add_impl::<u64>(traits);
    TypeName::add_impl::<i64>(traits);
    TypeName::add_impl::<usize>(traits);
    TypeName::add_impl::<isize>(traits);
    TypeName::add_impl::<char>(traits);
    TypeName::add_impl::<bool>(traits);
    TypeName::add_impl::<RString>(traits);
    TypeName::add_impl::<PathBuf>(traits);
    TypeName::add_impl::<types::Array>(traits);
    TypeName::add_impl::<types::Function>(traits);
    TypeName::add_impl::<types::Map>(traits);
    TypeName::add_impl::<types::Either>(traits);
    traits.add_generator_by_trait_for_trait(|traits, tp| {
        if tp.id == RVec::<()>::grease_type().id {
            let inner_type = Type::from(tp.data.clone());
            traits
                .get_type::<TypeName>(&inner_type)
                .map(|tn| TypeName {
                    name: format!("Vec<{}>", tn.name()).into(),
                })
                .into()
        } else {
            ROption::RNone
        }
    });
}

impl TypeName {
    /// Get the type name as a str.
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    /// Add the TypeName grease trait implementation for T.
    pub fn add_impl<T: GreaseTypeName + GreaseType>(traits: &mut Traits) {
        traits.add_impl_for_type::<T, TypeName>(TypeName {
            name: T::grease_type_name().into(),
        });
    }
}
