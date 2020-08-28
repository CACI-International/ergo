//! The Nested grease trait.
//!
//! This trait is used to expose nested Values within Value types.

use crate::types;
use abi_stable::{std_types::ROption, StableAbi};
use futures::future::{FutureExt, TryFutureExt};
use futures::stream::{futures_unordered::FuturesUnordered, TryStreamExt};
use grease::{
    bst::BstSet, runtime::Traits, traits::GreaseTrait, type_erase::Erased, types::GreaseType,
    value::Value,
};
use std::collections::BTreeSet;

/// A grease trait which exposes nested Values within a single Value's data.
#[derive(Clone, GreaseTrait, StableAbi)]
#[repr(C)]
pub struct Nested {
    nested: extern "C" fn(&Erased) -> BstSet<Value>,
}

impl Nested {
    pub fn nested(&self, data: &Erased) -> BstSet<Value> {
        (self.nested)(data)
    }

    /// Add an implementation of the trait for values with type T.
    pub fn add_impl<T: GreaseType + GreaseNested + Sync>(traits: &mut Traits) {
        extern "C" fn nested<T: GreaseNested>(data: &Erased) -> BstSet<Value> {
            unsafe { data.as_ref::<T>() }.nested().into()
        }

        traits.add_impl_for_type::<T, Nested>(Nested {
            nested: nested::<T>,
        });
    }
}

/// Describe the nested Values within a type.
pub trait GreaseNested {
    fn nested(&self) -> BTreeSet<Value>;
}

impl GreaseNested for types::Array {
    fn nested(&self) -> BTreeSet<Value> {
        self.0.iter().cloned().collect()
    }
}

impl GreaseNested for types::Map {
    fn nested(&self) -> BTreeSet<Value> {
        self.0.iter().map(|t| t.1).cloned().collect()
    }
}

pub fn traits(traits: &mut Traits) {
    Nested::add_impl::<types::Array>(traits);
    Nested::add_impl::<types::Map>(traits);

    // types::Either
    {
        extern "C" fn nested(data: &Erased) -> BstSet<Value> {
            let either = unsafe { data.as_ref::<types::Either>() };
            vec![either.value()].into_iter().collect()
        }

        traits.add_generator_by_trait_for_trait(|_traits, tp| {
            if !types::Either::matches_grease_type(tp) {
                return ROption::RNone;
            }

            ROption::RSome(Nested { nested })
        });
    }
}

macro_rules! add_value {
    ( $vals:expr, $traits:expr, $v:ident) => {{
        let nested = $traits.get::<Nested>(&$v);
        $vals.push(
            $v.map_ok(move |data| match nested {
                Some(n) => n.nested(&data),
                None => Default::default(),
            })
            .boxed(),
        );
    }};
}

/// Force all Values, and all recursive Values from the Nested grease trait.
pub async fn force_value_nested(traits: &Traits, v: Value) -> Result<(), grease::value::Error> {
    let mut values = FuturesUnordered::new();
    add_value!(values, traits, v);
    while let Some(vals) = values.try_next().await? {
        for v in vals {
            add_value!(values, traits, v);
        }
    }
    Ok(())
}
