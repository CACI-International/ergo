//! The nested grease trait.
//!
//! This trait is used to expose nested Values within Value types.

use crate::script::runtime::script_types::{ScriptArray, ScriptMap};
use futures::prelude::*;
use futures::stream::futures_unordered::FuturesUnordered;
use grease::{trait_generator, Trait, TraitImpl, TraitRef, Value, ValueData};
use std::collections::BTreeSet;

/// The nested grease trait storage struct.
#[derive(Clone, Trait)]
pub struct NestedTrait {
    nested: fn(&ValueData) -> BTreeSet<Value>,
}

impl NestedTrait {
    pub fn nested(&self, data: &ValueData) -> BTreeSet<Value> {
        (self.nested)(data)
    }
}

TraitRef! {
    /// The nested grease trait reference.
    pub struct Nested(NestedTrait);
}

/// Describe nested Values within a type.
pub trait NestedValues {
    fn nested(&self) -> BTreeSet<Value>;
}

fn nested<T: NestedValues + Sync>(v: &ValueData) -> BTreeSet<Value> {
    unsafe { v.as_ref::<T>() }.nested()
}

/// Implement NestedTrait for the given type.
pub fn impl_nested<T: NestedValues + Sync>() -> TraitImpl {
    TraitImpl::for_trait::<Nested>(NestedTrait {
        nested: nested::<T>,
    })
}

impl NestedValues for ScriptArray {
    fn nested(&self) -> BTreeSet<Value> {
        self.0.iter().cloned().collect()
    }
}

impl NestedValues for ScriptMap {
    fn nested(&self) -> BTreeSet<Value> {
        self.0.values().cloned().collect()
    }
}

trait_generator!(impl_nested(ScriptArray, ScriptMap));

macro_rules! add_value {
    ( $vals:expr, $traits:expr, $v:ident) => {{
        let nested = $traits.get::<Nested>(&$v);
        $vals.push(
            $v.map_ok(move |data| match nested {
                Some(n) => n.storage.nested(&data),
                None => Default::default(),
            })
            .boxed(),
        );
    }};
}

/// Force all Values, and all recursive Values from the Nested grease trait.
pub async fn force_value_nested(traits: &grease::Traits, v: Value) -> Result<(), grease::Error> {
    let mut values = FuturesUnordered::new();
    add_value!(values, traits, v);
    while let Some(vals) = values.try_next().await? {
        for v in vals {
            add_value!(values, traits, v);
        }
    }
    Ok(())
}
