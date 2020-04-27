//! Runtime trait tracking.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::{Trait, TraitImpl, Value, ValueType};

/// A trait generator.
pub type TraitGenerator = fn(Arc<ValueType>) -> Vec<TraitImpl>;

/// Trait interface.
#[derive(Debug, Default)]
pub struct Traits {
    traits: HashMap<Arc<ValueType>, HashSet<TraitImpl>>,
    generators: Vec<TraitGenerator>,
}

impl Traits {
    /// Create a new instance.
    pub fn new() -> Self {
        Traits {
            traits: Default::default(),
            generators: vec![crate::value::trait_generator],
        }
    }

    /// Add a trait implementor.
    ///
    /// Trait implementors can provide any number of trait implementations for any
    /// number of types.
    pub fn add(&mut self, gen: TraitGenerator) {
        for (k, v) in &mut self.traits {
            v.extend(gen(k.clone()));
        }
        self.generators.push(gen);
    }

    /// Get a trait for a particular Value's type, if it is implemented.
    pub fn get<'a, T: 'a + Trait<'a> + Sync>(&'a mut self, v: &Value) -> Option<T> {
        let tp = v.value_type();
        if !self.traits.contains_key(tp.as_ref()) {
            let mut m = HashSet::new();
            for g in &self.generators {
                m.extend(g(v.value_type()));
            }
            self.traits.insert(tp, m);
        }
        self.traits
            .get(v.value_type().as_ref())
            .unwrap()
            .get(&T::trait_type())
            .map(|imp| T::create(unsafe { imp.as_ref() }))
    }
}
