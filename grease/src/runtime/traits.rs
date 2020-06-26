//! Runtime trait tracking.

use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::{CreateTrait, TraitImpl, TraitImplRef, Value, ValueType};

/// A trait generator.
pub type TraitGenerator = fn(Arc<ValueType>) -> Vec<TraitImpl>;

/// Create a function (named `trait_generator` by default) which is a `TraitGenerator` using the
/// given factory function and types.
///
/// The factory function must accept a single type parameter and no arguments.
#[macro_export]
macro_rules! trait_generator {
    ( $name:ident $f:ident ( $( $t:ty ),+ ) ) => {
        pub fn $name(v: std::sync::Arc<$crate::ValueType>) -> Vec<$crate::TraitImpl> {
            $crate::match_value_type!(*v => {
                $( $t => vec![$f::<$t>()] ),+
                => vec![]
            })
        }
    };

    ( $f:ident ( $( $t:ty ),+ ) ) => {
        $crate::trait_generator!(trait_generator $f($($t),+));
    };
}

/// Trait interface.
#[derive(Clone, Debug)]
pub struct Traits {
    inner: Arc<RwLock<Inner>>,
}

/// Trait interface implementation.
#[derive(Debug, Default)]
struct Inner {
    traits: HashMap<Arc<ValueType>, HashSet<Arc<TraitImpl>>>,
    generators: Vec<TraitGenerator>,
}

impl Default for Traits {
    fn default() -> Self {
        Self::new()
    }
}

impl Traits {
    /// Create a new instance.
    pub fn new() -> Self {
        Traits {
            inner: Arc::new(RwLock::new(Inner {
                traits: Default::default(),
                generators: vec![crate::value::trait_generator],
            })),
        }
    }

    /// Add a trait implementor.
    ///
    /// Trait implementors can provide any number of trait implementations for any
    /// number of types.
    pub fn add(&self, gen: TraitGenerator) {
        let mut lock = self.inner.write();
        for (k, v) in &mut lock.traits {
            v.extend(gen(k.clone()).into_iter().map(Arc::new));
        }
        lock.generators.push(gen);
    }

    /// Get a trait for a particular Value's type, if it is implemented.
    pub fn get<T: CreateTrait>(&self, v: &Value) -> Option<T> {
        self.get_type(v.value_type())
    }

    /// Get a trait for a ValueType, if it is implemented.
    pub fn get_type<T: CreateTrait>(&self, tp: Arc<ValueType>) -> Option<T> {
        let lock = self.inner.upgradable_read();

        let lock = if !lock.traits.contains_key(tp.clone().as_ref()) {
            let mut m = HashSet::new();
            for g in &lock.generators {
                m.extend(g(tp.clone()).into_iter().map(Arc::new));
            }
            let mut lock = parking_lot::RwLockUpgradableReadGuard::<Inner>::upgrade(lock);
            lock.traits.insert(tp.clone(), m);
            parking_lot::RwLockWriteGuard::<Inner>::downgrade(lock)
        } else {
            parking_lot::RwLockUpgradableReadGuard::<Inner>::downgrade(lock)
        };

        lock.traits
            .get(tp.as_ref())
            .unwrap()
            .get(&T::trait_type())
            .map(|imp| {
                T::create(self.clone(), tp.clone(), unsafe {
                    TraitImplRef::new(imp.clone())
                })
            })
    }
}
