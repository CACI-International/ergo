//! The values currently being evaluated.
//!
//! This contains unique identities _per Value instance_ derived from the inner Value mutex.

use crate::abi_stable::{
    external_types::RMutex,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, RHashMap, ROption},
    StableAbi,
};

#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Evaluating {
    inner: EvaluatingInterface_TO<'static, RBox<()>>,
}

impl std::fmt::Debug for Evaluating {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl Evaluating {
    pub fn new(enabled: bool) -> Self {
        Evaluating {
            inner: if enabled {
                EvaluatingInterface_TO::from_value(enabled::Evaluating::default(), TU_Opaque)
            } else {
                EvaluatingInterface_TO::from_value(disabled::Evaluating::default(), TU_Opaque)
            },
        }
    }

    /// Consider a new value as being evaluated.
    pub fn push(&mut self, id: usize) {
        self.inner.push(id)
    }

    /// Returns whether a deadlock is detected when locking the given lock.
    pub fn locking_would_deadlock(&self, id: usize) -> bool {
        self.inner.locking_would_deadlock(id)
    }

    /// Consider the current evaluating value to have acquired the given lock.
    pub fn locked(&self, id: usize) {
        self.inner.locked(id)
    }

    /// Consider the current evaluating value to have released the given lock.
    pub fn unlocked(&self, id: usize) {
        self.inner.unlocked(id)
    }

    /// Return whether deadlock detection is enabled;
    pub fn deadlock_detect_enabled(&self) -> bool {
        self.inner.deadlock_detect_enabled()
    }
}

#[sabi_trait]
trait EvaluatingInterface: Clone + Debug + Send + Sync {
    fn push(&mut self, id: usize);

    fn locking_would_deadlock(&self, id: usize) -> bool;

    fn locked(&self, id: usize);

    fn unlocked(&self, id: usize);

    fn deadlock_detect_enabled(&self) -> bool;
}

mod enabled {
    use super::*;

    #[derive(Clone, StableAbi)]
    #[repr(C)]
    pub struct Evaluating {
        current: ROption<usize>,
        locks: RArc<RMutex<RHashMap<usize, RHashMap<usize, usize>>>>,
    }

    impl Default for Evaluating {
        fn default() -> Self {
            Evaluating {
                current: Default::default(),
                locks: RArc::new(RMutex::new(Default::default())),
            }
        }
    }

    impl std::fmt::Debug for Evaluating {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("Evaluating")
                .field("current", &self.current)
                .field("locks", &*self.locks.lock())
                .finish()
        }
    }

    fn has_loop(m: &RHashMap<usize, RHashMap<usize, usize>>) -> bool {
        use std::collections::HashSet;

        struct Visitor<'a> {
            visited: HashSet<usize>,
            m: &'a RHashMap<usize, RHashMap<usize, usize>>,
        }

        impl<'a> Visitor<'a> {
            pub fn visit(&mut self, key: usize) -> bool {
                if !self.visited.insert(key) {
                    return true;
                }
                let ret = if let Some(v) = self.m.get(&key) {
                    v.iter().any(|e| self.visit(*e.0))
                } else {
                    false
                };
                self.visited.remove(&key);
                ret
            }
        }

        let mut visitor = Visitor {
            visited: Default::default(),
            m,
        };

        m.iter().any(|e| visitor.visit(*e.0))
    }

    impl EvaluatingInterface for Evaluating {
        /// Consider a new value as being evaluated.
        fn push(&mut self, id: usize) {
            self.current = ROption::RSome(id);
        }

        /// Returns whether a deadlock is detected when locking the given lock.
        fn locking_would_deadlock(&self, id: usize) -> bool {
            if let ROption::RSome(c_id) = &self.current {
                let mut guard = self.locks.lock();
                *guard.entry(*c_id).or_default().entry(id).or_default() += 1;
                let ret = has_loop(&guard);
                let v = guard.entry(*c_id).or_default().entry(id).or_default();
                *v -= 1;
                let remove = *v == 0;
                drop(v);
                if remove {
                    guard.entry(*c_id).or_default().remove(&id);
                }
                ret
            } else {
                false
            }
        }

        /// Consider the current evaluating value to have acquired the given lock.
        fn locked(&self, id: usize) {
            if let ROption::RSome(c_id) = &self.current {
                let mut guard = self.locks.lock();
                *guard.entry(*c_id).or_default().entry(id).or_default() += 1;
            }
        }

        /// Consider the current evaluating value to have released the given lock.
        fn unlocked(&self, id: usize) {
            if let ROption::RSome(c_id) = &self.current {
                let mut guard = self.locks.lock();
                let v = guard.entry(*c_id).or_default().entry(id).or_default();
                *v -= 1;
                let remove = *v == 0;
                drop(v);
                if remove {
                    guard.entry(*c_id).or_default().remove(&id);
                }
            }
        }

        fn deadlock_detect_enabled(&self) -> bool {
            true
        }
    }
}

mod disabled {
    use super::*;

    #[derive(Default, Clone, Debug)]
    pub struct Evaluating;

    impl EvaluatingInterface for Evaluating {
        fn push(&mut self, _id: usize) {}

        fn locking_would_deadlock(&self, _id: usize) -> bool {
            false
        }

        fn locked(&self, _id: usize) {}

        fn unlocked(&self, _id: usize) {}

        fn deadlock_detect_enabled(&self) -> bool {
            false
        }
    }
}
