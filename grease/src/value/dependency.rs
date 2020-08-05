//! Dependency tracking.

use super::{TypedValue, Value};
use crate::bst::BstSet;
use crate::hash::HashFn;
use crate::u128::U128;
use abi_stable::{std_types::RVec, StableAbi};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;

/// A dependency of a Value.
///
/// A single dependency is either a hash digest from arbitrary data or a grease `Value`. The
/// `Value` identifier is used later as the dependency, but the `Value` is stored so that a tree of
/// dependencies may be retrieved. The `Value` is stored as an unevaluated form.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, StableAbi)]
#[repr(u8)]
pub enum Dependency {
    Value(Value),
    Hashed(U128),
}

impl From<&'_ Value> for Dependency {
    fn from(v: &'_ Value) -> Self {
        Dependency::from(v.clone())
    }
}

impl From<Value> for Dependency {
    fn from(v: Value) -> Self {
        Dependency::Value(v.unevaluated())
    }
}

impl<T> From<&'_ TypedValue<T>> for Dependency {
    fn from(v: &'_ TypedValue<T>) -> Self {
        Dependency::from(&v.inner)
    }
}

impl<T> From<TypedValue<T>> for Dependency {
    fn from(v: TypedValue<T>) -> Self {
        Dependency::from(v.inner)
    }
}

impl<H: Hash> From<&'_ H> for Dependency {
    fn from(v: &'_ H) -> Self {
        let mut hfn = HashFn::default();
        v.hash(&mut hfn);
        Dependency::Hashed(hfn.finish_ext().into())
    }
}

impl Hash for Dependency {
    fn hash<H: Hasher>(&self, state: &'_ mut H) {
        match self {
            Dependency::Value(v) => v.id.hash(state),
            Dependency::Hashed(v) => v.hash(state),
        }
    }
}

/// A set of dependencies.
///
/// The set tracks unordered and ordered dependencies independently.
#[derive(Clone, Debug, Default, Hash, StableAbi)]
#[repr(C)]
pub struct Dependencies {
    unordered: BstSet<Dependency>,
    ordered: RVec<Dependency>,
}

impl Dependencies {
    /// Create a new group of dependencies.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new group of unordered dependencies from a dependency source.
    pub fn unordered<I: IntoIterator<Item = Dependency>>(deps: I) -> Self {
        Dependencies {
            unordered: BstSet::from_iter(deps),
            ordered: Default::default(),
        }
    }

    /// Create a new group of ordered depedencies from a dependency source.
    pub fn ordered<I: IntoIterator<Item = Dependency>>(deps: I) -> Self {
        Dependencies {
            unordered: Default::default(),
            ordered: RVec::from_iter(deps),
        }
    }

    /// Return an iterator over the dependencies.
    ///
    /// Unordered dependencies precede ordered ones.
    pub fn iter<'a>(&'a self) -> Iter<'a> {
        Iter(Box::new(self.unordered.iter().chain(self.ordered.iter())))
    }
}

impl std::ops::Add for Dependencies {
    type Output = Self;

    /// Combine two Dependencies into one. The order matters with respect
    /// to any ordered dependencies that are stored.
    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl std::ops::AddAssign for Dependencies {
    fn add_assign(&mut self, other: Self) {
        self.unordered.extend(other.unordered);
        self.ordered.extend(other.ordered);
    }
}

impl std::iter::Sum for Dependencies {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut deps = Dependencies::new();
        for d in iter {
            deps += d;
        }
        deps
    }
}

pub struct Iter<'a>(Box<dyn Iterator<Item = &'a Dependency> + 'a>);

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Dependency;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T> From<T> for Dependencies
where
    T: IntoIterator,
    <T as IntoIterator>::Item: Into<Dependencies>,
{
    fn from(v: T) -> Self {
        use std::iter::Sum;
        Dependencies::sum(v.into_iter().map(|v| v.into()))
    }
}

/// Create Dependencies from the given values.
///
/// Values are always accessed by reference in the normal form.
/// Prepending '^' to an item will instead attempt to convert the value to `Dependencies` directly.
///
/// Example usage:
/// ```
/// # #[macro_use] extern crate grease;
/// // Create ordered dependencies
/// let deps = depends!["a","b"];
/// // Merge dependencies from deps, and add another ordered dependency (after those in deps)
/// let deps2 = depends![^deps,"c"];
/// // Add unordered dependencies (1, 2, "3", and 4), and more ordered dependencies.
/// let deps_unordered = depends![{1,2,"3"},^deps2,{4},"d"];
/// ```
#[macro_export]
macro_rules! depends {
    // Unordered items
    // Create unordered dependencies from single items
    ( @item { $( $exp:expr ),* } ) => {
        $crate::value::Dependencies::unordered(vec![$( $crate::value::Dependency::from(&$exp) ),*])
    };

    // Merge item
    ( @item ^ $exp:expr ) => {
        $crate::value::Dependencies::from($exp)
    };

    // Basic item
    // Add as single (ordered) dependency.
    ( @item $exp:expr ) => {
        $crate::value::Dependencies::ordered(vec![$crate::value::Dependency::from(&$exp)])
    };

    // End of items and input tokens
    // Expand to expressions
    ( @toks expressions { $e:ident $({ $( $tok:tt )* })* } next_item { } tokens ) => {
        $(
            $e += $( $tok )*;
        )*
    };

    // End of input tokens
    // Process final item
    ( @toks expressions { $e:ident $( $res:tt )* } next_item { $( $tok:tt )+ } tokens ) => {
        $crate::depends!(@toks expressions { $e $( $res )* { $crate::depends!(@item $( $tok )*) } } next_item { } tokens)
    };

    // Comma separator
    // Superfluous comma
    ( @toks expressions { $e:ident $( $res:tt )* } next_item { } tokens , $( $rest:tt )* ) => {
        $crate::depends!(@toks expressions { $e $( $res )* } next_item { } tokens $( $rest )*)
    };
    // Process stored tokens as an item
    ( @toks expressions { $e:ident $( $res:tt )* } next_item { $( $tok:tt )+ } tokens , $( $rest:tt )* ) => {
        $crate::depends!(@toks expressions { $e $( $res )* { $crate::depends!(@item $( $tok )*) } } next_item { } tokens $( $rest )*)
    };

    // Token (non-comma)
    // Shift token into stored tokens
    ( @toks expressions { $e:ident $( $res:tt )* } next_item { $( $tok:tt )* } tokens $next:tt $( $rest:tt )* ) => {
        $crate::depends!(@toks expressions { $e $( $res )* } next_item { $( $tok )* $next } tokens $( $rest )*)
    };

    // Entry
    // Pass tokens into stateful expansion with initial state
    ( $( $tok:tt )* ) => {
        {
            let mut deps = $crate::value::Dependencies::new();
            $crate::depends!(@toks expressions { deps } next_item { } tokens $( $tok )*);
            deps
        }
    };
}
