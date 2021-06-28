//! Dependency tracking.

use crate::abi_stable::{bst::BstSet, std_types::RVec, u128::U128, StableAbi};
use crate::hash::HashFn;
use crate::type_system::{Trait, Type};
use crate::value::{TypedValue, Value};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;

/// A dependency of a Value.
///
/// A single dependency is either a hash digest from arbitrary data or a `Value`. The `Value`
/// identifier is used later as the dependency, but the `Value` is stored so that a tree of
/// dependencies may be retrieved. The `Value` is stored as an unevaluated form.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, StableAbi)]
#[repr(u8)]
pub enum Dependency {
    Value(Value),
    Hashed(U128),
}

impl<T: AsDependency> From<T> for Dependency {
    fn from(v: T) -> Self {
        v.as_dependency()
    }
}

/// Get a Dependency from a value.
pub trait AsDependency {
    fn as_dependency(&self) -> Dependency;
}

/// Use Dependency::hashed to implement `AsDependency` for the given type.
#[macro_export]
macro_rules! HashAsDependency {
    ( $t:ty ) => {
        impl $crate::dependency::AsDependency for $t {
            fn as_dependency(&self) -> $crate::dependency::Dependency {
                $crate::dependency::Dependency::hashed(self)
            }
        }
    };
}

HashAsDependency!(crate::abi_stable::uuid::Uuid);
HashAsDependency!(String);
HashAsDependency!(&'_ str);
HashAsDependency!(std::path::PathBuf);
HashAsDependency!(&'_ std::path::Path);
HashAsDependency!(u32);
HashAsDependency!(i32);
HashAsDependency!(u64);
HashAsDependency!(i64);
HashAsDependency!(usize);
HashAsDependency!(isize);
HashAsDependency!(u128);
HashAsDependency!(i128);
HashAsDependency!(Type);
HashAsDependency!(Trait);

impl AsDependency for Value {
    fn as_dependency(&self) -> Dependency {
        let mut v = self.clone();
        v.unevaluated();
        Dependency::Value(v)
    }
}

impl<T> AsDependency for TypedValue<T> {
    fn as_dependency(&self) -> Dependency {
        (**self).as_dependency()
    }
}

impl<T: AsDependency> AsDependency for &'_ T {
    fn as_dependency(&self) -> Dependency {
        (&**self).as_dependency()
    }
}

impl<T: AsDependency> AsDependency for &'_ mut T {
    fn as_dependency(&self) -> Dependency {
        (&**self).as_dependency()
    }
}

impl<T: AsDependency> AsDependency for Option<T> {
    fn as_dependency(&self) -> Dependency {
        match self {
            None => Dependency::Hashed(0.into()),
            Some(v) => v.as_dependency(),
        }
    }
}

impl Hash for Dependency {
    fn hash<H: Hasher>(&self, state: &'_ mut H) {
        match self {
            Dependency::Value(v) => v.id().hash(state),
            Dependency::Hashed(v) => v.hash(state),
        }
    }
}

impl Dependency {
    /// Create a dependency from a Hash value.
    pub fn hashed<T: Hash>(v: &T) -> Self {
        let mut hfn = HashFn::default();
        v.hash(&mut hfn);
        Dependency::Hashed(hfn.finish_ext().into())
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

/// Helper trait for implementations of Into<Dependencies> for references.
pub trait GetDependencies {
    fn get_depends(&self) -> Dependencies;
}

impl<T> GetDependencies for T
where
    for<'a> &'a T: Into<Dependencies>,
{
    fn get_depends(&self) -> Dependencies {
        self.into()
    }
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
/// Values are always accessed by reference in the normal form, and must implement `AsDependency`.
/// Prepending '^' to an item will instead attempt to convert the value to `Dependencies` directly.
/// Prepending '^@' to an item will convert each item in the value to a `Dependency` (by reference using
/// `.iter()`) and add those as ordered dependencies.
/// Curly brackets indicate unordered dependencies.
///
/// Example usage:
/// ```
/// # #[macro_use] extern crate ergo_runtime;
/// // Create ordered dependencies
/// let deps = depends!["a","b"];
/// // Merge dependencies from deps, and add another ordered dependency (after those in deps)
/// let deps2 = depends![^deps,"c"];
/// // Convert each item in vals to `Dependency` and merge them.
/// let vals = vec!["hello", "goodbye"];
/// let deps3 = depends![^deps2, ^@vals];
/// // Add unordered dependencies (1, 2, "3", and 4), and more ordered dependencies.
/// let deps_unordered = depends![{1,2,"3"},^deps3,{4},"d"];
/// ```
#[macro_export]
macro_rules! depends {
    // Unordered items
    // Create unordered dependencies from single items
    ( @item { $( $exp:expr ),* } ) => {
        $crate::dependency::Dependencies::unordered(vec![$( $crate::dependency::Dependency::from(&$exp) ),*])
    };

    // Merge item
    ( @item ^ $exp:expr ) => {
        $crate::dependency::Dependencies::from($exp)
    };

    // Merge and convert item
    ( @item ^ @ $exp:expr ) => {
        $crate::dependency::Dependencies::ordered($exp.iter().map($crate::dependency::Dependency::from))
    };

    // Basic item
    // Add as single (ordered) dependency.
    ( @item $exp:expr ) => {
        $crate::dependency::Dependencies::ordered(vec![$crate::dependency::Dependency::from(&$exp)])
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
            let mut deps = $crate::dependency::Dependencies::new();
            $crate::depends!(@toks expressions { deps } next_item { } tokens $( $tok )*);
            deps
        }
    };
}
