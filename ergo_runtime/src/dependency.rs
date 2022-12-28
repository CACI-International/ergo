//! Dependency tracking.

use crate::abi_stable::{std_types::RVec, u128::U128, StableAbi};
use crate::gc;
use crate::hash::HashFn;
use crate::type_system::{Trait, Type};
use crate::value::{EvaluatedValue, IdentifiedValue, Identity, TypedValue, Value};
use std::hash::Hash;
use std::iter::FromIterator;

/// A dependency of a Value.
///
/// A single dependency is either a hash digest from arbitrary data or a `Value`. The `Value`
/// identifier is used later as the dependency, but the `Value` is stored so that a tree of
/// dependencies may be retrieved.
#[derive(Clone, Debug, StableAbi)]
#[repr(u8)]
pub enum Dependency {
    Value(Value),
    Constant(Constant),
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash, StableAbi)]
#[repr(C)]
pub struct Constant(U128);

impl<T: Hash> From<&'_ T> for Constant {
    fn from(v: &'_ T) -> Self {
        let mut hfn = HashFn::default();
        v.hash(&mut hfn);
        Constant(hfn.finish_ext().into())
    }
}

impl<T: AsDependency> From<T> for Dependency {
    fn from(v: T) -> Self {
        v.as_dependency()
    }
}

impl gc::GcRefs for Constant {
    fn gc_refs(&self, _v: &mut gc::Visitor) {}
}

impl gc::GcRefs for Dependency {
    fn gc_refs(&self, v: &mut gc::Visitor) {
        if let Self::Value(val) = self {
            val.gc_refs(v);
        }
    }
}

/// Get a Dependency from a value.
pub trait AsDependency {
    fn as_dependency(&self) -> Dependency;
}

/// Use Dependency::Constant to implement `AsDependency` for the given type.
#[macro_export]
macro_rules! ConstantDependency {
    ( $t:ty ) => {
        impl $crate::dependency::AsDependency for $t {
            fn as_dependency(&self) -> $crate::dependency::Dependency {
                $crate::dependency::Dependency::Constant(self.into())
            }
        }
    };
}

ConstantDependency!(crate::abi_stable::uuid::Uuid);
ConstantDependency!(String);
ConstantDependency!(&'_ str);
ConstantDependency!(std::path::PathBuf);
ConstantDependency!(&'_ std::path::Path);
ConstantDependency!(bool);
ConstantDependency!(u32);
ConstantDependency!(i32);
ConstantDependency!(u64);
ConstantDependency!(i64);
ConstantDependency!(usize);
ConstantDependency!(isize);
ConstantDependency!(u128);
ConstantDependency!(i128);
ConstantDependency!(Type);
ConstantDependency!(Trait);

impl AsDependency for Value {
    fn as_dependency(&self) -> Dependency {
        Dependency::Value(self.clone())
    }
}

impl AsDependency for IdentifiedValue {
    fn as_dependency(&self) -> Dependency {
        (**self).as_dependency()
    }
}

impl AsDependency for EvaluatedValue {
    fn as_dependency(&self) -> Dependency {
        (**self).as_dependency()
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
            None => Dependency::Constant((&0).into()),
            Some(v) => v.as_dependency(),
        }
    }
}

impl AsDependency for Constant {
    fn as_dependency(&self) -> Dependency {
        Dependency::Constant(self.clone())
    }
}

impl Dependency {
    /// Get the dependency identity.
    pub async fn id(&self) -> Identity {
        match self {
            Dependency::Value(v) => v.clone().eval_id().await,
            Dependency::Constant(v) => Identity::new(v.0.into()),
        }
    }
}

/// A set of dependencies.
///
/// The set tracks unordered and ordered dependencies independently.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Dependencies<Dep = Dependency> {
    // The unordered/ordered distinction is used in `id()`, but otherwise they are identical
    // because we cannot order Dependency alone.
    unordered: RVec<Dep>,
    ordered: RVec<Dep>,
}

pub type DependenciesConstant = Dependencies<Constant>;

impl<D> Default for Dependencies<D> {
    fn default() -> Self {
        Dependencies {
            unordered: Default::default(),
            ordered: Default::default(),
        }
    }
}

impl From<DependenciesConstant> for Dependencies {
    fn from(deps: DependenciesConstant) -> Self {
        Dependencies {
            unordered: deps
                .unordered
                .into_iter()
                .map(Dependency::Constant)
                .collect(),
            ordered: deps.ordered.into_iter().map(Dependency::Constant).collect(),
        }
    }
}

impl<D: gc::GcRefs> gc::GcRefs for Dependencies<D> {
    fn gc_refs(&self, v: &mut gc::Visitor) {
        for d in &self.unordered {
            d.gc_refs(v);
        }
        for d in &self.ordered {
            d.gc_refs(v);
        }
    }
}

/// Helper trait for implementations of Into<Dependencies> for references.
pub trait GetDependencies {
    fn get_depends(&self) -> Dependencies;
}

pub trait GetDependenciesConstant {
    fn get_depends(&self) -> DependenciesConstant;
}

impl<T: GetDependenciesConstant> GetDependencies for T {
    fn get_depends(&self) -> Dependencies {
        GetDependenciesConstant::get_depends(self).into()
    }
}

impl<Dep> Dependencies<Dep> {
    /// Create a new group of dependencies.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new group of unordered dependencies from a dependency source.
    pub fn unordered<I: IntoIterator<Item = Dep>>(deps: I) -> Self {
        Dependencies {
            unordered: RVec::from_iter(deps),
            ordered: Default::default(),
        }
    }

    /// Create a new group of ordered depedencies from a dependency source.
    pub fn ordered<I: IntoIterator<Item = Dep>>(deps: I) -> Self {
        Dependencies {
            unordered: Default::default(),
            ordered: RVec::from_iter(deps),
        }
    }

    /// Map a function over each dependency.
    pub fn map<F: FnMut(&Dep)>(&self, mut f: F) {
        for d in &self.unordered {
            f(d);
        }
        for d in &self.ordered {
            f(d);
        }
    }

    /// Map a function over each dependency.
    pub fn map_mut<F: FnMut(&mut Dep)>(&mut self, mut f: F) {
        for d in &mut self.unordered {
            f(d);
        }
        for d in &mut self.ordered {
            f(d);
        }
    }
}

impl DependenciesConstant {
    pub fn id(&self) -> Identity {
        let mut items = Vec::with_capacity(self.unordered.len() + self.ordered.len());
        for d in self.unordered.iter() {
            items.push(Identity::new(d.0.into()));
        }
        items.sort_unstable();
        for d in self.ordered.iter() {
            items.push(Identity::new(d.0.into()));
        }
        items.iter().sum()
    }
}

impl Dependencies {
    /// Get the identity of the dependencies.
    pub async fn id(&self) -> Identity {
        let mut items = Vec::with_capacity(self.unordered.len() + self.ordered.len());
        let unordered_count = self.unordered.len();
        for d in self.unordered.iter() {
            items.push(d.id());
        }
        for d in self.ordered.iter() {
            items.push(d.id());
        }

        let mut items = futures::future::join_all(items).await;
        items[..unordered_count].sort_unstable();
        items.iter().sum()
    }
}

impl<Dep> std::ops::Add for Dependencies<Dep> {
    type Output = Self;

    /// Combine two Dependencies into one. The order matters with respect
    /// to any ordered dependencies that are stored.
    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl std::ops::Add<Dependencies> for Dependencies<Constant> {
    type Output = Dependencies;

    /// Combine two Dependencies into one. The order matters with respect
    /// to any ordered dependencies that are stored.
    fn add(self, other: Dependencies) -> Self::Output {
        let mut ret = Dependencies::new();
        ret.unordered
            .extend(self.unordered.into_iter().map(|v| v.into()));
        ret.ordered
            .extend(self.ordered.into_iter().map(|v| v.into()));
        ret += other;
        ret
    }
}

impl std::ops::Add<Dependencies<Constant>> for Dependencies {
    type Output = Dependencies;

    /// Combine two Dependencies into one. The order matters with respect
    /// to any ordered dependencies that are stored.
    fn add(mut self, other: Dependencies<Constant>) -> Self::Output {
        self.unordered
            .extend(other.unordered.into_iter().map(|v| v.into()));
        self.ordered
            .extend(other.ordered.into_iter().map(|v| v.into()));
        self
    }
}

impl<Dep> std::ops::AddAssign for Dependencies<Dep> {
    fn add_assign(&mut self, other: Self) {
        self.unordered.extend(other.unordered);
        self.ordered.extend(other.ordered);
    }
}

impl<Dep> std::iter::Sum for Dependencies<Dep> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut deps = Dependencies::new();
        for d in iter {
            deps += d;
        }
        deps
    }
}

impl<T, Dep> From<T> for Dependencies<Dep>
where
    T: IntoIterator,
    <T as IntoIterator>::Item: Into<Dependencies<Dep>>,
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
/// # use ergo_runtime::dependency::Dependencies;
/// // Create ordered dependencies
/// let deps: Dependencies = depends!["a","b"];
/// // Merge dependencies from deps, and add another ordered dependency (after those in deps)
/// let deps2: Dependencies = depends![^deps,"c"];
/// // Convert each item in vals to `Dependency` and merge them.
/// let vals = vec!["hello", "goodbye"];
/// let deps3: Dependencies = depends![^deps2, ^@vals];
/// // Add unordered dependencies (1, 2, "3", and 4), and more ordered dependencies.
/// let deps_unordered: Dependencies = depends![{1,2,"3"},^deps3,{4},"d"];
/// ```
#[macro_export]
macro_rules! depends {
    // Unordered items
    // Create unordered dependencies from single items
    ( @item { $( $exp:expr ),* } ) => {
        $crate::dependency::Dependencies::unordered(vec![$( (&$exp).into() ),*])
    };

    // Merge item
    ( @item ^ $exp:expr ) => {
        $crate::dependency::Dependencies::from($exp)
    };

    // Merge and convert item
    ( @item ^ @ $exp:expr ) => {
        $crate::dependency::Dependencies::ordered($exp.iter().map(|v| v.into()))
    };

    // Basic item
    // Add as single (ordered) dependency.
    ( @item $exp:expr ) => {
        $crate::dependency::Dependencies::ordered(vec![(&$exp).into()])
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

    // Entries
    // Pass tokens into stateful expansion with initial state

    // Force creation of Dependencies<Constant>
    ( const $( $tok:tt )* ) => {
        {
            let mut deps = $crate::dependency::DependenciesConstant::new();
            $crate::depends!(@toks expressions { deps } next_item { } tokens $( $tok )*);
            deps
        }
    };

    // Force creation of Dependencies<Dependency>
    ( dyn $( $tok:tt )* ) => {
        {
            let mut deps = $crate::dependency::Dependencies::<$crate::dependency::Dependency>::new();
            $crate::depends!(@toks expressions { deps } next_item { } tokens $( $tok )*);
            deps
        }
    };

    // Infer Dependencies<>
    ( $( $tok:tt )* ) => {
        {
            let mut deps = $crate::dependency::Dependencies::new();
            $crate::depends!(@toks expressions { deps } next_item { } tokens $( $tok )*);
            deps
        }
    };
}
