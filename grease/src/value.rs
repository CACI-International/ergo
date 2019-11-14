//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use fasthash::{HasherExt, SpookyHasherExt as HasherFn};
use futures::future::{BoxFuture, Future, FutureExt, Shared, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::pin::Pin;
use std::sync::Arc;
use uuid::Uuid;

/// A value type.
///
/// The type is an id and type-specific data.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueType {
    pub id: Uuid,
    pub data: Vec<u8>,
}

impl ValueType {
    /// Create a new ValueType.
    pub fn new(id: Uuid) -> Self {
        Self::with_data(id, vec![])
    }

    pub fn with_data(id: Uuid, data: Vec<u8>) -> Self {
        ValueType { id, data }
    }
}

/// A dependency of a Value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dependency {
    Value(Value),
    Hashed(u128),
}

impl From<&'_ Value> for Dependency {
    fn from(v: &'_ Value) -> Self {
        Dependency::from(v.clone())
    }
}

impl From<Value> for Dependency {
    fn from(v: Value) -> Self {
        Dependency::Value(v)
    }
}

impl<H: Hash> From<&'_ H> for Dependency {
    fn from(v: &'_ H) -> Self {
        let mut hfn = HasherFn::default();
        v.hash(&mut hfn);
        Dependency::Hashed(hfn.finish_ext())
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


/// Types which can be converted into an iterator over dependencies.
///
/// The lifetime parameter is the iterator's lifetime.
pub trait IntoDependencies<'a> {
    type Iter: Iterator<Item = Dependency> + 'a;

    fn into_dependencies(self) -> Self::Iter;
}

impl IntoDependencies<'static> for Dependency {
    type Iter = std::iter::Once<Dependency>;

    fn into_dependencies(self) -> Self::Iter {
        std::iter::once(self)
    }
}

impl<'a, T> IntoDependencies<'a> for T
where
    T: IntoIterator,
    <T as IntoIterator>::Item: Into<Dependency>,
    <T as IntoIterator>::IntoIter: 'a,
{
    type Iter = Box<dyn Iterator<Item = Dependency> + 'a>;

    fn into_dependencies(self) -> Self::Iter {
        Box::new(self.into_iter().map(|d| d.into()))
    }
}

/// Create a Vec<Dependency> from the given dependant values.
///
/// Values are always accessed by shared reference in the normal form.
/// Prepending 'join' to the list will instead call into_dependencies on each argument and join the
/// resulting list.
#[macro_export]
macro_rules! depends {
    ( $( $exp:expr ),* ) => {
        {
            let v: Vec<$crate::Dependency> = vec![$( $crate::Dependency::from(&$exp) ),*];
            v
        }
    };
    ( join $( $exp:expr ),* ) => {
        {
            use $crate::IntoDependencies;
            let v: Vec<$crate::Dependency> = std::iter::empty::<$crate::Dependency>()
                $( .chain($exp.into_dependencies()) )* .collect();
            v
        }
    };
}

/// A value shared amongst tasks.
///
/// Values have a type and a future value.
#[derive(Clone, Debug)]
pub struct Value {
    tp: Arc<ValueType>,
    value: Shared<BoxFuture<'static, Result<Arc<Vec<u8>>, String>>>,
    dependencies: Arc<[Value]>,
    id: u128,
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.id == other.id
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Value) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl Value {
    /// Create a value with the given type, future, and dependencies.
    pub fn new<'a, F, D>(tp: ValueType, value: F, deps: D) -> Value
    where
        F: Future<Output = Result<Vec<u8>, String>> + Send + 'static,
        D: IntoDependencies<'a>,
    {
        let mut hasher = HasherFn::default();
        tp.hash(&mut hasher);
        let depset = BTreeSet::from_iter(deps.into_dependencies());
        depset.hash(&mut hasher);
        Value {
            tp: Arc::new(tp),
            value: value.map_ok(Arc::new).boxed().shared(),
            dependencies: Arc::from_iter(depset.into_iter().filter_map(|v| match v {
                Dependency::Value(val) => Some(val),
                _ => None,
            })),
            id: hasher.finish_ext(),
        }
    }

    /// Create a constant value with the given type and data.
    pub fn constant(tp: ValueType, data: Vec<u8>) -> Value {
        let deps = depends![data];
        Self::new(tp, futures::future::ok(data), deps)
    }

    /// Get the value identifier.
    ///
    /// This identifier is deterministically derived from the value type and dependencies, so is
    /// functionally pure and consistent.
    pub fn get_id(&self) -> u128 {
        self.id
    }

    /// Get the type of the contained value.
    pub fn get_type(&self) -> &ValueType {
        &*self.tp
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called if only one value is needed. Otherwise, joining
    /// values will optimally poll all values at once. This will block the caller until the result
    /// is available.
    pub fn get(&mut self) -> Result<Arc<Vec<u8>>, String> {
        futures::executor::block_on(self)
    }
}

impl Future for Value {
    type Output = Result<Arc<Vec<u8>>, String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(Pin::new(&mut self.get_mut().value), cx)
    }
}
