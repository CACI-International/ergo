//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use fasthash::{HasherExt, SpookyHasherExt as HasherFn};
use futures::future::{BoxFuture, Future, FutureExt, Shared, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};
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

pub trait Dependency {
    fn get_value(&self) -> Option<Value> {
        None
    }
    fn identity(&self) -> u128;
}

impl<T: Hash> Dependency for T {
    fn identity(&self) -> u128 {
        let mut hfn = HasherFn::default();
        self.hash(&mut hfn);
        hfn.finish_ext()
    }
}

impl Dependency for Value {
    fn get_value(&self) -> Option<Value> {
        Some(self.clone())
    }
    fn identity(&self) -> u128 {
        self.id
    }
}

/// Dependencies of a value.
#[derive(Debug, Default)]
pub struct Dependencies {
    values: Vec<Value>,
    dependencies: BTreeSet<u128>,
}

impl Dependencies {
    /// Create a new dependency tracker.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add the given value as a dependency.
    pub fn add<D: Dependency>(mut self, dep: &D) -> Self {
        if let Some(v) = dep.get_value() {
            self.values.push(v);
        }
        self.dependencies.insert(dep.identity());
        self
    }
}

impl Hash for Dependencies {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dependencies.hash(state);
    }
}

/// An intermediate untyped value which can be used to create a Value.
pub struct UntypedValue<F> {
    value: F,
    dependencies: Dependencies,
}

impl<F> UntypedValue<F> {
    /// Create a new untyped value.
    pub fn new(value: F, dependencies: Dependencies) -> Self {
        UntypedValue {
            value,
            dependencies,
        }
    }

    /// Add a dependency to the untyped value.
    pub fn add_dependency<D: Dependency>(mut self, dep: &D) -> Self {
        self.dependencies = self.dependencies.add(dep);
        self
    }
}

impl<F> UntypedValue<F>
where
    F: Future<Output = Result<Vec<u8>, String>> + Send + 'static,
{
    /// Create a value using the given type.
    pub fn with_type(self, tp: ValueType) -> Value {
        Value::new(tp, self.value, self.dependencies)
    }
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

impl Value {
    /// Create a value with the given type, future, and dependencies.
    pub fn new<F>(tp: ValueType, value: F, deps: Dependencies) -> Value
    where
        F: Future<Output = Result<Vec<u8>, String>> + Send + 'static,
    {
        let mut hasher = HasherFn::default();
        tp.hash(&mut hasher);
        deps.hash(&mut hasher);
        Value {
            tp: Arc::new(tp),
            value: value.map_ok(Arc::new).boxed().shared(),
            dependencies: Arc::from(deps.values.as_slice()),
            id: hasher.finish_ext(),
        }
    }

    /// Create a constant value with the given type and data.
    pub fn constant(tp: ValueType, data: Vec<u8>) -> Value {
        let deps = Dependencies::default().add(&data);
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
