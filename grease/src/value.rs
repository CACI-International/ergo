//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use futures::future::{BoxFuture, Future, FutureExt, Shared, TryFutureExt};
use futures::task::{Context, Poll};
use std::pin::Pin;
use std::sync::Arc;
use uuid::Uuid;

/// A value type.
///
/// The type is an id and type-specific data.
#[derive(Debug, PartialEq, Eq)]
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

/// A value shared amongst tasks.
///
/// Values have a type and a future value.
#[derive(Clone, Debug)]
pub struct Value {
    tp: Arc<ValueType>,
    value: Shared<BoxFuture<'static, Result<Arc<Vec<u8>>, String>>>,
    dependencies: Arc<[Value]>,
}

impl Value {
    /// Create a value with the given type and future.
    pub fn new<F>(tp: ValueType, value: F, deps: &[Value]) -> Value
    where
        F: Future<Output = Result<Vec<u8>, String>> + Send + 'static,
    {
        Value {
            tp: Arc::new(tp),
            value: value.map_ok(Arc::new).boxed().shared(),
            dependencies: Arc::from(deps),
        }
    }

    /// Create a constant value with the given type and data.
    pub fn constant(tp: ValueType, data: Vec<u8>) -> Value {
        Self::new(tp, futures::future::ok(data), &[])
    }

    /// Create a value from a future.
    pub fn future<F>(tp: ValueType, value: F) -> Value
    where
        F: Future<Output = Result<Vec<u8>, String>> + Send + 'static,
    {
        Self::new(tp, value, &[])
    }

    /// Get the type of the contained value.
    pub fn get_type(&self) -> &ValueType {
        &*self.tp
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called if only one value is needed. Otherwise, joining
    /// values will optimally poll all values at once.
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

#[macro_export]
macro_rules! derived_value {
    ( $tp:expr , ( $( $name:ident = $val:expr ),* ) $body:expr ) => {
        {
            $( let $name = $val.clone(); )*
            $crate::Value::new($tp, $body, &[$($val.clone()),*])
        }
    }
}
