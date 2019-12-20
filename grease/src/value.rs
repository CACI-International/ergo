//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use fasthash::{HasherExt, SpookyHasherExt as HasherFn};
use futures::future::{BoxFuture, Future, FutureExt, Shared, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};
use std::io::{BufRead, BufReader, Read};
use std::iter::FromIterator;
use std::pin::Pin;
use std::sync::Arc;
use uuid::Uuid;

mod types;

pub use types::*;

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

/// A type that has an associated ValueType.
pub trait GetValueType {
    fn value_type() -> ValueType;
}

/// Hash the contents of a type implementing Read.
pub fn hash_read<R: Read>(read: R) -> std::io::Result<u128> {
    let mut hfn = HasherFn::default();
    let mut br = BufReader::new(read);
    loop {
        let slice = br.fill_buf()?;
        if slice.len() == 0 {
            break;
        }
        hfn.write(slice);
        let len = slice.len();
        br.consume(len);
    }
    Ok(hfn.finish_ext())
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
    ( join $( $exp:expr ),* ) => {
        {
            use $crate::IntoDependencies;
            let v: Vec<$crate::Dependency> = std::iter::empty::<$crate::Dependency>()
                $( .chain($exp.into_dependencies()) )* .collect();
            v
        }
    };
    ( $( $exp:expr ),* ) => {
        {
            let v: Vec<$crate::Dependency> = vec![$( $crate::Dependency::from(&$exp) ),*];
            v
        }
    };
}

/// Value data with a stored drop function.
///
/// The drop function is called when the ValueData instance itself is dropped.
pub struct ValueData(*mut (), extern "C" fn(*mut ()));

// ValueData should always be Send, and should be Sync if the value it stores is Sync. However, we
// cannot enforce the Sync restriction, so we instead ensure values are Sync in the Alias struct,
// which is used to access ValueData.
unsafe impl Send for ValueData {}
unsafe impl Sync for ValueData {}

extern "C" fn no_drop(_v: *mut ()) {}

extern "C" fn drop_box<T>(ptr: *mut ()) {
    unsafe { drop(Box::from_raw(ptr as *mut T)) };
}

impl ValueData {
    /// Create a ValueData from raw components.
    pub fn from_raw(data: *mut (), drop_fn: extern "C" fn(*mut ())) -> Self {
        ValueData(data, drop_fn)
    }

    /// Create a ValueData from the given value.
    pub fn new<T>(v: T) -> Self {
        let ptr = Box::into_raw(Box::new(v)) as *mut ();
        Self::from_raw(ptr, drop_box::<T>)
    }

    /// Create a ValueData from a primitive value.
    pub fn primitive(v: usize) -> Self {
        Self::from_raw(v as *mut (), no_drop)
    }

    /// Get the ValueData as a &T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_ref<T: Sync>(&self) -> &T {
        (self.0 as *const T).as_ref().unwrap()
    }

    /// Get the ValueData as a primitive.
    ///
    /// Unsafe because callers must ensure the data is a primitive type.
    pub unsafe fn as_primitive(&self) -> usize {
        self.0 as usize
    }
}

impl Drop for ValueData {
    fn drop(&mut self) {
        self.1(self.0)
    }
}

type ValueResult = Result<Arc<ValueData>, String>;

/// A value shared amongst tasks.
///
/// Values have a type and a future value.
#[derive(Clone, Debug)]
pub struct Value {
    tp: Arc<ValueType>,
    data: Shared<BoxFuture<'static, ValueResult>>,
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

/// A group of dependencies, which can be used to get the final value id.
#[derive(Clone, Debug, Default)]
pub struct Dependencies {
    unordered: BTreeSet<Dependency>,
    ordered: Vec<Dependency>,
}

impl Dependencies {
    /// Create a new group of dependencies.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new group of unordered dependencies from a dependency source.
    pub fn unordered<'a, D: IntoDependencies<'a>>(deps: D) -> Self {
        Dependencies {
            unordered: BTreeSet::from_iter(deps.into_dependencies()),
            ordered: Vec::new(),
        }
    }

    /// Create a new group of ordered depedencies from a dependency source.
    pub fn ordered<'a, D: IntoDependencies<'a>>(deps: D) -> Self {
        Dependencies {
            unordered: BTreeSet::new(),
            ordered: Vec::from_iter(deps.into_dependencies()),
        }
    }

    /// Get the value id with the given ValueType.
    pub fn value_id_with(&self, tp: &ValueType) -> u128 {
        let mut hasher = HasherFn::default();
        tp.hash(&mut hasher);
        self.unordered.hash(&mut hasher);
        self.ordered.hash(&mut hasher);
        hasher.finish_ext()
    }

    /// Get the value id with the given type.
    pub fn value_id<T: GetValueType>(&self) -> u128 {
        self.value_id_with(&T::value_type())
    }
}

impl std::ops::Add for Dependencies {
    type Output = Self;

    /// Combine two Dependencies into one. The order matter with respect
    /// to any ordered dependencies that are stored.
    fn add(self, other: Self) -> Self {
        Dependencies {
            unordered: self
                .unordered
                .into_iter()
                .chain(other.unordered.into_iter())
                .collect(),
            ordered: self
                .ordered
                .into_iter()
                .chain(other.ordered.into_iter())
                .collect(),
        }
    }
}

impl IntoIterator for Dependencies {
    type Item = Dependency;
    type IntoIter = Box<dyn Iterator<Item = Dependency>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.unordered.into_iter().chain(self.ordered.into_iter()))
    }
}

impl IntoIterator for &Dependencies {
    type Item = Dependency;
    type IntoIter = Box<dyn Iterator<Item = Dependency>>;

    fn into_iter(self) -> Self::IntoIter {
        self.clone().into_iter()
    }
}

impl Value {
    /// Create a value with the given type, future, and dependencies.
    pub fn new<'a, F, D>(tp: ValueType, value: F, deps: D) -> Value
    where
        F: Future<Output = Result<ValueData, String>> + Send + 'static,
        D: IntoDependencies<'a>,
    {
        let deps = Dependencies::ordered(deps);
        let id = deps.value_id_with(&tp);
        Value {
            tp: Arc::new(tp),
            data: value.map_ok(Arc::new).boxed().shared(),
            dependencies: Arc::from_iter(deps.into_iter().filter_map(|v| match v {
                Dependency::Value(val) => Some(val),
                _ => None,
            })),
            id,
        }
    }

    /// Try to convert this Value to a TypedValue.
    ///
    /// If the conversion fails, the Err result contains the original Value.
    pub fn typed<T: GetValueType>(self) -> Result<TypedValue<T>, Value> {
        if *self.value_type() == T::value_type() {
            Ok(TypedValue {
                inner: self,
                phantom: Default::default(),
            })
        } else {
            Err(self)
        }
    }

    /// Get the value identifier.
    ///
    /// This identifier is deterministically derived from the value type and dependencies, so is
    /// functionally pure and consistent.
    pub fn id(&self) -> u128 {
        self.id
    }

    /// Get the type of the contained value.
    pub fn value_type(&self) -> &ValueType {
        &*self.tp
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> ValueResult {
        futures::executor::block_on(self)
    }
}

impl Future for Value {
    type Output = ValueResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.data) }, cx)
    }
}

/// An alias of ValueData that can be dereferenced to T.
pub struct Alias<T>(Arc<ValueData>, std::marker::PhantomData<T>);

impl<T> Alias<T> {
    /// Create a new alias from a ValueData.
    ///
    /// Unsafe because callers must ensure that the ValueData stores T.
    unsafe fn new(inner: Arc<ValueData>) -> Self {
        Alias(inner, Default::default())
    }
}

impl<T: Sync> std::ops::Deref for Alias<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { (*self.0).as_ref() }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedValue<T> {
    inner: Value,
    phantom: std::marker::PhantomData<T>,
}

impl<T: GetValueType> TypedValue<T> {
    /// Create a typed value from the given future and dependencies.
    pub fn new<'a, F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = Result<T, String>> + Send + 'static,
        D: IntoDependencies<'a>,
    {
        TypedValue {
            inner: Value::new(T::value_type(), value.map_ok(|r| ValueData::new(r)), deps),
            phantom: Default::default(),
        }
    }

    /// Create a constant value with the given type and data.
    pub fn constant(data: T) -> Self
    where
        T: Hash + Send + 'static,
    {
        let deps = depends![data];
        Self::new(futures::future::ok(data), deps)
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> Result<Alias<T>, String> {
        futures::executor::block_on(self)
    }

    /// Create a new TypedValue by consuming and mapping the result of this TypedValue.
    pub fn map<U, F>(self, f: F) -> TypedValue<U>
    where
        U: GetValueType,
        F: FnOnce(&T) -> Result<U, String> + Send + 'static,
        T: Sync + 'static,
    {
        let deps = depends![self];
        TypedValue::new(
            FutureExt::map(self, move |result| result.and_then(move |at| f(&*at))),
            deps,
        )
    }
}

impl<T> From<T> for TypedValue<T>
where T: GetValueType + Hash + Send + 'static
{
    fn from(v: T) -> Self {
        TypedValue::constant(v)
    }
}

// Value is Send (as it is just pointers to Sync values), so TypedValue should be Send as well.
unsafe impl<T> Send for TypedValue<T> {}

impl<T> Future for TypedValue<T> {
    type Output = Result<Alias<T>, String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.inner) }, cx)
            .map(|v| v.map(|data| unsafe { Alias::new(data) }))
    }
}

impl<T> std::ops::Deref for TypedValue<T> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> From<TypedValue<T>> for Value {
    fn from(v: TypedValue<T>) -> Self {
        v.inner
    }
}
