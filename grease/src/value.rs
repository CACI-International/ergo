//! Plan values.
//!
//! A plan is a graph of values, where each value may depend on others. Plans use asynchronous
//! values to build the graph and dependency tree.

use fasthash::{HasherExt, SpookyHasherExt as HasherFn};
use futures::future::{BoxFuture, FusedFuture, Future, FutureExt, Shared, TryFutureExt};
use futures::task::{Context, Poll};
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};
use std::io::{BufRead, BufReader, Read};
use std::iter::FromIterator;
use std::pin::Pin;
use std::sync::Arc;
use uuid::Uuid;

use crate::runtime::call_on_error;

mod types;

pub use types::*;

/// A value type.
///
/// The type is an id and type-specific data.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[repr(align(8))]
struct ValueDataBuffer([u8; 24]);

impl ValueDataBuffer {
    /// Panics if T has improper size or alignment.
    pub fn new<T>(v: T) -> Self {
        assert!(std::mem::size_of_val(&v) <= std::mem::size_of::<Self>());
        assert!(std::mem::align_of_val(&v) <= std::mem::align_of::<Self>());
        let mut ret = ValueDataBuffer(unsafe { std::mem::MaybeUninit::uninit().assume_init() });
        let ptr = &mut ret.0 as *mut [u8; 24] as *mut T;
        unsafe {
            ptr.write(v);
        }
        ret
    }

    /// Convert into the given type.
    pub fn as_value<T>(&mut self) -> T {
        let ptr = &mut self.0 as *mut [u8; 24] as *mut T;
        unsafe { ptr.read() }
    }

    /// Get a reference of type T.
    pub fn as_ref<T>(&self) -> &T {
        let ptr = &self.0 as *const [u8; 24] as *const T;
        unsafe { ptr.as_ref().unwrap() }
    }
}

/// Value data with a stored drop function.
///
/// The drop function is called when the ValueData instance itself is dropped.
/// The bool flag indicates whether the stored data is a Box.
pub struct ValueData(ValueDataBuffer, extern "C" fn(&mut ValueDataBuffer), bool);

// ValueData should always be Send, and should be Sync if the value it stores is Sync. However, we
// cannot enforce the Sync restriction, so we instead ensure values are Sync in the Alias struct,
// which is used to access ValueData.
unsafe impl Send for ValueData {}
unsafe impl Sync for ValueData {}

extern "C" fn no_drop(_v: &mut ValueDataBuffer) {}

extern "C" fn drop_type<T>(v: &mut ValueDataBuffer) {
    drop(v.as_value::<T>());
}

impl ValueData {
    /// Create a ValueData from the given value.
    pub fn new<T>(v: T) -> Self {
        if std::mem::size_of_val(&v) <= std::mem::size_of::<ValueDataBuffer>()
            && std::mem::align_of_val(&v) <= std::mem::align_of::<ValueDataBuffer>()
        {
            Self::from_type(v, false)
        } else {
            // Box<T> is defined as having the same size as usize/pointer, so the size
            // assertions should be true on any platform.
            Self::from_type(Box::new(v), true)
        }
    }

    /// Create a ValueData from the given type, assuming the size and alignment of the type will
    /// fit in the ValueDataBuffer. Panics if these constraints are not satisfied (in debug
    /// builds).
    fn from_type<T>(v: T, boxed: bool) -> Self {
        debug_assert!(std::mem::size_of::<T>() <= std::mem::size_of::<ValueDataBuffer>());
        debug_assert!(std::mem::align_of::<T>() <= std::mem::align_of::<ValueDataBuffer>());
        ValueData(
            ValueDataBuffer::new(v),
            if std::mem::needs_drop::<T>() {
                drop_type::<T>
            } else {
                no_drop
            },
            boxed,
        )
    }

    /// Get the ValueData as a &T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_ref<T: Sync>(&self) -> &T {
        if self.2 {
            let b: &Box<T> = self.0.as_ref();
            b.as_ref()
        } else {
            self.0.as_ref()
        }
    }

    /// Get the ValueData as an owned value.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn owned<T>(mut self) -> T {
        let mut v: ValueDataBuffer = std::mem::MaybeUninit::uninit().assume_init();
        std::mem::swap(&mut v, &mut self.0);
        self.1 = no_drop;
        if self.2 {
            let v = v.as_value::<Box<T>>();
            *v
        } else {
            let v = v.as_value::<T>();
            v
        }
    }
}

impl Drop for ValueData {
    fn drop(&mut self) {
        self.1(&mut self.0)
    }
}

type ExternalError = dyn std::error::Error + Send + Sync;

#[derive(Clone, Debug)]
pub struct Error {
    inner: InnerError,
}

#[derive(Clone, Debug)]
pub struct WrappedError {
    inner: InnerError,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl std::fmt::Display for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Clone, Debug)]
enum InnerError {
    Aborted,
    New(Arc<ExternalError>),
    Nested(Vec<Arc<ExternalError>>),
}

impl std::fmt::Display for InnerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InnerError::*;
        match self {
            Aborted => write!(f, "aborted"),
            New(e) => write!(f, "{}", e),
            Nested(es) => {
                for e in es {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for InnerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            InnerError::New(e) => Some(e.as_ref()),
            _ => None,
        }
    }
}

impl std::error::Error for WrappedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

impl Error {
    /// Change the error into a type supporting std::error::Error.
    pub fn error(self) -> WrappedError {
        WrappedError { inner: self.inner }
    }

    /// Get a reference to the error as an external error.
    pub fn error_ref(&self) -> &ExternalError {
        self.as_ref()
    }

    pub(crate) fn aborted() -> Self {
        Error {
            inner: InnerError::Aborted,
        }
    }
}

impl AsRef<ExternalError> for Error {
    fn as_ref(&self) -> &ExternalError {
        &self.inner
    }
}

fn has_inner_error(v: &(dyn std::error::Error + 'static)) -> bool {
    if v.is::<InnerError>() {
        true
    } else if let Some(s) = v.source() {
        has_inner_error(s)
    } else {
        false
    }
}

impl<T> From<T> for Error
where
    T: Into<Box<ExternalError>>,
{
    fn from(v: T) -> Self {
        let ext: Box<ExternalError> = v.into();
        Error {
            inner: match ext.downcast::<WrappedError>() {
                Ok(v) => v.inner,
                // TODO: traverse source() to find WrappedError?
                Err(e) => {
                    if !has_inner_error(e.as_ref()) {
                        call_on_error();
                    }
                    InnerError::New(e.into())
                }
            },
        }
    }
}

impl std::iter::FromIterator<Error> for Error {
    fn from_iter<I: IntoIterator<Item = Error>>(iter: I) -> Self {
        let errs: Vec<Error> = iter.into_iter().collect();

        if errs.len() == 1 {
            errs.into_iter().next().unwrap()
        } else {
            Error {
                inner: InnerError::Nested(
                    errs.into_iter()
                        .map(|e| match e.inner {
                            InnerError::Aborted => vec![],
                            InnerError::New(e) => vec![e],
                            InnerError::Nested(es) => es,
                        })
                        .flatten()
                        .collect(),
                ),
            }
        }
    }
}

pub type ValueResult = Result<Arc<ValueData>, Error>;

/// A value shared amongst tasks.
///
/// Values have a type and a future value.
#[derive(Clone, Debug)]
pub struct Value {
    tp: Arc<ValueType>,
    data: Shared<BoxFuture<'static, ValueResult>>,
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
    pub fn new<'a, F, D>(tp: Arc<ValueType>, value: F, deps: D) -> Value
    where
        F: Future<Output = ValueResult> + Send + 'static,
        D: IntoDependencies<'a>,
    {
        let deps = Dependencies::ordered(deps);
        let id = deps.value_id_with(&tp);
        Value {
            tp,
            data: value.boxed().shared(),
            id,
        }
    }

    /// Create a Value from raw components.
    pub fn from_raw(tp: Arc<ValueType>, data: Arc<ValueData>, id: u128) -> Self {
        Value {
            tp,
            data: futures::future::ok(data).boxed().shared(),
            id,
        }
    }

    /// Set dependencies of this value, producing a new value with a new identifier.
    ///
    /// The value will have the same type and evaluate to the same value as the original.
    pub fn set_dependencies<'a, D>(self, deps: D) -> Value
    where
        D: IntoDependencies<'a>,
    {
        let deps = Dependencies::ordered(deps);
        let id = deps.value_id_with(&self.tp);
        Value {
            tp: self.tp,
            data: self.data,
            id,
        }
    }

    /// Cause an error to occur if this value's future is ever evaluated.
    pub fn unevaluated(self) -> Value {
        Value {
            data: futures::future::err("unevaluated value".into())
                .boxed()
                .shared(),
            ..self
        }
    }

    /// Return a value that computes this value, discards the result (on success), and returns the
    /// given value.
    pub fn then(self, v: Value) -> Value {
        let deps = Dependencies::ordered(depends![self, v]);
        Self::new(v.value_type(), self.and_then(move |_| v), deps)
    }

    /// Map the error value, returning an identical value with an altered error.
    pub fn map_err<F, E>(self, f: F) -> Value
    where
        F: FnOnce(Error) -> E + Send + 'static,
        E: Into<Error>,
    {
        Value {
            data: self.data.map_err(|e| f(e).into()).boxed().shared(),
            ..self
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

    /// Try to convert this Value by reference to a TypedValue reference.
    ///
    /// If the conversion fails, a unit Err result is returned.
    pub fn typed_ref<'a, T: GetValueType>(&'a mut self) -> Result<TypedValueRef<'a, T>, ()> {
        if *self.value_type() == T::value_type() {
            Ok(TypedValueRef::new(self))
        } else {
            Err(())
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
    pub fn value_type(&self) -> Arc<ValueType> {
        self.tp.clone()
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> ValueResult {
        futures::executor::block_on(self)
    }

    /// Get the result of the value, if immediately available.
    pub fn peek(&self) -> Option<&ValueResult> {
        self.data.peek()
    }
}

impl Future for Value {
    type Output = ValueResult;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.data) }, cx)
    }
}

impl FusedFuture for Value {
    fn is_terminated(&self) -> bool {
        self.data.is_terminated()
    }
}

/// Match expression for ValueTypes.
///
/// Matching is based on types, not patterns, and each type must implement GetValueType. The else
/// case is required.
///
/// The evaluation contex is similar to regular match expressions, in that flow-control statements
/// will pertain to the calling code.
#[macro_export]
macro_rules! match_value_type {
    ( $value:expr => { $( $t:ty => $e:expr $(,)? )+ => $else:expr } ) => {
        $( if $value == <$t>::value_type() { $e } else )+ { $else }
    }
}

/// Match expression for Values.
///
/// Matching is based on types, not patterns, and each type must implement GetValueType. The else
/// case is required. All cases must be in the form `|name| body` that evaluate to a final type T,
/// and expressions must all agree on this final type. For each `body`, `name` will be bound to a
/// TypedValue according to the case type.
///
/// The evaluation context is similar to regular match expressions, in that flow-control statements
/// will pertain to the calling code.
#[macro_export]
macro_rules! match_value {
    ( $value:expr => { $( $t:ty => |$bind:pat| $e:expr $(,)? )+ => |$elsebind:pat| $else:expr } ) => {
        loop {
            let mut match_value__val = $value;
            $( match_value__val = match match_value__val.typed::<$t>() {
                Ok($bind) => break $e,
                Err(v) => v
            };)*
            {
                let $elsebind = match_value__val;
                break $else
            }
        }
    }
}

/// An alias of ValueData that can be dereferenced to T.
pub struct Alias<T>(Arc<ValueData>, std::marker::PhantomData<Arc<T>>);

impl<T> Alias<T> {
    /// Create a new alias from a ValueData.
    ///
    /// Unsafe because callers must ensure that the ValueData stores T.
    unsafe fn new(inner: Arc<ValueData>) -> Self {
        Alias(inner, Default::default())
    }
}

impl<T> std::fmt::Debug for Alias<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Alias")
            .field("data", &format_args!("{:p}", self.0))
            .finish()
    }
}

impl<T: Clone + Sync> Alias<T> {
    /// Get an owned version of the value.
    ///
    /// This may clone or simply move the value, depending on whether the value is needed
    /// elsewhere.
    pub fn owned(self) -> T {
        match Arc::try_unwrap(self.0) {
            Ok(v) => unsafe { v.owned() },
            Err(r) => unsafe {
                let v: &T = (*r).as_ref();
                v.clone()
            },
        }
    }
}

impl<T: Sync> std::ops::Deref for Alias<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { (*self.0).as_ref() }
    }
}

impl<T: Sync> AsRef<T> for Alias<T> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedValue<T> {
    inner: Value,
    phantom: std::marker::PhantomData<Arc<T>>,
}

impl<T: GetValueType> TypedValue<T> {
    /// Create a typed value from the given future and dependencies.
    pub fn new<'a, F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = Result<T, Error>> + Send + 'static,
        D: IntoDependencies<'a>,
    {
        TypedValue {
            inner: Value::new(
                T::value_type().into(),
                value.map_ok(|r| ValueData::new(r).into()),
                deps,
            ),
            phantom: Default::default(),
        }
    }

    /// Create a typed value from the given future (which cannot fail) and dependencies.
    pub fn ok<'a, F, D>(value: F, deps: D) -> Self
    where
        F: Future<Output = T> + Send + 'static,
        D: IntoDependencies<'a>,
    {
        Self::new(value.map(|v| Ok(v)), deps)
    }

    /// Create a constant value.
    pub fn constant(data: T) -> Self
    where
        T: Hash + Send + 'static,
    {
        let deps = depends![data];
        Self::constant_deps(data, deps)
    }

    /// Create a constant value with the given dependencies.
    pub fn constant_deps<'a, D>(data: T, deps: D) -> Self
    where
        T: Send + 'static,
        D: IntoDependencies<'a>,
    {
        Self::ok(futures::future::ready(data), deps)
    }

    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> Result<Alias<T>, Error> {
        futures::executor::block_on(self)
    }

    /// Create a new TypedValue by consuming and mapping the result of this TypedValue.
    pub fn map<U, F>(self, f: F) -> TypedValue<U>
    where
        U: GetValueType,
        F: FnOnce(Alias<T>) -> U + Send + 'static,
        T: Sync + 'static,
    {
        self.and_then(move |v| Ok(f(v)))
    }

    /// Create a new TypedValue by consuming and applying the given function (which can fail) over
    /// the result of this TypedValue.
    pub fn and_then<U, F>(self, f: F) -> TypedValue<U>
    where
        U: GetValueType,
        F: FnOnce(Alias<T>) -> Result<U, Error> + Send + 'static,
        T: Sync + 'static,
    {
        let deps = depends![self];
        TypedValue::new(
            FutureExt::map(self, move |result| result.and_then(move |at| f(at))),
            deps,
        )
    }

    /// Get a by-reference typed value.
    pub fn as_ref<'a>(&'a mut self) -> TypedValueRef<'a, T> {
        TypedValueRef::new(&mut self.inner)
    }
}

impl<T> From<T> for TypedValue<T>
where
    T: GetValueType + Hash + Send + 'static,
{
    fn from(v: T) -> Self {
        TypedValue::constant(v)
    }
}

pub struct TypedValueRef<'a, T> {
    inner: &'a mut Value,
    phantom: std::marker::PhantomData<&'a T>,
}

impl<'a, T> TypedValueRef<'a, T> {
    fn new(inner: &'a mut Value) -> Self {
        TypedValueRef {
            inner,
            phantom: Default::default(),
        }
    }
}

impl<'a, T: Sync> TypedValueRef<'a, T> {
    /// Get the result of the value.
    ///
    /// In general, this should only be called on a top-level value. This will block the caller
    /// until the result is available.
    pub fn get(self) -> Result<&'a T, Error> {
        futures::executor::block_on(self)
    }
}

/// A convenience trait for converting directly to a value.
pub trait IntoValue {
    fn into_value(self) -> Value;
}

impl<T> IntoValue for T
where
    T: Into<TypedValue<T>>,
{
    fn into_value(self) -> Value {
        self.into().into()
    }
}

impl<T> From<T> for Value
where
    T: IntoValue,
{
    fn from(v: T) -> Value {
        v.into_value()
    }
}

// Value is Send (as it is just pointers to Sync values), so TypedValue should be Send as well.
unsafe impl<T> Send for TypedValue<T> {}

impl<T> Future for TypedValue<T> {
    type Output = Result<Alias<T>, Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| &mut s.inner) }, cx)
            .map(|v| v.map(|data| unsafe { Alias::new(data) }))
    }
}

impl<'a, T: 'a + Sync> Future for TypedValueRef<'a, T> {
    type Output = Result<&'a T, Error>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        Future::poll(unsafe { self.map_unchecked_mut(|s| s.inner) }, cx)
            .map(|v| v.map(|v| unsafe { std::mem::transmute::<&T, &'a T>((*v).as_ref::<T>()) }))
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
