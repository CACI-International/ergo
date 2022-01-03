//! Script values.

use crate::abi_stable::{
    bst::{BstMap, BstSet},
    future, sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox},
    type_erase::{Eraseable, Erased},
    u128::U128,
    StableAbi,
};
use crate::dependency::{
    Dependencies, DependenciesConstant, GetDependencies, GetDependenciesConstant,
};
use crate::hash::HashFn;
use crate::type_system::{ErgoType, Type};
use crate::Context;

/// A shared runtime value.
///
/// Values have an id and metadata, and either a type and associated data or a future to evaluate
/// it.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Value {
    id: RArc<ValueId>,
    metadata: BstMap<U128, RArc<Erased>>,
    data: ValueData,
}

// TODO replace with method from std
trait UnwrapUnchecked {
    type Output;
    unsafe fn unwrap_unchecked_(self) -> Self::Output;
}

impl<T> UnwrapUnchecked for Option<T> {
    type Output = T;

    unsafe fn unwrap_unchecked_(self) -> T {
        match self {
            Some(v) => v,
            None => std::hint::unreachable_unchecked(),
        }
    }
}

mod value_id {
    use super::*;
    use std::sync::atomic::{AtomicU8, Ordering};

    #[derive(StableAbi)]
    #[repr(C)]
    union ValueIdState {
        id: U128,
        deps: std::mem::ManuallyDrop<Dependencies>,
    }

    #[derive(Debug, StableAbi)]
    #[repr(C)]
    pub struct ValueId {
        state: std::cell::UnsafeCell<ValueIdState>,
        // Zeroth bit indicates whether the value has ever been accessed before.
        // First bit indicates whether `state` has `id` (and implies the zeroth bit).
        tag: AtomicU8,
    }

    unsafe impl Sync for ValueId {}

    // Box this future to break the async loop between Dependencies.hash and Value.hash
    fn calc_id(deps: &Dependencies) -> futures::future::BoxFuture<u128> {
        futures::future::FutureExt::boxed(async move {
            let mut h = HashFn::default();
            deps.hash(&mut h).await;
            h.finish_ext()
        })
    }

    impl ValueId {
        pub fn new(deps: Dependencies) -> Self {
            ValueId {
                state: std::cell::UnsafeCell::new(ValueIdState {
                    deps: std::mem::ManuallyDrop::new(deps),
                }),
                tag: AtomicU8::new(0b00),
            }
        }

        pub fn constant(deps: DependenciesConstant) -> Self {
            Self::id({
                let mut h = HashFn::default();
                std::hash::Hash::hash(&deps, &mut h);
                h.finish_ext()
            })
        }

        pub async fn immediate(deps: Dependencies) -> Self {
            Self::id(calc_id(&deps).await)
        }

        pub fn id(id: u128) -> Self {
            ValueId {
                state: std::cell::UnsafeCell::new(ValueIdState { id: id.into() }),
                tag: AtomicU8::new(0b11),
            }
        }

        pub fn try_get(&self) -> Option<&u128> {
            (self.tag.load(Ordering::Acquire) == 0b11).then(|| {
                // Safety: if the tag is 0b11, the state _must_ be a U128
                unsafe { self.state.get().as_ref().unwrap_unchecked_().id.as_ref() }
            })
        }

        pub async fn get(&self) -> u128 {
            let mut spins = 0;
            loop {
                match self.tag.fetch_or(0b01, Ordering::Acquire) {
                    0b00 => {
                        // Safety: if the tag is 0b00, the state must be deps
                        let state = unsafe { self.state.get().as_mut().unwrap_unchecked_() };
                        let deps = unsafe { std::mem::ManuallyDrop::take(&mut state.deps) };
                        let id = calc_id(&deps).await;
                        state.id = id.into();
                        self.tag.store(0b11, Ordering::Release);
                        break;
                    }
                    0b01 => {
                        std::hint::spin_loop();
                        spins += 1;
                        if spins == 50 {
                            std::thread::yield_now();
                            spins = 0;
                        }
                    }
                    0b11 => break,
                    _ => panic!("invalid value id state"),
                }
            }
            // Safety: the tag must be 0b11, so the state _must_ be a U128
            unsafe { self.state.get().as_ref().unwrap_unchecked_().id }.into()
        }
    }

    impl Drop for ValueId {
        fn drop(&mut self) {
            if self.tag.load(Ordering::Acquire) == 0b00 {
                // Safety: if the tag is 0b00, the state must be deps
                unsafe { std::mem::ManuallyDrop::drop(&mut self.state.get_mut().deps) };
            }
        }
    }

    impl From<u128> for ValueId {
        fn from(id: u128) -> Self {
            Self::id(id)
        }
    }

    impl From<Dependencies> for ValueId {
        fn from(deps: Dependencies) -> Self {
            Self::new(deps)
        }
    }

    impl From<DependenciesConstant> for ValueId {
        fn from(deps: DependenciesConstant) -> Self {
            Self::constant(deps)
        }
    }
}

use value_id::ValueId;

impl Value {
    /// Create a new evaluated Value with the given identity.
    ///
    /// ### Safety
    /// The caller must ensure the data corresponds to the type.
    pub unsafe fn new<T>(tp: RArc<Type>, data: RArc<Erased>, id: T) -> Self
    where
        T: Into<ValueId>,
    {
        Value {
            id: RArc::new(id.into()),
            metadata: Default::default(),
            data: ValueData::Typed { tp, data },
        }
    }

    /// Create a typed, evaluated value.
    pub fn evaluated<T: ErgoType + Eraseable + GetDependencies>(value: T) -> Self {
        let deps = value.get_depends();
        Self::with_id(value, deps)
    }

    /// Create a constant (typed, evaluated, identified) value.
    pub fn constant<T: ErgoType + Eraseable + GetDependenciesConstant>(value: T) -> Self {
        let deps = value.get_depends();
        Self::with_id(value, deps)
    }

    /// Create a typed, evaluated value with the given identity.
    pub fn with_id<T: ErgoType + Eraseable, D>(value: T, id: D) -> Self
    where
        D: Into<ValueId>,
    {
        unsafe { Self::new(RArc::new(T::ergo_type()), RArc::new(Erased::new(value)), id) }
    }

    /// Create a dynamically-typed (unevaluated) value with the given identity.
    ///
    /// `FnOnce + Clone` is used rather than `Fn` because this is more convenient for callers when
    /// moving values into the returned Future.
    pub fn dynamic<F, Fut, D>(func: F, id: D) -> Self
    where
        F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
        Fut: std::future::Future<Output = Value> + Send,
        D: Into<ValueId>,
    {
        let next = DynamicNext_TO::from_value(
            std::sync::Arc::new(futures::lock::Mutex::new(DynamicNextState::Pending(
                func,
                Default::default(),
            ))),
            TU_Opaque,
        );
        Value {
            id: RArc::new(id.into()),
            metadata: Default::default(),
            data: ValueData::Dynamic { next },
        }
    }

    /// Get the value's identity.
    pub async fn id(&self) -> u128 {
        self.id.get().await
    }

    /// Try to get the value's identity, if immediately available.
    pub fn try_id(&self) -> Option<&u128> {
        self.id.try_get()
    }

    /// Hash the value based on identity.
    pub async fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        use std::hash::Hash;
        self.id().await.hash(hasher);
    }

    /// Get the value's type, if evaluated.
    pub fn ergo_type(&self) -> Option<&Type> {
        match &self.data {
            ValueData::Typed { tp, .. } => Some(tp.as_ref()),
            _ => None,
        }
    }

    /// Get the value's data, if evaluated.
    pub fn data(&self) -> Option<&Erased> {
        match &self.data {
            ValueData::Typed { data, .. } => Some(data.as_ref()),
            _ => None,
        }
    }

    /// Check whether this value has the given type.
    pub fn is_type<T: ErgoType>(&self) -> bool {
        match &self.data {
            ValueData::Typed { tp, .. } => tp.as_ref() == &T::ergo_type(),
            _ => false,
        }
    }

    /// Get this value as the given type, if evaluated to that type.
    pub fn as_type<T: ErgoType>(self) -> Result<TypedValue<T>, Self> {
        match &self.data {
            ValueData::Typed { tp, .. } => {
                if tp.as_ref() == &T::ergo_type() {
                    return Ok(unsafe { TypedValue::from_value(self) });
                }
            }
            _ => (),
        }
        Err(self)
    }

    /// Get an IdentifiedValue from this value.
    pub async fn as_identified(self) -> IdentifiedValue {
        IdentifiedValue::from_value(self).await
    }

    /// Get this value as the given mutable type, if evaluated to that type and no other references
    /// exist.
    pub fn as_mut<T: ErgoType>(&mut self) -> Option<&mut T> {
        match &mut self.data {
            ValueData::Typed { tp, data } if tp.as_ref() == &T::ergo_type() => {
                if let Some(data) = RArc::get_mut(data) {
                    Some(unsafe { data.as_mut::<T>() })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Set the dependencies of this value.
    pub fn set_dependencies<D>(&mut self, deps: D)
    where
        D: Into<ValueId>,
    {
        self.id = RArc::new(deps.into());
    }

    /// Set a metadata entry for this value.
    pub fn set_metadata<T: MetadataKey>(&mut self, key: &T, value: T::Value) {
        self.metadata
            .insert(key.id().into(), RArc::new(Erased::new(value)));
    }

    /// Copy the metadata from another value.
    pub fn copy_metadata(&mut self, from: &Self) {
        self.metadata = from.metadata.clone();
    }

    /// Clear a metadata entry for this value.
    pub fn clear_metadata<T: MetadataKey>(&mut self, key: &T) {
        self.metadata.remove(&key.id());
    }

    /// Get a metadata entry for this value.
    pub fn get_metadata<T: MetadataKey>(&self, key: &T) -> Option<Ref<T::Value>> {
        self.metadata
            .get(&key.id())
            .map(|v| unsafe { Ref::new(v.clone()) })
    }

    /// Evaluate the value once (if dynamic).
    pub async fn eval_once(&mut self) {
        match std::mem::replace(&mut self.data, ValueData::None) {
            ValueData::Dynamic { next } => {
                let Value { id, metadata, data } = next.next().await;
                self.id = id;
                self.metadata.extend(metadata);
                self.data = data;
            }
            o => self.data = o,
        }
    }

    /// Return whether this value is fully evaluated (and has a type and data immediately available).
    pub fn is_evaluated(&self) -> bool {
        match &self.data {
            ValueData::Typed { .. } => true,
            _ => false,
        }
    }

    /// Cause an error to occur if this value is ever evaluated.
    pub fn unevaluated(&mut self) {
        self.data = ValueData::None;
    }
}

/// Match expression for Values.
///
/// Matching is based on types implementing ErgoType. You may write the match expression
/// approximately as you normally would, except that the match value comes first (followed by a
/// comma), each pattern must be a struct, tuple struct, or 'or' pattern, and the final pattern
/// must be a wildcard or identifier pattern (as the required else pattern). The struct patterns
/// are matched by reference to the value's data, and the else pattern is matched to the original
/// value (if the value is dynamically typed or none of the type cases matched).
///
/// ## Example
/// ```
/// # use ergo_runtime::value::{match_value, Value};
/// # use ergo_runtime::types::{Unit, Bool, MapEntry};
/// let value: Value = Unit.into();
/// let result = match_value! { value,
///     Bool(b) => b,
///     MapEntry { key, value } => true,
///     _ => false
/// };
/// assert_eq!(result, false);
/// ```
pub use ergo_runtime_macro::match_value;

/// A reference to a type-erased result.
pub type Ref<T> = crate::abi_stable::type_erase::Ref<T, RArc<Erased>>;

/// A value with a known rust type.
#[derive(StableAbi)]
#[repr(C)]
#[sabi(phantom_field = "phantom: RArc<T>")]
pub struct TypedValue<T> {
    inner: Value,
    phantom: std::marker::PhantomData<RArc<T>>,
}

impl<T> TypedValue<T> {
    /// Create a typed value from the given value.
    ///
    /// ### Safety
    /// The caller must ensure that the Value has type `T`.
    pub unsafe fn from_value(v: Value) -> Self {
        TypedValue {
            inner: v,
            phantom: Default::default(),
        }
    }
}

impl<T: ErgoType + Eraseable> TypedValue<T> {
    /// Create a typed value.
    pub fn new(data: T) -> Self
    where
        T: GetDependencies,
    {
        unsafe { TypedValue::from_value(Value::evaluated(data)) }
    }

    /// Create a value with constant dependencies.
    pub fn constant(data: T) -> Self
    where
        T: GetDependenciesConstant,
    {
        unsafe { TypedValue::from_value(Value::constant(data)) }
    }

    /// Create a value with the given identity.
    pub fn with_id<D: Into<ValueId>>(data: T, id: D) -> Self {
        unsafe { TypedValue::from_value(Value::with_id(data, id)) }
    }

    /// Extract the Value's type as an owned value.
    pub fn to_owned(self) -> T
    where
        T: Clone,
    {
        match self.inner.data {
            ValueData::Typed { data, .. } => match RArc::try_unwrap(data) {
                Ok(e) => unsafe { e.to_owned() },
                Err(arc) => unsafe { arc.as_ref().as_ref::<T>() }.clone(),
            },
            _ => panic!("invalid TypedValue"),
        }
    }
}

impl<T> Clone for TypedValue<T> {
    fn clone(&self) -> Self {
        TypedValue {
            inner: self.inner.clone(),
            phantom: Default::default(),
        }
    }
}

impl<T> std::ops::Deref for TypedValue<T> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for TypedValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> AsRef<T> for TypedValue<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.inner.data().unwrap().as_ref::<T>() }
    }
}

impl<T> From<TypedValue<T>> for Value {
    fn from(v: TypedValue<T>) -> Self {
        v.inner
    }
}

impl<T> std::borrow::Borrow<Value> for TypedValue<T> {
    fn borrow(&self) -> &Value {
        &self.inner
    }
}

/// Metadata key types.
///
/// Keys generate the metadata id and have an associated output type.
pub trait MetadataKey {
    type Value: Eraseable;

    /// Get the metadata identifier from this key.
    fn id(&self) -> u128;
}

#[sabi_trait]
trait DynamicNext: Clone + Send + Sync {
    fn ref_id(&self) -> usize;

    #[sabi(last_prefix_field)]
    fn next(self) -> future::BoxFuture<'static, Value>;
}

impl<F, Fut> DynamicNext for std::sync::Arc<futures::lock::Mutex<DynamicNextState<F>>>
where
    F: FnOnce() -> Fut + Clone + Send + Sync + 'static,
    Fut: std::future::Future<Output = Value> + Send,
{
    fn ref_id(&self) -> usize {
        self.as_ref() as *const futures::lock::Mutex<_> as usize
    }

    fn next(self) -> future::BoxFuture<'static, Value> {
        future::BoxFuture::new(async move {
            let lock_id = self.ref_id();
            if Context::with(|ctx| ctx.evaluating.locking_would_deadlock(lock_id)) {
                return crate::error!(error: "deadlock detected").into();
            }

            let mut guard = self.lock().await;
            match &mut *guard {
                DynamicNextState::Pending(func, complete) => {
                    if !complete.is_empty() {
                        let ids = Context::with(|ctx| ctx.dynamic_scope.ids());
                        if let Some(v) = complete.get(&ids) {
                            return v.clone();
                        }
                    }
                    Context::with(|ctx| ctx.evaluating.locked(lock_id));
                    let func_inner = func.clone();
                    let (val, accessed) =
                        Context::fork(|ctx| ctx.evaluating.push(lock_id), async move {
                            let val = func_inner().await;
                            let accessed = Context::with(|ctx| ctx.dynamic_scope.accessed());
                            (val, accessed)
                        })
                        .await;
                    if accessed.len() == 0 {
                        debug_assert!(complete.is_empty());
                        *guard = DynamicNextState::Done(val.clone());
                    } else {
                        complete.insert(&accessed, val.clone());
                    }
                    drop(guard);
                    Context::with(|ctx| ctx.evaluating.unlocked(lock_id));
                    val
                }
                DynamicNextState::Done(val) => val.clone(),
            }
        })
    }
}

#[derive(Default)]
struct DynamicComplete {
    // The completed values and the number of entries they have in ids.
    values: Vec<(Value, usize)>,
    // HashMap of (keyid, valueid) -> index in values
    ids: std::collections::HashMap<(u128, u128), Vec<usize>>,
}

impl DynamicComplete {
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn insert(&mut self, entries: &[(u128, u128)], value: Value) {
        debug_assert!(!entries.is_empty());
        let index = self.values.len();
        self.values.push((value, entries.len()));
        for e in entries {
            self.ids.entry(*e).or_default().push(index);
        }
    }

    pub fn get(&self, entries: &[(u128, u128)]) -> Option<&Value> {
        let mut counts: Vec<_> = self.values.iter().map(|(v, s)| (v, *s)).collect();
        for e in entries {
            if let Some(indices) = self.ids.get(e) {
                for i in indices {
                    let c = &mut counts[*i];
                    c.1 -= 1;
                    if c.1 == 0 {
                        return Some(&c.0);
                    }
                }
            }
        }
        None
    }
}

enum DynamicNextState<F> {
    Pending(F, DynamicComplete),
    Done(Value),
}

/// The data within a Value.
#[derive(Clone, StableAbi)]
#[repr(C)]
enum ValueData {
    Typed {
        tp: RArc<Type>,
        data: RArc<Erased>,
    },
    Dynamic {
        next: DynamicNext_TO<'static, RBox<()>>,
    },
    None,
}

impl std::fmt::Debug for ValueData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValueData::None => write!(f, "None"),
            ValueData::Dynamic { .. } => write!(f, "Dynamic"),
            ValueData::Typed { tp, .. } => write!(f, "Typed ({:?})", tp),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Value")
            .field("data", &self.data)
            .field(
                "metadata",
                &self.metadata.iter().map(|(k, _)| k).collect::<BstSet<_>>(),
            )
            .field("id", &self.id)
            .finish()
    }
}

impl<T> std::fmt::Debug for TypedValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("TypedValue")
            .field("inner", &self.inner)
            .finish()
    }
}

/// A value with an immediately-available identity.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct IdentifiedValue(Value);

impl IdentifiedValue {
    pub async fn from_value(v: Value) -> Self {
        v.id().await;
        IdentifiedValue(v)
    }

    pub fn id(&self) -> &u128 {
        // Safety: you cannot construct this type without first fetching the id
        unsafe { self.0.try_id().unwrap_unchecked_() }
    }
}

impl std::ops::Deref for IdentifiedValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// IdentifiedValue is not DerefMut because this may invalidate the id state (e.g. if a value has
// its identity changed).

impl From<IdentifiedValue> for Value {
    fn from(v: IdentifiedValue) -> Self {
        v.0
    }
}

impl std::borrow::Borrow<Value> for IdentifiedValue {
    fn borrow(&self) -> &Value {
        &self.0
    }
}

impl PartialEq for IdentifiedValue {
    fn eq(&self, other: &IdentifiedValue) -> bool {
        self.id() == other.id()
    }
}

impl Eq for IdentifiedValue {}

impl PartialOrd for IdentifiedValue {
    fn partial_cmp(&self, other: &IdentifiedValue) -> Option<std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
    }
}

impl Ord for IdentifiedValue {
    fn cmp(&self, other: &IdentifiedValue) -> std::cmp::Ordering {
        self.id().cmp(&other.id())
    }
}

impl std::borrow::Borrow<u128> for IdentifiedValue {
    fn borrow(&self) -> &u128 {
        self.id()
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
        self.into().inner
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

impl<T> From<T> for IdentifiedValue
where
    T: GetDependenciesConstant + IntoValue,
{
    fn from(v: T) -> IdentifiedValue {
        let val = v.into_value();
        assert!(val.try_id().is_some());
        IdentifiedValue(val)
    }
}
