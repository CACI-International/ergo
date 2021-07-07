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
use crate::dependency::{Dependencies, GetDependencies};
use crate::hash::HashFn;
use crate::type_system::{ErgoType, Type};

/// A shared runtime value.
///
/// Values have an id and metadata, and either a type and associated data or a future to evaluate
/// it.
#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Value {
    id: U128,
    metadata: BstMap<U128, RArc<Erased>>,
    data: ValueData,
}

/// Calculate the value id for a value given the dependencies.
pub fn value_id(deps: &Dependencies) -> u128 {
    use std::hash::Hash;
    let mut h = HashFn::default();
    deps.hash(&mut h);
    h.finish_ext().into()
}

impl Value {
    /// Create a new evaluated Value with the given identity.
    ///
    /// ### Safety
    /// The caller must ensure the data corresponds to the type.
    pub unsafe fn with_id(tp: RArc<Type>, data: RArc<Erased>, id: u128) -> Self {
        Value {
            id: id.into(),
            metadata: Default::default(),
            data: ValueData::Typed { tp, data },
        }
    }

    /// Create a new evaluated Value with the given dependencies.
    ///
    /// ### Safety
    /// The caller must ensure the data corresponds to the type.
    pub unsafe fn new<D>(tp: RArc<Type>, data: RArc<Erased>, deps: D) -> Self
    where
        D: Into<Dependencies>,
    {
        let deps = deps.into();
        let id = value_id(&deps);
        Self::with_id(tp, data, id)
    }

    /// Create a constant (typed, evaluated) value.
    pub fn constant<T: ErgoType + Eraseable>(value: T) -> Self
    where
        T: GetDependencies,
    {
        let deps = value.get_depends();
        Self::constant_deps(value, deps)
    }

    /// Create a constant (typed, evaluated) value with the given dependencies.
    pub fn constant_deps<T: ErgoType + Eraseable, D>(value: T, deps: D) -> Self
    where
        D: Into<Dependencies>,
    {
        unsafe {
            Self::new(
                RArc::new(T::ergo_type()),
                RArc::new(Erased::new(value)),
                deps.into(),
            )
        }
    }

    /// Create a dynamically-typed (unevaluated) value with the given dependencies.
    ///
    /// `FnOnce + Clone` is used rather than `Fn` because this is more convenient for callers when
    /// moving values into the returned Future.
    pub fn dyn_new<'b, F, Fut, D>(f: F, deps: D) -> Self
    where
        F: FnOnce(&'b crate::Context) -> Fut + Clone + Send + Sync + 'static,
        Fut: std::future::Future<Output = Value> + Send + 'b,
        D: Into<Dependencies>,
    {
        let deps = deps.into();
        let id = value_id(&deps);
        Self::dyn_with_id(f, id)
    }

    /// Create a dynamically-typed (unevaluated) value with the given identity.
    pub fn dyn_with_id<'b, F, Fut>(func: F, id: u128) -> Self
    where
        F: FnOnce(&'b crate::Context) -> Fut + Clone + Send + Sync + 'static,
        Fut: std::future::Future<Output = Value> + Send + 'b,
    {
        let next = DynamicNext_TO::from_value(
            std::sync::Arc::new(futures::lock::Mutex::new(DynamicNextState::NeedContext {
                known_context_dependent: false,
                func,
                phantom: std::marker::PhantomData,
            })),
            TU_Opaque,
        );
        // The value _is_ actually 'static, but with `Fut` in the type, it thinks it is 'b
        let next =
            unsafe { std::mem::transmute::<DynamicNext_TO<_>, DynamicNext_TO<'static, _>>(next) };
        Value {
            id: id.into(),
            metadata: Default::default(),
            data: ValueData::Dynamic { next },
        }
    }

    /// Get the value's identity.
    pub fn id(&self) -> u128 {
        *self.id
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

    /// Set the dependencies of this value.
    pub fn set_dependencies<D>(&mut self, deps: D)
    where
        D: Into<Dependencies>,
    {
        self.id = value_id(&deps.into()).into();
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
    pub async fn eval_once(&mut self, ctx: &crate::Context) {
        match std::mem::replace(&mut self.data, ValueData::None) {
            ValueData::Dynamic { next } => {
                let Value { id, metadata, data } = next.next(ctx).await;
                self.id = id;
                self.metadata.extend(metadata);
                self.data = data;
            }
            o => self.data = o,
        }
    }

    /// Evaluate this value until it is typed.
    pub async fn eval(&mut self, ctx: &crate::Context) {
        while !self.is_evaluated() {
            self.eval_once(ctx).await;
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
#[derive(PartialEq, Eq, PartialOrd, Ord, StableAbi)]
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
    /// Create a constant value.
    pub fn constant(data: T) -> Self
    where
        T: GetDependencies,
    {
        unsafe { TypedValue::from_value(Value::constant(data)) }
    }

    /// Create a constant value with the given dependencies.
    pub fn constant_deps<D>(data: T, deps: D) -> Self
    where
        D: Into<Dependencies>,
    {
        unsafe { TypedValue::from_value(Value::constant_deps(data, deps)) }
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
    #[sabi(last_prefix_field)]
    fn next<'a>(self, ctx: &'a crate::Context) -> future::BoxFuture<'a, Value>;
}

impl<'b, F, Fut> DynamicNext for std::sync::Arc<futures::lock::Mutex<DynamicNextState<F, Fut>>>
where
    F: FnOnce(&'b crate::Context) -> Fut + Clone + Send + Sync + 'b,
    Fut: std::future::Future<Output = Value> + Send + 'b,
{
    fn next<'a>(self, ctx: &'a crate::Context) -> future::BoxFuture<'a, Value> {
        // Let 'b = 'a
        let ctx = unsafe { std::mem::transmute::<&'a _, &'b _>(ctx) };
        let fut = future::BoxFuture::new(async move {
            let mut guard = self.lock().await;
            match &mut *guard {
                DynamicNextState::NeedContext {
                    known_context_dependent,
                    func,
                    ..
                } => {
                    // TODO determine if Pending is returned from a lock() call
                    if *known_context_dependent {
                        (func.clone())(ctx).await
                    } else {
                        let func = func.clone();
                        let (val, accessed) = ctx
                            .fork(
                                |_| {},
                                move |ctx| async move {
                                    let ret = func(unsafe { std::mem::transmute(ctx) }).await;
                                    let accessed = ctx.dynamic_scope.accessed();
                                    (ret, accessed)
                                },
                            )
                            .await;
                        if accessed {
                            *known_context_dependent = true;
                        } else {
                            *guard = DynamicNextState::Done(val.clone());
                        }
                        val
                    }
                }
                DynamicNextState::Done(val) => val.clone(),
            }
        });
        // Change 'b back to 'a
        unsafe {
            std::mem::transmute::<future::BoxFuture<'b, Value>, future::BoxFuture<'a, Value>>(fut)
        }
    }
}

enum DynamicNextState<F, Fut> {
    NeedContext {
        known_context_dependent: bool,
        func: F,
        phantom: std::marker::PhantomData<fn() -> Fut>,
    },
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

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.id == other.id
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Value) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl std::borrow::Borrow<u128> for Value {
    fn borrow(&self) -> &u128 {
        self.id.as_ref()
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
