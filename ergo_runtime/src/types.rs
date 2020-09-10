//! Types exposed by default in the ergo type system implemented using grease.
//!
//! All types are ABI-stable.
//! Plugins may define other types; these are just common types that may be used among
//! implementors.

use abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, RString, RVec},
    StableAbi,
};
use grease::bst::BstMap;
use grease::depends;
use grease::future::BoxFuture;
use grease::type_erase::Erased;
use grease::types::{GreaseType, Type, TypeParameters};
use grease::value::{TypedValue, Value};

/// Script unit type.
pub type Unit = ();

/// Script string type.
pub type String = RString;

/// Script array type.
#[derive(Clone, Debug, GreaseType, PartialEq, StableAbi)]
#[repr(C)]
pub struct Array(pub RVec<Value>);

impl From<Array> for TypedValue<Array> {
    fn from(v: Array) -> Self {
        let deps = depends![^@v.0];
        Self::constant_deps(v, deps)
    }
}

/// Script map type.
#[derive(Clone, Debug, GreaseType, PartialEq, StableAbi)]
#[repr(C)]
pub struct Map(pub BstMap<RString, Value>);

impl From<Map> for TypedValue<Map> {
    fn from(v: Map) -> Self {
        let deps: grease::value::Dependencies = v.0.iter().map(|(a, v)| depends![*a, *v]).sum();
        Self::constant_deps(v, deps)
    }
}

/// Script function type.
#[derive(GreaseType, StableAbi)]
#[repr(C)]
pub struct Function(FunctionAbi_TO<'static, RBox<()>>);

#[sabi_trait]
trait FunctionAbi: Send + Sync {
    #[sabi(last_prefix_field)]
    fn call<'a>(&'a self, ctx: &'a mut crate::FunctionCall) -> BoxFuture<'a, crate::EvalResultAbi>;
}

impl<F> FunctionAbi for F
where
    F: for<'a> Fn(&'a mut crate::FunctionCall) -> futures::future::BoxFuture<'a, crate::EvalResult> + Send + Sync + 'static,
{
    fn call<'a>(&'a self, ctx: &'a mut crate::FunctionCall) -> BoxFuture<'a, crate::EvalResultAbi> {
        BoxFuture::new(async move { self(ctx).await.into() })
    }
}

impl Function {
    /// Create a new function with the given implementation.
    pub fn new<F>(f: F) -> Self
    where
        F: for<'a> Fn(&'a mut crate::FunctionCall) -> futures::future::BoxFuture<'a, crate::EvalResult> + Send + Sync + 'static,
    {
        Function(FunctionAbi_TO::from_value(f, TU_Opaque))
    }

    /// Call the function.
    pub async fn call(&self, ctx: &mut crate::FunctionCall<'_>) -> crate::EvalResult {
        self.0.call(ctx).await.into()
    }
}

/// A function which can be called using only function arguments and a call site source.
#[derive(GreaseType)]
pub struct PortableFunction(grease::value::Ref<Function>, crate::Runtime);

impl PortableFunction {
    /// Call the function with the given arguments and call site source location.
    pub async fn call(
        &self,
        args: crate::FunctionArguments,
        call_site: crate::source::Source<()>,
    ) -> crate::EvalResult {
        let f = &self.0;
        f.call(&mut crate::FunctionCall::new(
            &mut self.1.clone(),
            args,
            call_site,
        ))
        .await
    }
}

/// Portable function extension trait.
pub trait Portable {
    /// Wrap a function's context into a portable function to be called later.
    fn portable(self, ctx: &mut crate::Runtime) -> TypedValue<PortableFunction>;
}

impl Portable for TypedValue<Function> {
    fn portable(self, ctx: &mut crate::Runtime) -> TypedValue<PortableFunction> {
        let mut c = ctx.clone();
        c.global_env.clear();
        c.env.clear();
        self.map(|v| PortableFunction(v, c))
    }
}

/// Script either type, used for conditional results.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Either(usize, Value);

impl GreaseType for Either {
    fn grease_type() -> Type {
        Type::named(b"ergo_runtime::types::Either")
    }

    /// Ignores the extra type information.
    fn matches_grease_type(a: &Type) -> bool {
        Self::grease_type().id == a.id
    }
}

impl Either {
    /// Create a new Either TypedValue.
    pub fn new(vals: Vec<Value>, index: TypedValue<usize>) -> TypedValue<Self> {
        let deps = depends![^@vals, index];
        let types: Vec<_> = vals
            .iter()
            .map(|v| v.grease_type().as_ref().clone())
            .collect();
        Value::new(
            RArc::new(Self::full_grease_type(types)),
            async move {
                let idx = index.await?;
                let v = vals.get(*idx).expect("invalid either index");
                Ok(RArc::new(Erased::new(Either(*idx, v.clone()))))
            },
            deps,
        )
        .typed::<Self>()
        .expect("must return an Either type")
    }

    /// Get the full grease type (with either types).
    pub fn full_grease_type(types: Vec<Type>) -> Type {
        let mut tp = Self::grease_type();
        tp.data = TypeParameters(types).into();
        tp
    }

    /// Create an Either with the given resolved index and value.
    pub fn with_value(index: usize, value: Value) -> Self {
        Either(index, value)
    }

    /// The index of the chosen value.
    pub fn index(&self) -> usize {
        self.0
    }

    /// The chosen value.
    pub fn value(&self) -> Value {
        self.1.clone()
    }
}

impl std::hash::Hash for Function {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        std::ptr::hash(self.0.obj.sabi_erased_ref(), h)
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Function ({:p})", self.0.obj.sabi_erased_ref())
    }
}
