//! Types exposed by default in the so type system implemented using grease.
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
use grease::type_erase::Erased;
use grease::types::{GreaseType, TypeParameters};
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
    fn call(&self, ctx: &mut grease::runtime::Context<crate::FunctionCall>)
        -> crate::EvalResultAbi;
}

impl<F> FunctionAbi for F
where
    F: Fn(&mut grease::runtime::Context<crate::FunctionCall>) -> crate::EvalResult + Send + Sync,
{
    fn call(
        &self,
        ctx: &mut grease::runtime::Context<crate::FunctionCall>,
    ) -> crate::EvalResultAbi {
        self(ctx).into()
    }
}

impl Function {
    /// Create a new function with the given implementation.
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&mut grease::runtime::Context<crate::FunctionCall>) -> crate::EvalResult
            + Send
            + Sync
            + 'static,
    {
        Function(FunctionAbi_TO::from_value(f, TU_Opaque))
    }

    /// Call the function.
    pub fn call(
        &self,
        ctx: &mut grease::runtime::Context<crate::FunctionCall>,
    ) -> crate::EvalResult {
        self.0.call(ctx).into()
    }
}

/// Script either type, used for conditional results.
///
/// When Values are made with this type, the type should have sideband data with `TypeParameters`
/// of each of the possible `Value` types. `Either::new` manages this safely.
#[derive(Clone, Debug, GreaseType, StableAbi)]
#[repr(C)]
pub struct Either(usize, Value);

impl Either {
    pub fn new<F>(vals: Vec<Value>, index: TypedValue<usize>) -> TypedValue<Self> {
        let deps = depends![^@vals, index];
        let types: Vec<_> = vals
            .iter()
            .map(|v| v.grease_type().as_ref().clone())
            .collect();
        let mut tp = Self::grease_type();
        tp.sideband = TypeParameters(types).into();
        Value::new(
            RArc::new(tp),
            async move {
                let idx = index.await?;
                let v = vals.get(*idx).expect("invalid either index");
                Ok(RArc::new(Erased::new(Either(*idx, v.clone()))))
            },
            deps,
        )
        .typed::<Self>()
        .unwrap()
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
