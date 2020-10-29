//! Types exposed by default in the ergo type system implemented using grease.
//!
//! All types are ABI-stable.
//! Plugins may define other types; these are just common types that may be used among
//! implementors.

use abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RBox, RString, RVec},
    StableAbi,
};
use grease::bst::BstMap;
use grease::depends;
use grease::future::BoxFuture;
use grease::types::GreaseType;
use grease::value::{Dependencies, TypedValue, Value};

pub(crate) mod byte_stream;

pub use byte_stream::ByteStream;

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
pub struct Map(pub BstMap<Value, Value>);

impl From<Map> for TypedValue<Map> {
    fn from(v: Map) -> Self {
        let deps: grease::value::Dependencies = v.0.iter().map(|(k, v)| depends![*k, *v]).sum();
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
    F: for<'a> Fn(&'a mut crate::FunctionCall) -> futures::future::BoxFuture<'a, crate::EvalResult>
        + Send
        + Sync
        + 'static,
{
    fn call<'a>(&'a self, ctx: &'a mut crate::FunctionCall) -> BoxFuture<'a, crate::EvalResultAbi> {
        BoxFuture::new(async move { self(ctx).await.into() })
    }
}

impl Function {
    /// Create a new function with the given implementation.
    pub fn new<F>(f: F, deps: Dependencies) -> TypedValue<Self>
    where
        F: for<'a> Fn(
                &'a mut crate::FunctionCall,
            ) -> futures::future::BoxFuture<'a, crate::EvalResult>
            + Send
            + Sync
            + 'static,
    {
        TypedValue::constant_deps(Function(FunctionAbi_TO::from_value(f, TU_Opaque)), deps)
    }

    /// Call the function.
    pub async fn call(&self, ctx: &mut crate::FunctionCall<'_>) -> crate::EvalResult {
        self.0.call(ctx).await.into()
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Function ({:p})", self.0.obj.sabi_erased_ref())
    }
}

/// Create a namespace id from the given namespaced name.
///
/// Example usage:
/// ```
/// # #[macro_use] extern crate ergo_runtime;
/// namespace_id!(string::format);
/// ```
#[macro_export]
macro_rules! namespace_id {
    ( $( $l:ident )::+ ) => {
        {
            let mut id = ::grease::uuid::grease_uuid(b"ergo");
            $( id = ::grease::uuid::Uuid::new_v5(&id, stringify!($l).as_bytes()); )+
            id
        }
    }
}

/// A macro to wrap a closure returning a Value as an ergo Function.
///
/// The first argument should be a namespace identifier for the function, with an optional
/// 'independent' prefix which, if present, prevents the produced value from depending on the
/// function identity.
///
/// The closure takes a single &mut FunctionCall context argument, can use the try operator or
/// return EvalResult directly, and should evaluate to a Value.
#[macro_export]
macro_rules! ergo_function {
    ( independent $( $l:ident )::+ , $( $m:ident ),* |$ctx:ident| $body:expr ) => {
        $crate::types::Function::new(
            move |$ctx| {
                $( let $m = $m.clone(); )*
                $crate::future::FutureExt::boxed(
                    async move { Ok($ctx.call_site.clone().with($body)) }
                )
            },
            ::grease::depends![$crate::namespace_id!($( $l )::+)]
        )
    };

    ( $( $l:ident )::+ , $( $m:ident ),* |$ctx:ident| $body:expr ) => {
        $crate::ergo_function!(independent $( $l )::+, $( $m ),* |$ctx| {
            let val: ::grease::value::Value = $body;
            let new_deps = ::grease::depends![$crate::namespace_id!($( $l )::+), val.id()];
            val.set_dependencies(new_deps)
        })
    };
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
