//! Types exposed by default in the ergo type system implemented using grease.
//!
//! All types are ABI-stable.
//! Plugins may define other types; these are just common types that may be used among
//! implementors.

use crate::metadata::Doc;
use crate::Source;
use abi_stable::{
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RBox, RString, RVec},
    StableAbi,
};
use grease::bst::BstMap;
use grease::depends;
use grease::future::BoxFuture;
use grease::make_value;
use grease::types::GreaseType;
use grease::value::{Dependencies, TypedValue, Value};

pub(crate) mod byte_stream;

pub use byte_stream::ByteStream;

/// Script unit type.
#[derive(Clone, Debug, GreaseType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct Unit;

impl From<Unit> for TypedValue<Unit> {
    fn from(v: Unit) -> Self {
        let mut v = Self::constant(v);
        v.set_metadata(&Doc, make_value!(Ok("()".into())));
        v
    }
}

/// Script string type.
#[derive(Clone, Debug, Default, GreaseType, PartialEq, Hash, Eq, StableAbi)]
#[repr(C)]
pub struct String(pub RString);

impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: Into<RString>> From<T> for String {
    fn from(t: T) -> Self {
        String(t.into())
    }
}

impl std::ops::Deref for String {
    type Target = RString;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for String {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<String> for TypedValue<String> {
    fn from(v: String) -> Self {
        let mut v = Self::constant(v.clone());
        let v_meta = v.clone();
        v.set_metadata(&Doc, v_meta);
        v
    }
}

impl String {
    /// Create a new (empty) string.
    pub fn new() -> Self {
        Default::default()
    }

    /// Convert this value into a std String.
    pub fn into_string(self) -> std::string::String {
        self.0.into_string()
    }
}

/// Script array type.
#[derive(Clone, Debug, GreaseType, PartialEq, StableAbi)]
#[repr(C)]
pub struct Array(pub RVec<Value>);

impl From<Array> for TypedValue<Array> {
    fn from(v: Array) -> Self {
        let deps = depends![^@v.0];
        let doc = format!("array with {} elements", v.0.len());
        let mut v = Self::constant_deps(v, deps);
        v.set_metadata(&Doc, TypedValue::constant(String::from(doc)));
        v
    }
}

/// Script map type.
#[derive(Clone, Debug, GreaseType, PartialEq, StableAbi)]
#[repr(C)]
pub struct Map(pub BstMap<Value, Value>);

impl Map {
    /// Create a new TypedValue<Map> from this Map.
    ///
    /// The context is used to generate default documentation for the value.
    pub fn into_typed_value(self, ctx: &grease::runtime::Context) -> TypedValue<Self> {
        let key_docs: Vec<_> = self.0.iter().map(|(k, _)| Doc::get(ctx, &k)).collect();
        let deps: grease::value::Dependencies = self.0.iter().map(|(k, v)| depends![*k, *v]).sum();
        let mut v = TypedValue::constant_deps(self, deps);
        v.set_metadata(
            &Doc,
            make_value!({
                let key_docs = futures::future::try_join_all(key_docs).await?;
                Ok(if key_docs.len() == 0 {
                    "map with no keys".into()
                } else {
                    let mut s = "map with keys:".to_owned();
                    for k in key_docs {
                        s.push_str(&format!("\n * {}", k.as_ref()));
                    }
                    s.into()
                })
            }),
        );
        v
    }

    /// Create a new TypedValue<Map> from this Map without documentation metadata.
    pub fn into_typed_value_no_doc(self) -> TypedValue<Self> {
        let deps: grease::value::Dependencies = self.0.iter().map(|(k, v)| depends![*k, *v]).sum();
        TypedValue::constant_deps(self, deps)
    }

    /// Create a new Value from this Map.
    ///
    /// The context is used to generate default documentation for the value.
    pub fn into_value(self, ctx: &grease::runtime::Context) -> Value {
        self.into_typed_value(ctx).into()
    }

    /// Create a new Value from this Map without documentation metadata.
    pub fn into_value_no_doc(self) -> Value {
        self.into_typed_value_no_doc().into()
    }
}

/// Script type for values that require a single value to produce a new value.
///
/// This is most importantly used to functions, where the input value is an `Args` or `PatternArgs`
/// value.
#[derive(GreaseType, StableAbi)]
#[repr(C)]
pub struct Unbound(UnboundAbi_TO<'static, RBox<()>>);

#[sabi_trait]
trait UnboundAbi: Send + Sync {
    #[sabi(last_prefix_field)]
    fn bind<'a>(
        &'a self,
        ctx: &'a mut crate::Runtime,
        arg: Source<Value>,
    ) -> BoxFuture<'a, crate::ResultAbi<Value>>;
}

impl<F> UnboundAbi for F
where
    F: for<'a> Fn(
            &'a mut crate::Runtime,
            Source<Value>,
        ) -> futures::future::BoxFuture<'a, crate::Result<Value>>
        + Send
        + Sync
        + 'static,
{
    fn bind<'a>(
        &'a self,
        ctx: &'a mut crate::Runtime,
        arg: Source<Value>,
    ) -> BoxFuture<'a, crate::ResultAbi<Value>> {
        BoxFuture::new(async move { self(ctx, arg).await.into() })
    }
}

impl Unbound {
    /// Create a new unbound value with the given implementation.
    pub fn new<F>(bind: F, deps: Dependencies, doc: Option<TypedValue<String>>) -> TypedValue<Self>
    where
        F: for<'a> Fn(
                &'a mut crate::Runtime,
                Source<Value>,
            ) -> futures::future::BoxFuture<'a, crate::Result<Value>>
            + Send
            + Sync
            + 'static,
    {
        let mut v =
            TypedValue::constant_deps(Unbound(UnboundAbi_TO::from_value(bind, TU_Opaque)), deps);
        if let Some(doc) = doc {
            v.set_metadata(&Doc, doc);
        }
        v
    }

    /// Bind the value.
    pub async fn bind(&self, ctx: &mut crate::Runtime, arg: Source<Value>) -> crate::Result<Value> {
        self.0.bind(ctx, arg).await.into()
    }
}

impl std::fmt::Debug for Unbound {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Unbound ({:p})", self.0.obj.sabi_erased_ref())
    }
}

/// A value to merge.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct Merge(pub Source<Value>);

impl From<Merge> for TypedValue<Merge> {
    fn from(v: Merge) -> Self {
        let deps = depends![*v.0];
        Self::constant_deps(v, deps)
    }
}

/// A key for maps to indicate to what to bind any remaining values.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct BindRest;

impl From<BindRest> for TypedValue<BindRest> {
    fn from(v: BindRest) -> Self {
        Self::constant_deps(v, depends![crate::namespace_id!(ergo::bind_rest)])
    }
}

/// Arguments in a function call.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct Args {
    pub args: crate::UncheckedArguments,
}

impl From<Args> for TypedValue<Args> {
    fn from(v: Args) -> Self {
        let deps: grease::value::Dependencies = (&v.args).into();
        Self::constant_deps(v, deps)
    }
}

/// Arguments in a bind function call.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct BindArgs {
    pub args: crate::UncheckedArguments,
}

impl From<BindArgs> for TypedValue<BindArgs> {
    fn from(v: BindArgs) -> Self {
        let deps: grease::value::Dependencies = (&v.args).into();
        Self::constant_deps(v, deps)
    }
}

/// The value in an index call.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct Index(pub Source<Value>);

impl From<Index> for TypedValue<Index> {
    fn from(v: Index) -> Self {
        let deps = depends![*v.0];
        Self::constant_deps(v, deps)
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
    ( independent $( $l:ident )::+, $doc:literal, $( $m:ident ),* |$ctx:ident, $args:ident| $body:expr ) => {
        $crate::types::Unbound::new(
            move |$ctx,__ergo_function_arg| {
                $( let $m = $m.clone(); )*
                $crate::future::FutureExt::boxed(
                    async move {
                        let mut $args: $crate::Arguments = {
                            let args = $crate::ContextExt::source_value_as::<$crate::types::Args>($ctx, __ergo_function_arg).await?.unwrap();
                            args.await?.owned().args.into()
                        };
                        Ok($body)
                    }
                )
            },
            ::grease::depends![$crate::namespace_id!($( $l )::+)],
            Some(::grease::value::TypedValue::constant($doc.into()))
        )
    };

    ( $( $l:ident )::+, $doc:literal, $( $m:ident ),* |$ctx:ident, $args:ident| $body:expr ) => {
        $crate::ergo_function!(independent $( $l )::+, $doc, $( $m ),* |$ctx,$args| {
            let val: ::grease::value::Value = $body;
            let new_deps = ::grease::depends![$crate::namespace_id!($( $l )::+), val.id()];
            val.set_dependencies(new_deps)
        })
    };
}
