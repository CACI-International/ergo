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
    std_types::{RBox, ROption, RString, RVec},
    StableAbi,
};
use grease::bst::BstMap;
use grease::depends;
use grease::future::BoxFuture;
use grease::make_value;
use grease::types::GreaseType;
use grease::value::{Dependencies, TypedValue, Value};

pub(crate) mod byte_stream;
pub(crate) mod shared_iter;
pub(crate) mod shared_stream;

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

/// Arguments in a pattern function call.
#[derive(Clone, GreaseType, StableAbi)]
#[repr(C)]
pub struct PatternArgs {
    pub args: crate::UncheckedArguments,
}

impl From<PatternArgs> for TypedValue<PatternArgs> {
    fn from(v: PatternArgs) -> Self {
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

/// The type indicating a value is unset.
#[derive(Clone, GreaseType)]
pub enum Unset {}

impl Unset {
    /// Create a new Unset value (which, when evaluated, will error).
    pub fn new() -> TypedValue<Self> {
        let mut v: TypedValue<Self> = make_value!(Err("unset value".into()));
        v.set_metadata(&Doc, make_value!(Ok("<unset>".into())));
        v
    }

    /// Return whether the given value is unset.
    pub fn is_unset(v: &Value) -> bool {
        v.grease_type_immediate() == Some(&Self::grease_type())
    }
}

/// A stream type.
///
/// This differs from `Iterator` in that you can partially consume streams and retain correct
/// dependency tracking.
#[derive(Clone, Debug, GreaseType, StableAbi)]
#[repr(C)]
pub struct Stream(StreamInner);

pub trait StreamInterface: Clone + std::fmt::Debug + Send + Sync {
    fn next(self) -> Option<(Value, TypedValue<Stream>)>;

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

impl<I: StreamInterface> StreamAbi for I {
    fn next(self) -> ROption<abi_stable::std_types::Tuple2<Value, TypedValue<Stream>>> {
        StreamInterface::next(self).map(|v| v.into()).into()
    }

    fn size_hint(&self) -> abi_stable::std_types::Tuple2<usize, ROption<usize>> {
        let (min, max) = StreamInterface::size_hint(self);
        (min, max.into()).into()
    }
}

impl Stream {
    /// Create a new Stream.
    pub fn new<I: StreamInterface + 'static>(interface: I) -> Self {
        Stream(StreamInner::Inner(StreamAbi_TO::from_value(
            interface, TU_Opaque,
        )))
    }
}

#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
enum StreamInner {
    Inner(StreamAbi_TO<'static, RBox<()>>),
    Pending(Value, TypedValue<Stream>),
    Empty,
}

#[sabi_trait]
trait StreamAbi: Clone + Debug + Send + Sync {
    fn next(self) -> ROption<abi_stable::std_types::Tuple2<Value, TypedValue<Stream>>>;

    #[sabi(last_prefix_field)]
    fn size_hint(&self) -> abi_stable::std_types::Tuple2<usize, ROption<usize>> {
        (0, ROption::RNone).into()
    }
}

impl StreamInner {
    fn take(&mut self) -> Self {
        std::mem::replace(self, StreamInner::Empty)
    }
}

impl futures::stream::Stream for Stream {
    type Item = crate::Result<Value>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context,
    ) -> std::task::Poll<Option<Self::Item>> {
        let me = &mut *self;
        loop {
            match me.0.take() {
                StreamInner::Inner(s) => match s.next().into_option() {
                    None => break std::task::Poll::Ready(None),
                    Some(v) => {
                        let (a, b) = v.into_tuple();
                        me.0 = StreamInner::Pending(a, b);
                    }
                },
                StreamInner::Pending(v, mut next) => {
                    match futures::Future::poll(std::pin::Pin::new(&mut next), cx) {
                        std::task::Poll::Pending => {
                            me.0 = StreamInner::Pending(v, next);
                            break std::task::Poll::Pending;
                        }
                        std::task::Poll::Ready(r) => match r {
                            Err(e) => break std::task::Poll::Ready(Some(Err(e))),
                            Ok(new_me) => {
                                *me = new_me.owned();
                                break std::task::Poll::Ready(Some(Ok(v)));
                            }
                        },
                    }
                }
                StreamInner::Empty => break std::task::Poll::Ready(None),
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            StreamInner::Inner(i) => {
                let (min, max) = i.size_hint().into_tuple();
                (min, max.into_option())
            }
            StreamInner::Empty => (0, Some(0)),
            _ => (0, None),
        }
    }
}

/// An iterator type.
///
/// Note that iterator types are meant to be fully consumed wherever they are used. Changing one
/// partially and creating a new Value may not correctly capture dependencies; for a partial
/// consumption case, use `Stream`.
#[derive(Clone, Debug, GreaseType, StableAbi)]
#[repr(C)]
pub struct Iter(shared_iter::SharedIterator<IterWrapper>);

#[sabi_trait]
trait IterAbi: Send + Sync {
    fn next(&mut self) -> ROption<Value>;

    #[sabi(last_prefix_field)]
    fn size_hint(&self) -> abi_stable::std_types::Tuple2<usize, ROption<usize>> {
        (0, ROption::RNone).into()
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct IterWrapper(IterAbi_TO<'static, RBox<()>>);

impl Iterator for IterWrapper {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().into_option()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (min, max) = self.0.size_hint().into_tuple();
        (min, max.into_option())
    }
}

impl Iterator for Iter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        Iterator::next(&mut self.0)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        Iterator::size_hint(&self.0)
    }
}

impl<I> IterAbi for I
where
    I: Iterator<Item = Value> + Send + Sync,
{
    fn next(&mut self) -> ROption<Value> {
        Iterator::next(self).into()
    }

    fn size_hint(&self) -> abi_stable::std_types::Tuple2<usize, ROption<usize>> {
        let (min, max) = Iterator::size_hint(self);
        (min, max.into()).into()
    }
}

impl Iter {
    /// Create a new Iter value with the given dependencies.
    pub fn new<I>(iter: I, deps: Dependencies) -> TypedValue<Self>
    where
        I: Iterator<Item = Value> + Send + Sync + 'static,
    {
        TypedValue::constant_deps(Self::from_iter(iter), deps)
    }

    /// Create an Iter from an iterator.
    pub fn from_iter<I>(iter: I) -> Self
    where
        I: Iterator<Item = Value> + Send + Sync + 'static,
    {
        Iter(shared_iter::SharedIterator::new(IterWrapper(
            IterAbi_TO::from_value(iter, TU_Opaque),
        )))
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
                        let mut __ergo_function_args: $crate::Arguments = {
                            let args = $crate::ContextExt::source_value_as::<$crate::types::Args>($ctx, __ergo_function_arg).await?.unwrap();
                            args.await?.owned().args.into()
                        };
                        let $args = &mut __ergo_function_args;
                        match async move { Ok($body) }.await {
                            Ok(v) => {
                                __ergo_function_args.unused_arguments()?;
                                Ok(v)
                            }
                            Err(e) => {
                                __ergo_function_args.unchecked();
                                Err(e)
                            }
                        }
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
