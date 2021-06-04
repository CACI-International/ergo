//! The Unbound type.

use crate as ergo_runtime;
use crate::abi_stable::{
    future::BoxFuture, sabi_trait, sabi_trait::prelude::*, std_types::RBox, StableAbi,
};
use crate::metadata::Doc;
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::value::TypedValue;
use crate::{Dependencies, Source, Value};

/// Script type for values that require a single value to produce a new value.
///
/// This is most importantly used in functions, where the input value is an `Args` or `PatternArgs`
/// value.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct Unbound(UnboundAbi_TO<'static, RBox<()>>);

#[sabi_trait]
trait UnboundAbi: Clone + Send + Sync {
    #[sabi(last_prefix_field)]
    fn bind<'a>(&'a self, ctx: &'a crate::Context, arg: Source<Value>) -> BoxFuture<'a, Value>;
}

impl<F> UnboundAbi for F
where
    F: for<'a> Fn(&'a crate::Context, Source<Value>) -> futures::future::BoxFuture<'a, Value>
        + Clone
        + Send
        + Sync
        + 'static,
{
    fn bind<'a>(&'a self, ctx: &'a crate::Context, arg: Source<Value>) -> BoxFuture<'a, Value> {
        BoxFuture::new(async move { self(ctx, arg).await.into() })
    }
}

impl Unbound {
    /// Create a new unbound value with the given implementation and documentation.
    pub fn new<F, S: Into<String>>(bind: F, deps: Dependencies, doc: S) -> TypedValue<Self>
    where
        F: for<'a> Fn(&'a crate::Context, Source<Value>) -> futures::future::BoxFuture<'a, Value>
            + Clone
            + Send
            + Sync
            + 'static,
    {
        let mut v = Self::new_no_doc(bind, deps);
        Doc::set_string(&mut v, doc.into());
        v
    }

    /// Create a new unbound value with the given implementation.
    pub fn new_no_doc<F>(bind: F, deps: Dependencies) -> TypedValue<Self>
    where
        F: for<'a> Fn(&'a crate::Context, Source<Value>) -> futures::future::BoxFuture<'a, Value>
            + Clone
            + Send
            + Sync
            + 'static,
    {
        TypedValue::constant_deps(
            Unbound(UnboundAbi_TO::from_value(bind, TU_Opaque)),
            crate::depends![Self::ergo_type(), ^deps],
        )
    }

    /// Bind the value.
    pub async fn bind(&self, ctx: &crate::Context, arg: Source<Value>) -> Value {
        self.0.bind(ctx, arg).await.into()
    }
}

impl std::fmt::Debug for Unbound {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Unbound ({:p})", self.0.obj.sabi_erased_ref())
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, Unbound);

    impl traits::Bind for Unbound {
        async fn bind(&self, arg: Source<Value>) -> Value {
            self.bind(CONTEXT, arg).await
        }
    }
}
