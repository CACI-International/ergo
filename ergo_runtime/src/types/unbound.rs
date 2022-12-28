//! The Unbound type.

use crate as ergo_runtime;
use crate::abi_stable::{
    future::BoxFuture, sabi_trait, sabi_trait::prelude::*, std_types::RBox, u128::U128, StableAbi,
};
use crate::traits;
use crate::type_system::{ergo_traits_fn, ErgoType};
use crate::value::{lazy::LazyCaptures, IdInfo, LateBindContext, LateBound, ValueId};
use crate::Value;

/// Script type for values that require a single value to produce a new value.
///
/// This is most importantly used in functions, where the input value is an `Args` or `PatternArgs`
/// value.
#[derive(Clone, ErgoType, StableAbi)]
#[repr(C)]
pub struct Unbound(UnboundInterface_TO<RBox<()>>);

#[sabi_trait]
pub trait UnboundInterface: Clone + Send + Sync + 'static {
    fn id(&self) -> BoxFuture<IdInfo<U128>>;

    fn late_bind(&mut self, context: &mut LateBindContext);

    fn late_bound(&self) -> LateBound;

    fn gc_refs(&self, v: &mut crate::gc::Visitor);

    #[sabi(last_prefix_field)]
    fn bind(&self, arg: Value) -> BoxFuture<Value>;
}

impl crate::value::ValueDataInterface for Unbound {
    fn id(&self) -> BoxFuture<IdInfo<U128>> {
        self.0.id()
    }

    fn late_bind(&mut self, context: &mut LateBindContext) {
        self.0.late_bind(context);
    }

    fn late_bound(&self) -> LateBound {
        self.0.late_bound()
    }

    fn gc_refs(&self, v: &mut crate::gc::Visitor) {
        self.0.gc_refs(v);
    }

    fn get(&self) -> crate::value::ValueType {
        crate::value::ValueType::typed(self)
    }
}

#[derive(Clone)]
pub struct UnboundFn<Captures, F, const USE_CAPTURES_IN_ID: bool = true> {
    id: ValueId,
    captures: Captures,
    f: F,
}

impl<const U: bool, Captures, F> UnboundFn<Captures, F, U> {
    pub fn new(id: impl Into<ValueId>, captures: Captures, f: F) -> Self {
        UnboundFn {
            id: id.into(),
            captures,
            f,
        }
    }
}

impl<const USE_CAPS: bool, Captures, F, Fut> UnboundInterface for UnboundFn<Captures, F, USE_CAPS>
where
    Self: Clone + Send + Sync + 'static,
    Captures: LazyCaptures,
    F: FnOnce(Captures, Value) -> Fut + Clone,
    Fut: std::future::Future<Output = Value> + Send + 'static,
{
    fn id(&self) -> BoxFuture<IdInfo<U128>> {
        BoxFuture::new(async move {
            if USE_CAPS {
                let mut combiner = crate::value::IdentityCombiner::default();
                combiner.add(&self.id.id().await);
                combiner.add(&self.captures.id().await);
                combiner.finish().into()
            } else {
                self.id.id().await.into()
            }
        })
    }

    fn late_bind(&mut self, context: &mut LateBindContext) {
        self.id.late_bind(context);
        self.captures.late_bind(context);
    }

    fn late_bound(&self) -> LateBound {
        let mut s = self.id.late_bound();
        s.extend(self.captures.late_bound());
        s
    }

    fn gc_refs(&self, v: &mut crate::gc::Visitor) {
        use crate::gc::GcRefs;
        self.id.gc_refs(v);
        self.captures.gc_refs(v);
    }

    fn bind(&self, arg: Value) -> BoxFuture<Value> {
        BoxFuture::new((self.f.clone())(self.captures.clone(), arg))
    }
}

impl Unbound {
    /// Create a new unbound value with the given interface.
    pub fn new<T: UnboundInterface + 'static>(interface: T) -> Self {
        Unbound(UnboundInterface_TO::from_value(interface, TD_Opaque))
    }

    /// Bind the value.
    pub async fn bind(&self, arg: Value) -> Value {
        self.0.bind(arg).await.into()
    }
}

impl std::fmt::Debug for Unbound {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Unbound ({:p})", self.0.obj.sabi_erased_ref().get())
    }
}

ergo_traits_fn! {
    crate::ergo_type_name!(traits, Unbound);

    impl traits::Bind for Unbound {
        async fn bind(&self, arg: Value) -> Value {
            self.bind(arg).await
        }
    }
}
