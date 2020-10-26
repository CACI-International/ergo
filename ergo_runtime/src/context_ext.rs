//! Grease context extensions.

use crate::source::Source;
use futures::future::{BoxFuture, FutureExt};
use grease::{
    runtime::Context,
    types::{GreaseType, Type},
    value::{TypedValue, Value},
    Result, StableAbi,
};

pub trait AsContext {
    fn as_context(&self) -> &Context;
}

impl AsContext for Context {
    fn as_context(&self) -> &Context {
        self
    }
}

pub trait ContextExt: AsContext {
    fn type_name<'a>(&'a self, tp: &'a Type) -> BoxFuture<'a, Result<String>> {
        crate::traits::type_name(self.as_context(), tp).boxed()
    }

    fn display<'a>(&'a self, v: Value) -> BoxFuture<'a, Result<String>> {
        crate::traits::display(self.as_context(), v).boxed()
    }

    fn into<'a, T: GreaseType + StableAbi + Send + Sync + 'static>(
        &'a self,
        v: Value,
    ) -> BoxFuture<'a, Result<TypedValue<T>>> {
        crate::traits::into(self.as_context(), v).boxed()
    }

    fn into_sourced<'a, T: GreaseType + StableAbi + Send + Sync + 'static>(
        &'a self,
        v: Source<Value>,
    ) -> BoxFuture<'a, Result<Source<TypedValue<T>>>> {
        crate::traits::into_sourced(self.as_context(), v).boxed()
    }

    fn source_value_as<T: GreaseType + 'static>(
        &self,
        v: Source<Value>,
    ) -> BoxFuture<'static, Result<Source<TypedValue<T>>>> {
        let (source, v) = v.take();
        let s = source.clone();
        let ctx = self.as_context().clone();
        v.typed::<T, _, _>(move |t| {
            let ctx = ctx.clone();
            let t = t.clone();
            async move {
                let expected = match ctx.type_name(&T::grease_type()).await {
                    Err(e) => return e,
                    Ok(v) => v,
                };
                let found = match ctx.type_name(&t).await {
                    Err(e) => return e,
                    Ok(v) => v,
                };
                s.with(format!("expected {}, found {}", expected, found))
                    .into_grease_error()
            }
        })
        .map(move |r| r.map(move |v| source.with(v)))
        .boxed()
    }

    fn force_value_nested<'a>(&'a self, v: Value) -> BoxFuture<'a, Result<()>> {
        crate::traits::force_value_nested(self.as_context(), v).boxed()
    }

    fn read_from_store<'a>(
        &'a self,
        store_item: &'a grease::runtime::Item,
        id: u128,
    ) -> BoxFuture<'a, Result<Value>> {
        crate::traits::read_from_store(self.as_context(), store_item, id).boxed()
    }

    fn write_to_store<'a>(
        &'a self,
        store_item: &'a grease::runtime::Item,
        v: Value,
    ) -> BoxFuture<'a, Result<()>> {
        crate::traits::write_to_store(self.as_context(), store_item, v).boxed()
    }

    fn value_by_content<'a>(&'a self, v: Value) -> BoxFuture<'a, Result<Value>> {
        crate::traits::value_by_content(self.as_context(), v).boxed()
    }
}

impl<T: AsContext> ContextExt for T {}
