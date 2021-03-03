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

    fn display<'a>(
        &'a self,
        v: Value,
        f: &'a mut crate::traits::Formatter<'_>,
    ) -> BoxFuture<'a, Result<()>> {
        crate::traits::display(self.as_context(), v, f).boxed()
    }

    fn into_typed<'a, T: GreaseType + StableAbi + Send + Sync + 'static>(
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

    fn source_value_as_map_error<T: GreaseType + 'static, E>(
        &self,
        v: Source<Value>,
        map_error: E,
    ) -> BoxFuture<'static, Result<Source<TypedValue<T>>>>
    where
        E: FnOnce(grease::Error) -> grease::Error + Send + 'static,
    {
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
                map_error(
                    s.with(format!("expected {}, found {}", expected, found))
                        .into_grease_error(),
                )
            }
        })
        .map(move |r| r.map(move |v| source.with(v)))
        .boxed()
    }

    fn source_value_as_type_map_error<E>(
        &self,
        v: Source<Value>,
        target_tp: Type,
        map_error: E,
    ) -> BoxFuture<'static, Result<Source<Value>>>
    where
        E: FnOnce(grease::Error) -> grease::Error + Send + 'static,
    {
        let (source, v) = v.take();
        let s = source.clone();
        let ctx = self.as_context().clone();
        v.as_type(target_tp.clone(), move |t| {
            let ctx = ctx.clone();
            let t = t.clone();
            async move {
                let expected = match ctx.type_name(&target_tp).await {
                    Err(e) => return e,
                    Ok(v) => v,
                };
                let found = match ctx.type_name(&t).await {
                    Err(e) => return e,
                    Ok(v) => v,
                };
                map_error(
                    s.with(format!("expected {}, found {}", expected, found))
                        .into_grease_error(),
                )
            }
        })
        .map(move |r| r.map(move |v| source.with(v)))
        .boxed()
    }

    fn source_pattern_value_as<T: GreaseType + 'static>(
        &self,
        v: Source<Value>,
    ) -> BoxFuture<'static, Result<Source<TypedValue<T>>>> {
        // Any error that immediately occurs should be considered a
        // pattern error. The error that may occur later (in the
        // case of a dynamic type) should not be considered a
        // pattern error.
        self.source_value_as::<T>(v)
            .map(|res| res.map_err(crate::error::PatternError::wrap))
            .boxed()
    }

    fn source_pattern_value_as_type(
        &self,
        v: Source<Value>,
        target_tp: Type,
    ) -> BoxFuture<'static, Result<Source<Value>>> {
        // Any error that immediately occurs should be considered a
        // pattern error. The error that may occur later (in the
        // case of a dynamic type) should not be considered a
        // pattern error.
        self.source_value_as_type(v, target_tp)
            .map(|res| res.map_err(crate::error::PatternError::wrap))
            .boxed()
    }

    fn source_value_as<T: GreaseType + 'static>(
        &self,
        v: Source<Value>,
    ) -> BoxFuture<'static, Result<Source<TypedValue<T>>>> {
        self.source_value_as_map_error(v, std::convert::identity)
    }

    fn source_value_as_type(
        &self,
        v: Source<Value>,
        target_tp: Type,
    ) -> BoxFuture<'static, Result<Source<Value>>> {
        self.source_value_as_type_map_error(v, target_tp, std::convert::identity)
    }

    fn source_value_as_immediate<T: GreaseType + 'static>(
        &self,
        v: Source<Value>,
    ) -> BoxFuture<'static, Result<Source<TypedValue<T>>>> {
        let (source, v) = v.take();
        let s = source.clone();
        let ctx = self.as_context().clone();
        v.typed_immediate::<T, _, _>(move |t| {
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

    fn present_in_store(&self, store_item: &grease::runtime::Item, id: u128) -> bool {
        crate::traits::present_in_store(self.as_context(), store_item, id)
    }

    fn write_to_store<'a>(
        &'a self,
        store_item: &'a grease::runtime::Item,
        v: Value,
    ) -> BoxFuture<'a, Result<()>> {
        crate::traits::write_to_store(self.as_context(), store_item, v).boxed()
    }

    fn value_by_content<'a>(&'a self, v: Value, deep: bool) -> BoxFuture<'a, Result<Value>> {
        crate::traits::value_by_content(self.as_context(), v, deep).boxed()
    }
}

impl<T: AsContext> ContextExt for T {}
