//! The grease Call trait for commands.

use super::{display, type_name};
use crate::{
    source::IntoSource, types, ContextExt, EvalResult, FunctionCall, Runtime, Source,
    UncheckedFunctionArguments,
};
use abi_stable::StableAbi;
use grease::{depends, grease_trait, grease_traits_fn, Value};
use std::str::FromStr;

/// The Call grease trait.
#[grease_trait]
pub trait Call {
    async fn call(&self, ctx: &mut FunctionCall<'_>) -> Value;
}

/// Call a value with arguments.
pub async fn call(
    ctx: &mut Runtime,
    v: Source<Value>,
    args: UncheckedFunctionArguments,
) -> EvalResult {
    delay_call(ctx, v).await?.call(args).await
}

/// A call which may be performed later.
#[derive(Clone)]
pub struct DelayedCall {
    trt: Call,
    ctx: Runtime,
    pub value: Source<Value>,
}

impl DelayedCall {
    /// Perform the delayed call.
    pub async fn call(&self, mut args: UncheckedFunctionArguments) -> EvalResult {
        let DelayedCall {
            mut trt,
            mut ctx,
            value,
        } = self.clone();
        let pos_source = args.positional.as_slice().into_source().source();
        let (kw_source, non_pos) = std::mem::take(&mut args.non_positional)
            .into_source()
            .take();
        args.non_positional = non_pos;
        let call_site = ((value.source(), pos_source), kw_source)
            .into_source()
            .source();

        let fcall_site = call_site.clone();

        // Eagerly evaluate application
        Ok(call_site.with(
            value
                .map_async(|v| {
                    let deps = depends![{crate::namespace_id!(ergo::call)}, ^&args];
                    v.and_then_value(
                        move |v| async move {
                            let mut fcallctx = FunctionCall::new(&mut ctx, args.into(), fcall_site);
                            let ret = trt.call(v, &mut fcallctx).await;
                            if ret.is_err() {
                                fcallctx.args.clear();
                            } else {
                                fcallctx.unused_arguments()?;
                            }
                            ret
                        },
                        deps,
                    )
                })
                .await
                .transpose_err_with_context("while calling value")?,
        ))
    }
}

/// Create a delayed call.
pub async fn delay_call(ctx: &mut Runtime, v: Source<Value>) -> grease::Result<DelayedCall> {
    let t_ctx = ctx.clone();
    let trt = ctx
        .get_trait::<Call, _, _>(&v, move |t| {
            let t = t.clone();
            let ctx = t_ctx.clone();
            async move {
                let name = type_name(&ctx, &t).await?;
                Err(format!("cannot call value with type '{}'", name).into())
            }
        })
        .await?;
    Ok(DelayedCall {
        trt,
        ctx: ctx.empty(),
        value: v,
    })
}

grease_traits_fn! {
    impl Call for types::Function {
        async fn call(&self, ctx: &mut FunctionCall<'_>) -> Value {
            // Lose source info from call, it will be replaced with call site
            self.call(ctx).await?.unwrap()
        }
    }

    impl Call for types::Array {
        async fn call(&self, ctx: &mut FunctionCall<'_>) -> Value {
            let ind = ctx.args.next().ok_or("no index provided")?;
            ctx.unused_arguments()?;

            let ind = ctx.source_value_as::<types::String>(ind).await?.await.transpose_ok()?;
            ind.map(|index| match usize::from_str(index.as_ref()) {
                Err(_) => Err(ctx.call_site.clone().with("non-integer index").into_grease_error()),
                Ok(ind) => self.0.get(ind).cloned().ok_or_else(|| format!("array index does not exist: {}", ind).into())
            })
            .transpose_err_with_context("while indexing array")?
        }
    }

    impl Call for types::Map {
        async fn call(&self, ctx: &mut FunctionCall<'_>) -> Value {
            let ind = ctx.args.next().ok_or("no key provided")?;
            ctx.unused_arguments()?;

            ind.map_async(|index| async move {
                match self.0.get(&index).cloned() {
                    Some(v) => Ok(v),
                    None => {
                        let d = display(CONTEXT, index).await?;
                        Err(grease::Error::from(format!("map index does not exist: {}", d)))
                    }
                }
            }).await.transpose_err_with_context("while indexing map")?
        }
    }
}
