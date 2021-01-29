//! The grease Bind trait for setting values.

use super::type_name;
use crate::{source::IntoSource, types, ContextExt, EvalResult, Runtime, Source};
use abi_stable::{
    std_types::{ROption, RVec},
    StableAbi,
};
use grease::{
    bst::BstMap,
    closure::FnPtr,
    depends, grease_trait, grease_traits_fn, match_value,
    runtime::Traits,
    traits::{GreaseTrait, Trait},
    type_erase::Erased,
    types::Type,
    value::IntoValue,
    Value,
};
use std::str::FromStr;

/// The Bind grease trait.
#[grease_trait]
pub trait Bind {
    async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value;
}

/// Create a bind error result for the given value.
pub async fn bind_error<T>(ctx: &grease::runtime::Context, v: Source<Value>) -> crate::Result<T> {
    let (src, v) = v.take();
    let name = type_name(ctx, v.grease_type().await?).await?;
    Err(src
        .with(format!("cannot bind to value with type '{}'", name))
        .into_grease_error())
}

/// Bind a value to an argument.
pub async fn bind(ctx: &mut Runtime, v: Source<Value>, arg: Source<Value>) -> EvalResult {
    delay_bind(ctx, v).await?.bind(arg).await
}

/// A bind which may be performed later.
#[derive(Clone)]
pub struct DelayedBind {
    trt: Bind,
    ctx: Runtime,
    pub value: Source<Value>,
}

impl DelayedBind {
    /// Perform the bind.
    pub async fn bind(&self, arg: Source<Value>) -> EvalResult {
        let DelayedBind {
            mut trt,
            mut ctx,
            value,
        } = self.clone();

        let call_site = (self.value.source(), arg.source()).into_source();

        // Eagerly evaluate binding
        Ok(call_site.with(
            value
                .map_async(move |v| {
                    let deps = depends![{ crate::namespace_id!(ergo::bind) }, *arg];
                    v.and_then_value(
                        move |v| async move { trt.bind(v, &mut ctx, arg).await },
                        deps,
                    )
                })
                .await
                .transpose_err()
                .map_err(|e| {
                    let (src, e) = e.take();
                    src.with("while binding/calling value").context_for_error(e)
                })?,
        ))
    }
}

/// Create a delayed bind.
pub async fn delay_bind(ctx: &mut Runtime, v: Source<Value>) -> crate::Result<DelayedBind> {
    let t_ctx = ctx.clone();
    let trt = ctx
        .get_trait::<Bind, _, _>(&v, move |t| {
            let t = t.clone();
            let ctx = t_ctx.clone();
            async move {
                let name = type_name(&ctx, &t).await?;
                Err(format!("cannot bind value with type '{}'", name).into())
            }
        })
        .await?;
    Ok(DelayedBind {
        trt,
        ctx: ctx.empty(),
        value: v,
    })
}

grease_traits_fn! {
    // Blanket implementation with id equality.
    // Sufficient for unit, string, and others.
    {
        extern "C" fn id_eq_f<'a>(
            _trait_data: &'a grease::type_erase::Erased,
            ctx: &'a grease::runtime::Context,
            v: &'a Value,
            tp: &'a Type,
            _data: &'a Erased,
            _rt: &'a mut Runtime,
            arg: Source<Value>) ->
            grease::future::BoxFuture<'a, grease::error::RResult<Value>> {
            grease::future::BoxFuture::new(async move {
                if v.id() != arg.id() {
                    let (arg_source, arg) = arg.take();
                    match_value!(peek arg => {
                        types::Args => |_| {
                            let name = type_name(ctx, tp).await?;
                            Err(arg_source.with(format!("cannot call value with type {}", name)).into_grease_error())?
                        }
                        types::BindArgs => |_| {
                            let name = type_name(ctx, tp).await?;
                            Err(arg_source.with(format!("cannot call value in binding with type {}", name)).into_grease_error())?
                        }
                        => |_| {
                            Err(arg_source.with("mismatched value in binding").into_grease_error())?
                        }
                    }).await.into()
                } else {
                    grease::error::RResult::ROk(types::Unit.into_value())
                }
            })
        }
        extern "C" fn id_eq(_traits: &Traits, _tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == Bind::grease_trait().id {
                ROption::RSome(Erased::new(BindImpl {
                    bind: unsafe { FnPtr::new(id_eq_f) },
                    grease_trait_data: Default::default(),
                }))
            } else {
                ROption::RNone
            }
        }
        unsafe { traits.add_generator(id_eq) };
    }

    async fn bind_array(
        ctx: &mut Runtime,
        to: RVec<Source<Value>>,
        from: Source<RVec<Source<Value>>>,
    ) -> grease::Result<()> {
        use futures::future::BoxFuture;
        use futures::FutureExt;

        type Iter = abi_stable::std_types::vec::IntoIter<Source<Value>>;

        fn back<'a>(ctx: &'a mut Runtime, to: &'a mut Iter, from: &'a mut Iter) -> BoxFuture<'a, grease::Result<()>> {
            async move {
                while let Some(t) = to.next_back() {
                    let (t_source, t) = t.take();
                    match_value!(t => {
                        types::Merge => |merge| {
                            let merge_val = merge.await?.0.clone();
                            match to.next_back() {
                                None => Err(t_source.with("undecidable merge").into_grease_error())?,
                                Some(to_v) => {
                                    // Keep taking until we find a match
                                    let mut vals = Vec::new();
                                    loop {
                                        match from.next_back() {
                                            Some(from_v) => {
                                                if bind(ctx, to_v.clone(), from_v.clone()).await.is_ok() {
                                                    break;
                                                } else {
                                                    vals.push(from_v);
                                                }
                                            }
                                            None => Err(to_v.source().with("no value matches this binding").into_grease_error())?
                                        }
                                    }
                                    // Values were pushed in reverse order
                                    let val_array = vals.into_source()
                                        .map(|f| types::Array(f.into_iter().rev().map(Source::unwrap).collect())
                                                    .into_value());
                                    bind(ctx, merge_val, val_array).await?;
                                }
                            }
                        }
                        => |to_v| {
                            match from.next_back() {
                                None => Err(t_source.with("no value matches this binding").into_grease_error())?,
                                Some(from_v) => {
                                    bind(ctx, t_source.with(to_v), from_v).await?;
                                }
                            }
                        }
                    }).await?;
                }

                Ok(())
            }.boxed()
        }

        fn forward<'a>(ctx: &'a mut Runtime, to: &'a mut Iter, from: &'a mut Iter) -> BoxFuture<'a, grease::Result<()>>
        {
            async move {
                while let Some(t) = to.next() {
                    let (t_source, t) = t.take();
                    match_value!(t => {
                        types::Merge => |merge| {
                            let merge_val = merge.await?.0.clone();
                            back(ctx, to, from).await?;
                            bind(ctx, merge_val, from.collect::<Vec<_>>()
                                .into_source()
                                .map(|f| types::Array(f.into_iter().map(Source::unwrap).collect()).into_value())
                            ).await?;
                        }
                        => |to_v| {
                            match from.next() {
                                None => Err(t_source.with("no value matches this binding").into_grease_error())?,
                                Some(from_v) => {
                                    bind(ctx, t_source.with(to_v), from_v).await?;
                                }
                            }
                        }
                    }).await?;
                }

                let remaining: RVec<_> = from.collect();
                if !remaining.is_empty() {
                    let mut errs = Vec::new();
                    for v in remaining {
                        errs.push(v.with("no binding matches this value").into_grease_error());
                    }
                    Err(grease::Error::aggregate(errs))
                } else {
                    Ok(())
                }
            }.boxed()
        }

        let from = from.unwrap();
        let mut to = to.into_iter();
        let mut from = from.into_iter();

        forward(ctx, &mut to, &mut from).await
    }

    impl Bind for types::Array {
        async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value {
            let (source, arg) = arg.take();

            match_value!(arg => {
                types::Index => |ind| {
                    let ind = ind.await?.owned().0;

                    // Return value at index
                    let (ind_source, ind) = ctx.source_value_as::<types::String>(ind).await?.await.transpose_ok()?.take();
                    source.with(match usize::from_str(ind.as_ref()) {
                        Err(_) => Err(ind_source.with("non-integer index").into_grease_error()),
                        Ok(ind) => self.0.get(ind).cloned().ok_or_else(|| format!("array has length {}", self.0.len()).into())
                    })
                    .transpose_err_with_context("while indexing array")?
                },
                types::Array => |arr| {
                    let arr = arr.await?;
                    bind_array(ctx, self.0.iter().map(|v| Source::builtin(v.clone())).collect(),
                        source.clone().with(
                            arr.0.iter().map(|v| source.clone().with(v.clone())).collect()
                        )
                    ).await?;
                    types::Unit.into_value()
                }
                => |v| bind_error(ctx, source.with(v)).await?
            }).await?
        }
    }

    async fn bind_map(
        ctx: &mut Runtime,
        mut to: BstMap<Source<Value>, Source<Value>>,
        from: Source<BstMap<Source<Value>, Source<Value>>>,
    ) -> grease::Result<()> {
        let rest = to.remove(&types::BindRest.into_value());
        let (from_source, mut from) = from.take();
        for (k,to_v) in to.into_iter() {
            bind(ctx, to_v, if let Some(from_v) = from.remove(&k) {
                from_v
            } else {
                // If `from` is missing a `to` key, use `Unset`.
                let mut v: Value = types::Unset::new().into();
                v = from_source.clone().with("bound map with missing key").imbue_error_context(v);
                v = k.source().with("missing key").imbue_error_context(v);
                from_source.clone().with(v)
            }).await?;
        }

        match rest {
            None => {
                if from.is_empty() {
                    Ok(())
                } else {
                    let mut errs = Vec::new();
                    for (k,_) in from.into_iter() {
                        errs.push(k.with("key missing in binding").into_grease_error());
                    }
                    Err(grease::Error::aggregate(errs))
                }
            }
            Some(v) => {
                let remaining = from.into_iter().map(|(k,v)| (k.unwrap(), v.unwrap())).collect::<BstMap<_,_>>();
                bind(ctx, v, from_source.with(types::Map(remaining).into_value(ctx))).await?;
                Ok(())
            }
        }
    }

    impl Bind for types::Map {
        async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value {
            let (source, arg) = arg.take();

            match_value!(arg => {
                types::Index => |ind| {
                    let index = ind.await?.owned().0.unwrap();

                    match self.0.get(&index).cloned() {
                        Some(v) => v,
                        None => source.with("map key does not exist").imbue_error_context(types::Unset::new().into()),
                    }
                },
                types::Map => |map| {
                    let map = map.await?;
                    bind_map(ctx, self.0.iter().map(|(k,v)| (Source::builtin(k.clone()), Source::builtin(v.clone()))).collect(),
                        source.clone().with(
                            map.0.iter()
                                .map(|(k,v)| (source.clone().with(k.clone()),source.clone().with(v.clone())))
                                .collect()
                        )
                    ).await?;
                    types::Unit.into_value()
                },
                => |v| bind_error(ctx, source.with(v)).await?
            }).await?
        }
    }

    async fn bind_args(ctx: &mut Runtime, to: &crate::UncheckedArguments, from: Source<crate::UncheckedArguments>) -> grease::Result<()> {
        let (from_source, from) = from.take();
        // Positional arguments are stored in reverse order, but it's easiest to match them
        // in-order (especially wrt merge values).
        bind_array(ctx,
            to.positional.iter().rev().cloned().collect(),
            from_source.clone().with(from.positional.into_iter().rev().collect())
        ).await?;
        bind_map(ctx, to.non_positional.clone(), from_source.with(from.non_positional)).await?;
        Ok(())
    }

    impl Bind for types::Args {
        async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value {
            let (src, args) = ctx.source_value_as::<types::Args>(arg).await?.take();
            let args = args.await?.owned().args;
            bind_args(ctx, &self.args, src.with(args)).await?;
            types::Unit.into_value()
        }
    }

    impl Bind for types::BindArgs {
        async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value {
            let (src, args) = ctx.source_value_as::<types::BindArgs>(arg).await?.take();
            let args = args.await?.owned().args;
            bind_args(ctx, &self.args, src.with(args)).await?;
            types::Unit.into_value()
        }
    }

    impl Bind for types::Index {
        async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value {
            let ind = ctx.source_value_as::<types::Index>(arg).await?.await.transpose_ok()?.unwrap().owned().0;
            crate::traits::bind(ctx, self.0.clone(), ind).await?;
            types::Unit.into_value()
        }
    }

    impl Bind for types::Unbound {
        async fn bind(&self, ctx: &mut Runtime, arg: Source<Value>) -> Value {
            self.bind(ctx, arg).await?
        }
    }
}
