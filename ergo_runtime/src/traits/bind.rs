//! The Bind ergo trait for setting values.

use super::{type_name, type_name_for};
use crate as ergo_runtime;
use crate::abi_stable::{
    bst::BstMap,
    closure::FnPtr,
    std_types::{ROption, RVec},
    type_erase::Erased,
};
use crate::type_system::{ergo_trait, ergo_traits_fn, ErgoTrait, Trait, Type};
use crate::{
    error::PatternError,
    source::IntoSource,
    types,
    value::{match_value, IntoValue},
    Context, Source, Value,
};

/// The Bind ergo trait.
#[ergo_trait]
pub trait Bind {
    async fn bind(&self, arg: Source<Value>) -> Value;
}

/// Create a bind error result for the given value.
///
/// The error is wrapped as a PatternError.
pub fn bind_error(ctx: &Context, v: Source<Value>) -> crate::Error {
    let (src, v) = v.take();
    let name = type_name(ctx, &v);
    PatternError::wrap(
        src.with(format!("cannot bind to value with type '{}'", name))
            .into_error(),
    )
}

/// Bind a value to an argument.
pub async fn bind(ctx: &Context, mut v: Source<Value>, arg: Source<Value>) -> Source<Value> {
    let call_site = (v.source(), arg.source()).into_source();

    ctx.eval(&mut v).await;
    call_site.with(match ctx.get_trait::<Bind>(&v) {
        None => bind_error(ctx, v).into(),
        Some(t) => t.bind(v.unwrap(), arg).await,
    })
}

async fn bind_no_error(ctx: &Context, v: Source<Value>, arg: Source<Value>) -> crate::Result<()> {
    let mut result = bind(ctx, v, arg).await.unwrap();
    ctx.eval(&mut result).await;
    crate::try_value!(result);
    Ok(())
}

/// create_bind_rest is used to create the value that is bound when a BindRest type is encountered.
/// The first argument is whether this is the first BindRest, and the second is the values to bind.
pub(crate) async fn bind_array<F>(
    ctx: &Context,
    to: RVec<Source<Value>>,
    from: Source<RVec<Source<Value>>>,
    mut create_bind_rest: F,
) -> crate::Result<()>
where
    F: FnMut(bool, Vec<Source<Value>>) -> Value + Send + Sync,
{
    use futures::future::BoxFuture;
    use futures::FutureExt;

    type Iter = crate::abi_stable::std_types::vec::IntoIter<Source<Value>>;

    fn back<'a, F>(
        ctx: &'a Context,
        to: &'a mut Iter,
        from: &'a mut Iter,
        create_bind_rest: &'a mut F,
    ) -> BoxFuture<'a, crate::Result<()>>
    where
        F: FnMut(bool, Vec<Source<Value>>) -> Value + Send + Sync,
    {
        async move {
            while let Some(t) = to.next_back() {
                let (t_source, t) = t.take();
                match_value!{t,
                    types::BindRest(rest) => {
                        match to.next_back() {
                            None => Err(PatternError::wrap(t_source.with("undecidable merge").into_error()))?,
                            Some(to_v) => {
                                // Keep taking until we find a match
                                let mut vals = Vec::new();
                                loop {
                                    match from.next_back() {
                                        Some(from_v) => {
                                            if bind(ctx, to_v.clone(), from_v.clone()).await.is_type::<types::Error>() {
                                                vals.push(from_v);
                                            } else {
                                                break;
                                            }
                                        }
                                        None => Err(PatternError::wrap(to_v.source().with("no value matches this binding").into_error()))?
                                    }
                                }
                                // Values were pushed in reverse order
                                vals.reverse();
                                let val_array = vals.into_source().map(|f| create_bind_rest(false, f));
                                bind_no_error(ctx, rest, val_array).await?;
                            }
                        }
                    }
                    to_v => {
                        match from.next_back() {
                            None => Err(PatternError::wrap(t_source.with("no value matches this binding").into_error()))?,
                            Some(from_v) => bind_no_error(ctx, t_source.with(to_v), from_v).await?,
                        }
                    }
                }
            }

            Ok(())
        }.boxed()
    }

    fn forward<'a, F>(
        ctx: &'a Context,
        to: &'a mut Iter,
        from: &'a mut Iter,
        create_bind_rest: &'a mut F,
    ) -> BoxFuture<'a, crate::Result<()>>
    where
        F: FnMut(bool, Vec<Source<Value>>) -> Value + Send + Sync,
    {
        async move {
                let mut first_rest = true;
                while let Some(t) = to.next() {
                    let (t_source, t) = t.take();
                    match_value!{t,
                        types::BindRest(rest) => {
                            back(ctx, to, from, create_bind_rest).await?;
                            bind_no_error(ctx, rest, from.collect::<Vec<_>>()
                                .into_source()
                                .map(|f| create_bind_rest(first_rest, f))
                            ).await?;
                            first_rest = false;
                        }
                        to_v => {
                            match from.next() {
                                None => Err(PatternError::wrap(t_source.with("no value matches this binding").into_error()))?,
                                Some(from_v) => {
                                    bind_no_error(ctx, t_source.with(to_v), from_v).await?;
                                }
                            }
                        }
                    }
                }

                let remaining: RVec<_> = from.collect();
                if !remaining.is_empty() {
                    let mut errs = Vec::new();
                    for v in remaining {
                        errs.push(PatternError::wrap(v.with("no binding matches this value").into_error()));
                    }
                    Err(crate::Error::aggregate(errs))
                } else {
                    Ok(())
                }
            }.boxed()
    }

    let from = from.unwrap();
    let mut to = to.into_iter();
    let mut from = from.into_iter();

    forward(ctx, &mut to, &mut from, &mut create_bind_rest).await
}

/// Returns the remaining items in `from` that were not bound if `return_rest` is true and
/// `BindRestKey` is not a key in `to`.
pub(crate) async fn bind_map(
    ctx: &Context,
    mut to: BstMap<Source<Value>, Source<Value>>,
    from: Source<BstMap<Source<Value>, Source<Value>>>,
    return_rest: bool,
) -> crate::Result<BstMap<Source<Value>, Source<Value>>> {
    let rest = to.remove(&types::BindRestKey.into_value());
    let (from_source, mut from) = from.take();
    for (k, to_v) in to.into_iter() {
        bind_no_error(
            ctx,
            to_v,
            if let Some(from_v) = from.remove(&k) {
                from_v
            } else {
                // If `from` is missing a `to` key, use `Unset`.
                let v: Value = types::Unset.into();
                from_source.clone().with(v)
            },
        )
        .await?;
    }

    match rest {
        None => {
            if return_rest {
                Ok(from)
            } else if from.is_empty() {
                Ok(Default::default())
            } else {
                let mut errs = Vec::new();
                for (k, _) in from.into_iter() {
                    errs.push(PatternError::wrap(
                        k.with("key missing in binding").into_error(),
                    ));
                }
                Err(crate::Error::aggregate(errs))
            }
        }
        Some(v) => {
            let remaining = from;
            bind_no_error(ctx, v, from_source.with(types::Map(remaining).into())).await?;
            Ok(Default::default())
        }
    }
}

ergo_traits_fn! {
    // Blanket implementation with id equality.
    // Sufficient for unit, string, and others.
    {
        extern "C" fn id_eq_f<'a>(
            _trait_data: &'a Erased,
            ctx: &'a Context,
            v: &'a Value,
            tp: &'a Type,
            _data: &'a Erased,
            arg: Source<Value>) ->
            crate::abi_stable::future::BoxFuture<'a, Value> {
            crate::abi_stable::future::BoxFuture::new(async move {
                if v.id() != arg.id() {
                    let (arg_source, arg) = arg.take();
                    match_value! { arg,
                        types::Args {..} => {
                            let name = type_name_for(ctx, tp);
                            arg_source.with(format!("cannot call value with type {}", name)).into_error().into()
                        }
                        types::PatternArgs {..} => {
                            let name = type_name_for(ctx, tp);
                            arg_source.with(format!("cannot call value in pattern with type {}", name)).into_error().into()
                        }
                        types::Index(_) => {
                            let name = type_name_for(ctx, tp);
                            arg_source.with(format!("cannot index value with type {}", name)).into_error().into()
                        }
                        _ => {
                            PatternError::wrap(arg_source.with("mismatched value in binding").into_error()).into()
                        }
                    }
                } else {
                    types::Unit.into()
                }
            })
        }
        extern "C" fn id_eq(_traits: &crate::context::Traits, _tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == Bind::ergo_trait().id {
                ROption::RSome(Erased::new(BindImpl {
                    bind: unsafe { FnPtr::new(id_eq_f) },
                    ergo_trait_data: Default::default(),
                }))
            } else {
                ROption::RNone
            }
        }
        unsafe { traits.add_generator(id_eq) };
    }
}
