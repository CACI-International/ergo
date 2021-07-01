//! The Bind ergo trait for setting values.

use super::{type_name, type_name_for};
use crate as ergo_runtime;
use crate::abi_stable::{
    bst::BstMap,
    closure::FnPtr,
    std_types::{ROption, RVec},
    type_erase::Erased,
};
use crate::metadata::Source;
use crate::source::Source as Src;
use crate::type_system::{ergo_trait, ergo_traits_fn, ErgoTrait, Trait, Type};
use crate::{
    source::IntoSource,
    types,
    value::{match_value, IntoValue},
    Context, Value,
};

/// The Bind ergo trait.
#[ergo_trait]
pub trait Bind {
    async fn bind(&self, arg: Value) -> Value;
}

/// Create a bind error result for the given value.
pub fn bind_error(ctx: &Context, v: Value) -> crate::Error {
    let name = type_name(ctx, &v);
    Source::get(&v)
        .with(format!("cannot bind to value with type '{}'", name))
        .into_error()
}

/// Bind a value to an argument.
pub async fn bind(ctx: &Context, mut v: Value, arg: Value) -> Value {
    let bind_site = (Source::get(&v), Source::get(&arg)).into_source().with(());

    if let Err(e) = ctx.eval(&mut v).await {
        return e.into();
    }

    match ctx.get_trait::<Bind>(&v) {
        None => bind_error(ctx, v).into(),
        Some(t) => {
            let mut ret = t.bind(v, arg).await;
            Source::set_if_missing(&mut ret, bind_site);
            ret
        }
    }
}

/// Bind a value to an argument, evaluating the result and checking for an error.
pub async fn bind_no_error(ctx: &Context, v: Value, arg: Value) -> crate::Result<()> {
    let mut result = bind(ctx, v, arg).await;
    ctx.eval(&mut result).await?;
    Ok(())
}

/// create_bind_rest is used to create the value that is bound when a BindRest type is encountered.
/// The first argument is whether this is the first BindRest, and the second is the values to bind.
pub(crate) async fn bind_array<F>(
    ctx: &Context,
    to: RVec<Value>,
    from: Src<RVec<Value>>,
    mut create_bind_rest: F,
) -> crate::Result<()>
where
    F: FnMut(bool, Vec<Value>) -> Value + Send + Sync,
{
    use futures::future::BoxFuture;
    use futures::FutureExt;

    type Iter = crate::abi_stable::std_types::vec::IntoIter<Value>;

    fn back<'a, F>(
        ctx: &'a Context,
        to: &'a mut Iter,
        from: &'a mut Iter,
        create_bind_rest: &'a mut F,
    ) -> BoxFuture<'a, crate::Result<()>>
    where
        F: FnMut(bool, Vec<Value>) -> Value + Send + Sync,
    {
        async move {
            while let Some(t) = to.next_back() {
                let t_source = Source::get(&t);
                match_value!{t,
                    types::BindRest(rest) => {
                        match to.next_back() {
                            None => Err(t_source.with("undecidable merge").into_error())?,
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
                                        None => Err(Source::get(&to_v).with("no value matches this binding").into_error())?
                                    }
                                }
                                // Values were pushed in reverse order
                                vals.reverse();
                                let src: Src<()> = vals.iter().map(Source::get).collect();
                                let mut val_array = create_bind_rest(false, vals);
                                Source::set_if_missing(&mut val_array, src);
                                bind_no_error(ctx, rest, val_array).await?;
                            }
                        }
                    }
                    to_v => {
                        match from.next_back() {
                            None => Err(t_source.with("no value matches this binding").into_error())?,
                            Some(from_v) => bind_no_error(ctx, to_v, from_v).await?,
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
        F: FnMut(bool, Vec<Value>) -> Value + Send + Sync,
    {
        async move {
            let mut first_rest = true;
            while let Some(t) = to.next() {
                let t_source = Source::get(&t);
                match_value!{t,
                    types::BindRest(rest) => {
                        back(ctx, to, from, create_bind_rest).await?;
                        bind_no_error(ctx, rest, {
                            Source::imbue(from
                                .map(|v| Source::extract(v))
                                .collect::<Vec<_>>()
                                .into_source()
                                .map(|f| create_bind_rest(first_rest, f.into_iter().map(Src::unwrap).collect())))
                        }).await?;
                        first_rest = false;
                    }
                    to_v => {
                        match from.next() {
                            None => Err(t_source.with("no value matches this binding").into_error())?,
                            Some(from_v) => {
                                bind_no_error(ctx, to_v, from_v).await?;
                            }
                        }
                    }
                }
            }

            let remaining: RVec<_> = from.collect();
            if !remaining.is_empty() {
                let mut errs = Vec::new();
                for v in remaining {
                    errs.push(Source::get(&v).with("no binding matches this value").into_error());
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
    mut to: BstMap<Value, Value>,
    from: Src<BstMap<Value, Value>>,
    return_rest: bool,
) -> crate::Result<BstMap<Value, Value>> {
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
                Source::imbue(from_source.clone().with(types::Unset.into()))
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
                    errs.push(Source::get(&k).with("key missing in binding").into_error());
                }
                Err(crate::Error::aggregate(errs))
            }
        }
        Some(v) => {
            let remaining = from;
            bind_no_error(
                ctx,
                v,
                Source::imbue(from_source.with(types::Map(remaining).into())),
            )
            .await?;
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
            arg: Value) ->
            crate::abi_stable::future::BoxFuture<'a, Value> {
            crate::abi_stable::future::BoxFuture::new(async move {
                if v.id() != arg.id() {
                    let arg_source = Source::get(&arg);
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
                            arg_source.with("mismatched value in binding").into_error().into()
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
