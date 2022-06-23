//! The Bind ergo trait for setting values.

use super::type_name;
use crate as ergo_runtime;
use crate::abi_stable::{
    bst::BstMap,
    closure::FnPtr,
    std_types::{ROption, RVec},
    type_erase::Erased,
};
use crate::error::DiagnosticInfo;
use crate::metadata::Source;
use crate::source::Source as Src;
use crate::type_system::{ergo_trait, ergo_traits_fn, ErgoTrait, Trait, Type};
use crate::{types, value::match_value, Context, EvaluatedValue, Value};

/// The Bind ergo trait.
#[ergo_trait]
pub trait Bind {
    async fn bind(&self, arg: Value) -> Value;
}

/// Create a bind error result for the given value.
pub fn bind_error(v: Value) -> crate::Error {
    let name = type_name(&v);
    Source::get(&v)
        .with(format!("cannot bind to value with type '{}'", name))
        .into_error()
}

/// Bind a value to an argument.
pub async fn bind(mut v: Value, arg: Value) -> Value {
    if let Err(e) = Context::eval(&mut v).await {
        return e.into();
    }

    match Context::get_trait::<Bind>(&v) {
        None => bind_error(v).into(),
        Some(t) => t.bind(v, arg).await,
    }
}

/// Bind a value to an argument, evaluating the result and checking for an error.
pub async fn bind_no_error(v: Value, arg: Value) -> crate::Result<()> {
    let mut result = bind(v, arg).await;
    Context::eval(&mut result).await?;
    Ok(())
}

/// create_bind_rest is used to create the value that is bound when a BindRest type is encountered.
/// The first argument is whether this is the first BindRest, and the second is the values to bind.
pub(crate) async fn bind_array<F>(
    to: RVec<Value>,
    to_value: &Value,
    from: RVec<Value>,
    from_value: &Value,
    mut create_bind_rest: F,
) -> crate::Result<()>
where
    F: FnMut(bool, Vec<Value>) -> Value + Send + Sync,
{
    use futures::future::BoxFuture;
    use futures::FutureExt;

    type Iter = crate::abi_stable::std_types::vec::IntoIter<Value>;

    fn back<'a, F>(
        to: &'a mut Iter,
        _to_value: &'a Value,
        from: &'a mut Iter,
        from_value: &'a Value,
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
                                            if bind(to_v.clone(), from_v.clone()).await.is_type::<types::Error>() {
                                                vals.push(from_v);
                                            } else {
                                                break;
                                            }
                                        }
                                        None => Err(
                                            crate::diagnostic! {
                                                message: "no value in 'values' matches 'target'"
                                            }
                                            .add_value_sources("target", &to_v)
                                            .add_value_info("values", from_value).await
                                        )?
                                    }
                                }
                                // Values were pushed in reverse order
                                vals.reverse();
                                let src: Src<()> = vals.iter().map(Source::get).collect();
                                let mut val_array = create_bind_rest(false, vals);
                                Source::set_if_missing(&mut val_array, src);
                                bind_no_error(rest, val_array).await?;
                            }
                        }
                    }
                    to_v => {
                        match from.next_back() {
                            // If `from` is missing a `to` element, error
                            None => Err(
                                crate::diagnostic! {
                                    message: "there are not enough 'values' to bind 'target'"
                                }
                                .add_value_sources("target", &to_v)
                                .add_value_info("values", from_value).await
                            )?,
                            Some(from_v) => bind_no_error(to_v, from_v).await?,
                        }
                    }
                }
            }

            Ok(())
        }.boxed()
    }

    fn forward<'a, F>(
        to: &'a mut Iter,
        to_value: &'a Value,
        from: &'a mut Iter,
        from_value: &'a Value,
        create_bind_rest: &'a mut F,
    ) -> BoxFuture<'a, crate::Result<()>>
    where
        F: FnMut(bool, Vec<Value>) -> Value + Send + Sync,
    {
        async move {
            let mut first_rest = true;
            while let Some(t) = to.next() {
                match_value!{t,
                    types::BindRest(rest) => {
                        back(to, to_value, from, from_value, create_bind_rest).await?;
                        bind_no_error(
                            rest,
                            Source::copy(from_value, create_bind_rest(first_rest, from.collect())),
                        ).await?;
                        first_rest = false;
                    }
                    to_v => {
                        match from.next() {
                            // If `from` is missing a `to` element, error
                            None => Err(
                                crate::diagnostic! {
                                    message: "there are not enough values in 'value' to bind 'target'"
                                }
                                .add_value_sources("target", &to_v)
                                .add_value_info("value", from_value).await
                            )?,
                            Some(from_v) => bind_no_error(to_v, from_v).await?,
                        }
                    }
                }
            }

            let remaining: RVec<_> = from.collect();
            if !remaining.is_empty() {
                let mut errs = Vec::new();
                for v in remaining {
                    errs.push(
                        crate::diagnostic! {
                            message: "extraneous value when binding"
                        }
                        .add_value_sources("target", to_value)
                        .add_value_info("value", &v).await
                        .into()
                    );
                }
                Err(crate::Error::aggregate(errs))
            } else {
                Ok(())
            }
        }.boxed()
    }

    let mut to = to.into_iter();
    let mut from = from.into_iter();

    forward(
        &mut to,
        to_value,
        &mut from,
        from_value,
        &mut create_bind_rest,
    )
    .await
}

/// Returns the remaining items in `from` that were not bound if `return_rest` is true and
/// `BindRestKey` is not a key in `to`.
pub(crate) async fn bind_map(
    mut to: BstMap<EvaluatedValue, Value>,
    to_value: &Value,
    mut from: BstMap<EvaluatedValue, Value>,
    from_value: &Value,
    return_rest: bool,
) -> crate::Result<BstMap<EvaluatedValue, Value>> {
    let rest = to.remove(&EvaluatedValue::from(types::BindRestKey));
    for (k, to_v) in to.into_iter() {
        bind_no_error(
            to_v,
            if let Some(from_v) = from.remove(&k) {
                from_v
            } else {
                // If `from` is missing a `to` key, use `Unset`.
                Source::copy(&from_value, types::Unset.into())
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
                    errs.push(
                        crate::diagnostic! {
                            message: "extraneous key in binding"
                        }
                        .add_value_info("key", &k)
                        .await
                        .add_value_sources("target", &to_value)
                        .into(),
                    );
                }
                Err(crate::Error::aggregate(errs))
            }
        }
        Some(v) => {
            let remaining = from;
            bind_no_error(v, Source::copy(&from_value, types::Map(remaining).into())).await?;
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
            v: &'a Value,
            tp: &'a Type,
            mut arg: Value) ->
            crate::abi_stable::future::BoxFuture<'a, Value> {
            crate::abi_stable::future::BoxFuture::new(async move {
                crate::try_result!(Context::eval(&mut arg).await);
                if tp != &arg.ergo_type().unwrap() || v.id().await != arg.id().await {
                    match_value! { arg.clone(),
                        types::Args {..} => {
                            crate::diagnostic! {
                                message: "cannot call value"
                            }
                            .add_value_info("target", v).await
                            .add_value_info("args", &arg).await
                            .into_error()
                            .into()
                        }
                        types::Index(_) => {
                            crate::diagnostic! {
                                message: "cannot index value"
                            }
                            .add_value_info("target", v).await
                            .add_value_info("index", &arg).await
                            .into_error()
                            .into()
                        }
                        _ => {
                            crate::diagnostic! {
                                message: "mismatched value in binding"
                            }
                            .add_value_info("target", v).await
                            .add_value_info("bound", &arg).await
                            .into_error()
                            .into()
                        }
                    }
                } else {
                    types::Unit.into()
                }
            })
        }
        #[allow(improper_ctypes_definitions)]
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
