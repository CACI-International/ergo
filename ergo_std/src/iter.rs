//! Iterator functions.

use ergo_runtime::{depends, nsid, traits, try_result, types, value::match_value, Value};
use futures::{
    future::{ready, FutureExt},
    stream::StreamExt,
};
use std::collections::BTreeSet;

pub fn module() -> Value {
    crate::make_string_map! {
        "from" = from(),
        "fold" = fold(),
        "filter" = filter(),
        "flatten" = flatten(),
        "map" = map(),
        "map-lazy" = map_lazy(),
        "skip" = skip(),
        "skip-while" = skip_while(),
        "take" = take(),
        "take-while" = take_while(),
        "unique" = unique(),
        "zip" = zip()
    }
}

#[types::ergo_fn]
/// Convert a value into an Iter.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    try_result!(traits::into_sourced::<types::Iter>(CONTEXT, value).await)
        .unwrap()
        .into()
}

#[types::ergo_fn]
/// Accumulate a value with each value in an iterator.
///
/// Arguments: `(Function :func) :accumulator (Into<Iter> :iter)`
///
/// The function will be applied on each subsequent value in the iterator, specifically as `func
/// :accumulator :iter-value`. The value the function produces will be the `accumulator` used in the
/// next call. Returns the last `accumulator`, which may be the original if there are no values in
/// the iterator.
async fn fold(func: _, acc: _, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    iter.to_owned()
        .fold(acc, |acc, v| {
            traits::bind(
                CONTEXT,
                func.clone(),
                ARGS_SOURCE.clone().with(
                    types::Args {
                        args: types::args::Arguments::positional(vec![acc, v]).unchecked(),
                    }
                    .into(),
                ),
            )
        })
        .await
        .unwrap()
}

#[types::ergo_fn]
/// Filter an iterator to unique values (by identity).
///
/// Arguments: `(Into<Iter> :iter)`
///
/// Returns a new iterator containing only the unique values of `iter` (where the first unique value is
/// retained).
async fn unique(iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::unique), iter];

    let mut seen: BTreeSet<u128> = Default::default();
    let new_iter = iter.to_owned().filter(move |v| ready(seen.insert(v.id())));

    types::Iter::new_stream(new_iter, deps).into()
}

#[types::ergo_fn]
/// Filter the values of an iterator according to a function.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` for which `func` returned a value
/// which was `true` when converted to Bool.
async fn filter(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::filter), iter];

    // TODO improve context of stream execution
    let ctx = CONTEXT.clone();
    let new_iter = iter.to_owned().filter_map(move |v| {
        let func = func.clone();
        let ctx = ctx.clone();
        let args_source = ARGS_SOURCE.clone();
        async move {
            let res = traits::bind(
                &ctx,
                func,
                args_source.with(
                    types::Args {
                        args: types::args::Arguments::positional(vec![v.clone()]).unchecked(),
                    }
                    .into(),
                ),
            )
            .await;
            let src = res.source();

            // TODO handle errors differently?
            match traits::into_sourced::<types::Bool>(&ctx, res).await {
                Ok(b) => {
                    if b.value().as_ref().0 {
                        Some(v)
                    } else {
                        None
                    }
                }
                Err(e) => Some(src.with(e.into())),
            }
        }
    });

    types::Iter::new_stream(new_iter, deps).into()
}

fn zip() -> Value {
    types::Unbound::new(|ctx, arg| {
        async move {
            let (arg_source, arg) = arg.take();
            match_value! { arg,
                types::Args { mut args } => {
                    let iters: Vec<_> = (&mut args).collect();

                    try_result!(args.unused_arguments());

                    let iters_typed = try_result!(ctx.task.join_all(
                            iters.into_iter().map(|i| traits::into_sourced::<types::Iter>(ctx, i))
                        ).await);

                    let deps = depends![nsid!(std::iter::zip), ^@iters_typed];

                    let new_iter = if iters_typed.is_empty() {
                        types::Iter::from_iter(std::iter::empty())
                    } else {
                        let cap = iters_typed.len();
                        let vec_stream = iters_typed.into_iter().fold(
                                futures::stream::repeat_with(move || {
                                    abi_stable::std_types::RVec::with_capacity(cap)
                                }).boxed(),
                                |vec_stream, iter| {
                                    vec_stream.zip(iter.unwrap().to_owned())
                                        .map(|(mut vec,v)| { vec.push(v); vec })
                                        .boxed()
                                },
                        );
                        let val_stream = vec_stream.map(move |vec| arg_source.clone().with(types::Array(vec).into()));
                        types::Iter::from_stream(val_stream)
                    };

                    Value::constant_deps(new_iter, deps)
                },
                types::PatternArgs { mut args } => {
                    let iter_outs: Vec<_> = (&mut args).collect();

                    try_result!(args.unused_arguments());

                    let deps = depends![nsid!(std::iter::zip::pattern), ^@iter_outs];

                    types::Unbound::new_no_doc(move |ctx, arg| {
                        let iter_outs = iter_outs.clone();
                        async move {
                            let (arg_source, arg) = try_result!(ctx.eval_as::<types::Iter>(arg).await).take();
                            let iter_id = arg.id();
                            let iter = arg.to_owned();

                            let items: Vec<_> = iter.collect().await;
                            let arrays = try_result!(ctx.task.join_all(items.into_iter().map(|v| ctx.eval_as::<types::Array>(v))).await);

                            let shared_arrays = ergo_runtime::abi_stable::stream::shared_async_stream::SharedAsyncStream::new(futures::stream::iter(arrays.into_iter()));
                            let mut errs = Vec::new();
                            for (i, out) in iter_outs.into_iter().enumerate() {
                                let arrays = shared_arrays.clone();
                                let filtered = arrays.filter_map(move |arr| {
                                        ready(match arr.value().as_ref().0.get(i) {
                                            None => None,
                                            Some(v) => if v.is_type::<types::Unset>() {
                                                // XXX this won't work for delayed Unset values
                                                None
                                            } else {
                                                Some(v.clone())
                                            }
                                        })
                                    });
                                let deps = depends![nsid!(std::iter::zip::pattern_result), iter_id, i];
                                let to_bind = arg_source.clone().with(types::Iter::new_stream(filtered, deps).into());
                                let mut result = traits::bind(ctx, out, to_bind).await.unwrap();
                                if let Err(e) = ctx.eval(&mut result).await {
                                    errs.push(e);
                                }
                            }
                            if errs.is_empty() {
                                types::Unit.into()
                            } else {
                                types::Error::aggregate(errs).into()
                            }
                        }.boxed()
                    }, deps).into()
                },
                v => traits::bind_error(ctx, arg_source.with(v)).into()
            }
        }.boxed()
    }, depends![nsid!(std::iter::zip)],
        r"Zip the values of iterators together.

Arguments: `^((Array:Of Into<Iter>) :iters)`

Returns a new iterator containing arrays where the each value index corresponds to the iterator
argument to the function. The returned iterator will have only as many values as the minimum of the
passed iterators.

When used in a pattern call, unzips an iterator of arrays into the provided iterators, ignoring
missing and extra values in each array. If arrays have `Unset` values, they are skipped in the respective
iterator.").into()
}

#[types::ergo_fn]
/// Skip the consecutive values for which a function returns true.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` following (and including) the first
/// value for which `func` returned a value which was `false` when converted to Bool.
async fn skip_while(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::skip_while), func, iter];

    // TODO improve context of stream execution
    let ctx = CONTEXT.clone();
    let skip = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
    let new_iter = iter.to_owned().filter_map(move |v| {
        if !skip.load(std::sync::atomic::Ordering::Relaxed) {
            ready(Some(v)).boxed()
        } else {
            let func = func.clone();
            let skip = skip.clone();
            let ctx = ctx.clone();
            let args_source = ARGS_SOURCE.clone();
            async move {
                let res = traits::bind(
                    &ctx,
                    func,
                    args_source.with(
                        types::Args {
                            args: types::args::Arguments::positional(vec![v.clone()]).unchecked(),
                        }
                        .into(),
                    ),
                )
                .await;

                let src = res.source();

                // TODO handle errors differently?
                match traits::into_sourced::<types::Bool>(&ctx, res).await {
                    Ok(b) => {
                        if b.value().as_ref().0 {
                            None
                        } else {
                            skip.store(false, std::sync::atomic::Ordering::Relaxed);
                            Some(v)
                        }
                    }
                    Err(e) => Some(src.with(e.into())),
                }
            }
            .boxed()
        }
    });

    types::Iter::new_stream(new_iter, deps).into()
}

#[types::ergo_fn]
/// Skip the first `n` consecutive values.
///
/// Arguments: `(String :n) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` after the first `n`.
async fn skip(n: types::String, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();
    let deps = depends![nsid!(std::iter::skip), n, iter];

    let n = try_result!(<usize as std::str::FromStr>::from_str(
        n.value().as_ref().as_str()
    ));

    types::Iter::new_stream(iter.to_owned().skip(n), deps).into()
}

#[types::ergo_fn]
/// Take the consecutive values for which a function returns true.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` preceding the first
/// value for which `func` returned a value which was `false` when converted to Bool.
async fn take_while(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::take_while), func, iter];

    // TODO improve context of stream execution
    let ctx = CONTEXT.clone();
    let new_iter = iter.to_owned().scan((), move |_, v| {
        let func = func.clone();
        let ctx = ctx.clone();
        let args_source = ARGS_SOURCE.clone();
        async move {
            let res = traits::bind(
                &ctx,
                func,
                args_source.with(
                    types::Args {
                        args: types::args::Arguments::positional(vec![v.clone()]).unchecked(),
                    }
                    .into(),
                ),
            )
            .await;

            let src = res.source();

            // TODO handle errors differently?
            match traits::into_sourced::<types::Bool>(&ctx, res).await {
                Ok(b) => {
                    if b.value().as_ref().0 {
                        Some(v)
                    } else {
                        None
                    }
                }
                Err(e) => Some(src.with(e.into())),
            }
        }
    });

    types::Iter::new_stream(new_iter, deps).into()
}

#[types::ergo_fn]
/// Take the first `n` consecutive values.
///
/// Arguments: `(String :n) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the first `n` values from `iter`.
async fn take(n: types::String, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::take), n, iter];

    let n = try_result!(<usize as std::str::FromStr>::from_str(
        n.value().as_ref().as_str()
    ));

    types::Iter::new_stream(iter.to_owned().take(n), deps).into()
}

#[types::ergo_fn]
/// Flatten the values of an iterator.
///
/// Arguments: `(Into<Iter> :iter)`
///
/// Returns a new iterator with each subsequent nested value in the values of `iter`. The values of
/// `iter` must be `Into<Iter>` themselves.
async fn flatten(iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::flatten), iter];

    // TODO stream context
    let ctx = CONTEXT.clone();
    let new_iter = iter
        .to_owned()
        .then(move |i| {
            let ctx = ctx.clone();
            async move {
                let src = i.source();
                match traits::into_sourced::<types::Iter>(&ctx, i).await {
                    Ok(i) => i.unwrap().to_owned(),
                    Err(e) => types::Iter::from_iter(std::iter::once(src.with(e.into()))),
                }
            }
        })
        .flatten();

    types::Iter::new_stream(new_iter, deps).into()
}

#[types::ergo_fn]
/// Apply a function to each value in an iterator.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// The function is applied concurrently to all values in the iterator. See `map-lazy` to map to
/// each subsequent value.
///
/// Returns a new iterator where each element is the result of applying `func` on each value in `iter`.
async fn map(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::map), iter];

    let vals: Vec<_> = iter.to_owned().collect().await;
    let new_iter = CONTEXT
        .task
        .join_all(vals.into_iter().map(|d| {
            traits::bind(
                CONTEXT,
                func.clone(),
                ARGS_SOURCE.clone().with(
                    types::Args {
                        args: types::args::Arguments::positional(vec![d]).unchecked(),
                    }
                    .into(),
                ),
            )
            .map(Ok)
        }))
        .await
        .unwrap()
        .into_iter();

    types::Iter::new(new_iter, deps).into()
}

#[types::ergo_fn]
/// Apply a function to each value in an iterator lazily.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// `func` will only be applied when the specific iterator value is needed.
///
/// Returns a new iterator where each element is the result of applying `func` on each value in `iter`.
async fn map_lazy(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into_sourced::<types::Iter>(CONTEXT, iter).await).unwrap();

    let deps = depends![nsid!(std::iter::map_lazy), iter];

    let ctx = CONTEXT.clone();
    let new_iter = iter.to_owned().then(move |v| {
        let func = func.clone();
        let ctx = ctx.clone();
        let args_source = ARGS_SOURCE.clone();
        async move {
            traits::bind(
                &ctx,
                func,
                args_source.with(
                    types::Args {
                        args: types::args::Arguments::positional(vec![v]).unchecked(),
                    }
                    .into(),
                ),
            )
            .await
        }
    });

    types::Iter::new_stream(new_iter, deps).into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn fold(t) {
            t.assert_content_eq("self:iter:fold (fn :r :a -> [:a,^:r]) [init] [a,b,c]", "[c,b,a,init]");
        }
    }

    ergo_script::test! {
        fn filter(t) {
            t.assert_content_eq("self:iter:filter (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,[],c,(),(),d,e]", "self:iter:from [a,b,c,d,e]");
        }
    }

    ergo_script::test! {
        fn flatten(t) {
            t.assert_content_eq("self:iter:flatten [[a,b],[],[],[c,d,e,f],[g]]", "self:iter:from [a,b,c,d,e,f,g]");
        }
    }

    ergo_script::test! {
        fn map(t) {
            t.assert_content_eq("self:iter:map (fn :a -> { mapped = :a }) [2,3]", "self:iter:from [{mapped = 2},{mapped = 3}]");
        }
    }

    ergo_script::test! {
        fn map_lazy(t) {
            t.assert_content_eq("self:iter:map-lazy (fn :a -> { mapped = :a }) [2,3]", "self:iter:from [{mapped = 2},{mapped = 3}]");
        }
    }

    ergo_script::test! {
        fn skip(t) {
            t.assert_content_eq("self:iter:skip 5 [a,b,c,d,e,f,g]", "self:iter:from [f,g]");
            t.assert_content_eq("self:iter:skip 5 [a,b]", "self:iter:from []");
        }
    }

    ergo_script::test! {
        fn skip_while(t) {
            t.assert_content_eq("self:iter:skip-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,c,d,(),e,f,g]", "self:iter:from [(),e,f,g]");
            t.assert_content_eq("self:iter:skip-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,c,d]", "self:iter:from []");
        }
    }

    ergo_script::test! {
        fn take(t) {
            t.assert_content_eq("self:iter:take 4 [a,b,c,d,e,f,g]", "self:iter:from [a,b,c,d]");
            t.assert_content_eq("self:iter:take 4 [a]", "self:iter:from [a]");
        }
    }

    ergo_script::test! {
        fn take_while(t) {
            t.assert_content_eq("self:iter:take-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,c,d,(),e,f,g]", "self:iter:from [a,b,c,d]");
            t.assert_content_eq("self:iter:take-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [(),e]", "self:iter:from []");
            t.assert_content_eq("self:iter:take-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b]", "self:iter:from [a,b]");
        }
    }

    ergo_script::test! {
        fn unique(t) {
            t.assert_content_eq("self:iter:unique [1,2,3,2,40,5,6,5]", "self:iter:from [1,2,3,40,5,6]");
        }
    }

    ergo_script::test! {
        fn zip(t) {
            t.assert_content_eq("self:iter:zip [a,b,c,d] [1,2,3,4]", "self:iter:from [[a,1],[b,2],[c,3],[d,4]]");
            t.assert_content_eq("self:iter:zip [a,b,c,d] [1,2]", "self:iter:from [[a,1],[b,2]]");
            t.assert_content_eq("self:iter:zip [a,b] [1,2,3,4]", "self:iter:from [[a,1],[b,2]]");
            t.assert_content_eq("self:iter:zip:", "self:iter:from []");
            t.assert_content_eq("self:iter:zip [a,b,c]", "self:iter:from [[a],[b],[c]]");
            t.assert_content_eq("self:iter:zip [a,b] [1,2] [x,y,z]", "self:iter:from [[a,1,x],[b,2,y]]");
            t.assert_content_eq("self:iter:zip :x :y = self:iter:from [[a,1],[b,2]]; [:x, :y]", "[self:iter:from [a,b],self:iter:from [1,2]]");
            t.assert_content_eq(
                "self:iter:zip :x :y :z = self:iter:from [[a,1,x],[b,2],[c,3,y,q],[d,!self:type:Unset:,z]]; [:x, :y, :z]",
                "[self:iter:from [a,b,c,d],self:iter:from [1,2,3],self:iter:from [x,y,z]]"
            );
        }
    }
}
