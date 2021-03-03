//! Iterator functions.

use ergo_runtime::{ergo_function, namespace_id, traits, types, Arguments, ContextExt};
use futures::{
    future::{ok, ready, FutureExt, TryFutureExt},
    stream::{StreamExt, TryStreamExt},
};
use grease::{depends, make_value, match_value, Value};
use std::collections::BTreeSet;

pub fn module() -> Value {
    crate::grease_string_map! {
        "from" = from_fn(),
        "fold" = fold_fn(),
        "filter" = filter_fn(),
        "flatten" = flatten_fn(),
        "map" = map_fn(),
        "map-all" = map_all_fn(),
        "skip" = skip_fn(),
        "skip-while" = skip_while_fn(),
        "take" = take_fn(),
        "take-while" = take_while_fn(),
        "unique" = unique_fn(),
        "zip" = zip_fn()
    }
}

fn from_fn() -> Value {
    ergo_function!(independent std::iter::from,
    r"Convert a value into an Iter.

Arguments: `:value`",
    |ctx, args| {
        let value = args.next().ok_or("value not provided")?;

        args.unused_arguments()?;

        ctx.into_sourced::<types::Iter>(value).await?.unwrap().into()
    })
    .into()
}

fn fold_fn() -> Value {
    ergo_function!(
        std::iter::fold,
        r"Accumulate a value with each value in an iterator.

Arguments: `(Function :func) :accumulator (Into<Iter> :iter)`
The function will be applied on each subsequent value in the iterator, specifically as `func
:accumulator :iter-value`. The value the function produces will be the `accumulator` used in the
next call. Returns a dynamically-typed value that will evaluate to the last `accumulator`, which
may be the original if there are no values in the iterator.",
        |ctx, args| {
            let func = args.next().ok_or("fold function not provided")?;
            let acc = args.next().ok_or("fold accumulator value not provided")?;
            let iter = args.next().ok_or("fold iterator not provided")?;

            args.unused_arguments()?;

            let func = traits::delay_bind(ctx, func).await?;

            let iter = ctx.into_sourced::<types::Iter>(iter).await?;

            let deps = depends![*func.value, *acc, *iter];

            Value::dyn_new(
                async move {
                    let (iter_source, iter) = iter.take();
                    let iter = iter.await?.owned();
                    let vals: Vec<_> = iter.try_collect().await?;
                    let val = vals.into_iter().fold(ok(acc).boxed(), move |acc, v| {
                        let src = iter_source.clone();
                        let func = func.clone();
                        acc.and_then(move |accv| {
                            let func = func.clone();
                            async move {
                                func.bind(
                                    src.clone().with(
                                        types::Args {
                                            args: Arguments::positional(vec![accv, src.with(v)])
                                                .unchecked(),
                                        }
                                        .into(),
                                    ),
                                )
                                .await
                            }
                        })
                        .boxed()
                    });

                    Ok(val.await?.unwrap().into_any_value())
                },
                deps,
            )
        }
    )
    .into()
}

fn unique_fn() -> Value {
    ergo_function!(
        std::iter::unique,
        r"Filter an iterator to unique values (by identity).

Arguments: `(Into<Iter> :iter)`
Returns a new iterator containing only the unique values of `iter` (where the first unique value is
retained).",
        |ctx, args| {
            let iter = args.next().ok_or("filter iterator not provided")?;

            args.unused_arguments()?;

            let iter = ctx.into_sourced::<types::Iter>(iter).await?.unwrap();

            make_value!([*iter] {
                let iter = iter.await?.owned();
                let mut seen: BTreeSet<u128> = Default::default();
                let new_iter = iter.try_filter(move |v| ready(seen.insert(v.id())));

                Ok(types::Iter::from_stream(new_iter))
            })
            .into()
        }
    )
    .into()
}

fn filter_fn() -> Value {
    ergo_function!(std::iter::filter,
    r"Filter the values of an iterator according to a function.

Arguments: `(Function :func) (Into<Iter> :iter)`
Returns a new iterator containing only the values from `iter` for which `func` returned a value
which was `true` when converted to Bool.",
    |ctx,args| {
        let func = args.next().ok_or("filter function not provided")?;
        let iter = args.next().ok_or("filter iterator not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let iter = ctx.into_sourced::<types::Iter>(iter).await?;

        let ctx = ctx.empty();
        make_value!([*func.value,*iter] {
            let (iter_source, iter) = iter.take();
            let iter = iter.await?.owned();
            let new_iter = iter
                .try_filter_map(move |d| {
                    let func = func.clone();
                    let iter_source = iter_source.clone();
                    let ctx = ctx.clone();
                    async move {
                        let res = func.bind(
                                iter_source.clone().with(types::Args {
                                    args: Arguments::positional(vec![iter_source.with(d.clone())]).unchecked()
                                }.into())
                            ).await?;
                        let res = ctx.into_sourced::<types::Bool>(res).await?.unwrap();
                        if res.await?.0 {
                            Ok(Some(d))
                        } else {
                            Ok(None)
                        }
                    }
                });

            Ok(types::Iter::from_stream(new_iter))
        }).into()
    }).into()
}

fn zip_fn() -> Value {
    types::Unbound::new(|ctx, arg| {
        async move {
            let (arg_source, arg) = arg.take();
            match_value!(arg => {
                types::Args => |args| {
                    let args = &mut args.await?.owned().args;
                    let iters: Vec<_> = args.collect();

                    args.unused_arguments()?;

                    let mut iters_typed = Vec::new();
                    for i in iters {
                        iters_typed.push(ctx.into_sourced::<types::Iter>(i).await?.unwrap());
                    }

                    let task = ctx.task.clone();
                    let deps = depends![^@iters_typed];
                    make_value!([namespace_id!(std::iter::zip), ^deps] {
                        let iters_typed = task.join_all(iters_typed).await?;

                        if iters_typed.is_empty() {
                            Ok(types::Iter::from_iter(std::iter::empty()))
                        } else {
                            let cap = iters_typed.len();
                            let vec_stream = iters_typed.into_iter().fold(futures::stream::repeat_with(move || ergo_runtime::Result::Ok(abi_stable::std_types::RVec::with_capacity(cap))).boxed(), |vec_stream, stream| {
                                vec_stream.zip(stream.owned()).map(|(vec,v)| { let mut vec = vec?; vec.push(v?); Ok(vec) }).boxed()
                            });
                            let val_stream = vec_stream.map(|vec| Ok(types::Array(vec?).into()));
                            Ok(types::Iter::from_stream(val_stream))
                        }
                    })
                    .into()
                },
                types::PatternArgs => |pat_args| {
                    let pat_args = &mut pat_args.await?.owned().args;
                    let iter_outs: Vec<_> = pat_args.collect();
                    pat_args.unused_arguments()?;

                    let mut deps = depends![];
                    for i in iter_outs.iter() {
                        deps += depends![**i];
                    }
                    types::Unbound::new(move |ctx, arg| {
                        let iter_outs = iter_outs.clone();
                        async move {
                            let (arg_source, arg) = ctx.source_pattern_value_as::<types::Iter>(arg).await?.take();
                            let iter_id = arg.id();
                            let iter = arg.await?.owned();

                            let iter_ctx = ctx.empty();
                            let iter_src = arg_source.clone();
                            let arrays = iter.and_then(move |v| {
                                let src = iter_src.clone();
                                let ctx = iter_ctx.clone();
                                async move {
                                    ctx.source_value_as::<types::Array>(src.with(v)).await?.unwrap().await
                                }
                            }).boxed();

                            let shared_arrays = ergo_runtime::SharedAsyncStream::new(arrays);
                            for (i, out) in iter_outs.into_iter().enumerate() {
                                let arrays = shared_arrays.clone();
                                let filtered = arrays.try_filter_map(move |arr| {
                                        ok(match arr.0.get(i) {
                                            None => None,
                                            Some(v) => if types::Unset::is_unset(v) {
                                                None
                                            } else {
                                                Some(v.clone())
                                            }
                                        })
                                    });
                                let deps = depends![namespace_id!(std::iter::zip::pattern), i, iter_id];
                                let to_bind = arg_source.clone().with(types::Iter::new_stream(filtered, deps).into());
                                traits::bind(ctx, out, to_bind).await?;
                            }
                            Ok(types::Unit.into())
                        }.boxed()
                    }, deps, ()).into()
                },
                => |v| traits::bind_error(ctx, arg_source.with(v)).await?
            }).await
        }.boxed()
    }, depends![namespace_id!(std::iter::zip)],
        r"Zip the values of iterators together.

Arguments: `^((Array:Of Into<Iter>) :iters)`
Returns a new iterator containing arrays where the each value index corresponds to the iterator
argument to the function. The returned iterator will have only as many values as the minimum of the
passed iterators.

When used in a pattern call, unzips an iterator of arrays into the provided iterators, ignoring
missing and extra values in each array. If arrays have `Unset` values, they are skipped in the respective
iterator.").into()
}

fn skip_while_fn() -> Value {
    ergo_function!(std::iter::skip_while,
    r"Skip the consecutive values for which a function returns true.

Arguments: `(Function :func) (Into<Iter> :iter)`
Returns a new iterator containing only the values from `iter` following (and including) the first
value for which `func` returned a value which was `false` when converted to Bool.",
    |ctx,args| {
        let func = args.next().ok_or("skip-while function not provided")?;
        let iter = args.next().ok_or("skip-while iterator not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let iter = ctx.into_sourced::<types::Iter>(iter).await?;

        let ctx = ctx.empty();
        make_value!([*func.value,*iter] {
            let (iter_source, iter) = iter.take();
            let iter = iter.await?.owned();
            let new_iter = iter
                .try_skip_while(move |d| {
                    let func = func.clone();
                    let iter_source = iter_source.clone();
                    let ctx = ctx.clone();
                    let d = d.clone();
                    async move {
                        let res = func.bind(
                                iter_source.clone().with(types::Args {
                                    args: Arguments::positional(vec![iter_source.with(d)]).unchecked()
                                }.into())
                            ).await?;
                        let res = ctx.into_sourced::<types::Bool>(res).await?.unwrap();
                        Ok(res.await?.0)
                    }
                });

            Ok(types::Iter::from_stream(new_iter))
        }).into()
    }).into()
}

fn skip_fn() -> Value {
    ergo_function!(
        std::iter::skip,
        r"Skip the first `n` consecutive values.

Arguments: `(String :n) (Into<Iter> :iter)`
Returns a new iterator containing only the values from `iter` after the first `n`.",
        |ctx, args| {
            let amount = args.next().ok_or("skip amount not provided")?;
            let iter = args.next().ok_or("skip iterator not provided")?;

            args.unused_arguments()?;

            let amount = ctx.source_value_as::<types::String>(amount).await?.unwrap();
            let iter = ctx.into_sourced::<types::Iter>(iter).await?.unwrap();

            make_value!([*amount,*iter] {
                let amount = amount.await?;
                let amount = <usize as std::str::FromStr>::from_str(amount.as_str())?;
                let iter = iter.await?.owned();
                let new_iter = iter.skip(amount);

                Ok(types::Iter::from_stream(new_iter))
            })
            .into()
        }
    )
    .into()
}

fn take_while_fn() -> Value {
    ergo_function!(std::iter::take_while,
    r"Take the consecutive values for which a function returns true.

Arguments: `(Function :func) (Into<Iter> :iter)`
Returns a new iterator containing only the values from `iter` preceding the first
value for which `func` returned a value which was `false` when converted to Bool.",
    |ctx,args| {
        let func = args.next().ok_or("take-while function not provided")?;
        let iter = args.next().ok_or("take-while iterator not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let iter = ctx.into_sourced::<types::Iter>(iter).await?;

        let ctx = ctx.empty();
        make_value!([*func.value,*iter] {
            let (iter_source, iter) = iter.take();
            let iter = iter.await?.owned();
            let new_iter = iter
                .try_take_while(move |d| {
                    let func = func.clone();
                    let iter_source = iter_source.clone();
                    let ctx = ctx.clone();
                    let d = d.clone();
                    async move {
                        let res = func.bind(
                                iter_source.clone().with(types::Args {
                                    args: Arguments::positional(vec![iter_source.with(d)]).unchecked()
                                }.into())
                            ).await?;
                        let res = ctx.into_sourced::<types::Bool>(res).await?.unwrap();
                        Ok(res.await?.0)
                    }
                });

            Ok(types::Iter::from_stream(new_iter))
        }).into()
    }).into()
}

fn take_fn() -> Value {
    ergo_function!(
        std::iter::take,
        r"Take the first `n` consecutive values.

Arguments: `(String :n) (Into<Iter> :iter)`
Returns a new iterator containing only the first `n` values from `iter`.",
        |ctx, args| {
            let amount = args.next().ok_or("take amount not provided")?;
            let iter = args.next().ok_or("take iterator not provided")?;

            args.unused_arguments()?;

            let amount = ctx.source_value_as::<types::String>(amount).await?.unwrap();
            let iter = ctx.into_sourced::<types::Iter>(iter).await?.unwrap();

            make_value!([*amount,*iter] {
                let amount = amount.await?;
                let amount = <usize as std::str::FromStr>::from_str(amount.as_str())?;
                let iter = iter.await?.owned();
                let new_iter = iter.take(amount);

                Ok(types::Iter::from_stream(new_iter))
            })
            .into()
        }
    )
    .into()
}

fn flatten_fn() -> Value {
    ergo_function!(
        std::iter::flatten,
        r"Flatten the values of an iterator.

Arguments: `(Into<Iter> :iter)`
Returns a new iterator with each subsequent nested value in the values of `iter`. The values of
`iter` must be `Into<Iter>` themselves.",
        |ctx, args| {
            let iter = args.next().ok_or("flatten iterator not provided")?;

            args.unused_arguments()?;

            let iter = ctx.into_sourced::<types::Iter>(iter).await?;

            let ctx = ctx.empty();
            make_value!([*iter] {
                let (iter_source, iter) = iter.take();
                let iter = iter.await?.owned();
                let new_iter = iter
                    .and_then(move |d| {
                        let ctx = ctx.clone();
                        let iter_source = iter_source.clone();
                        async move {
                            let v = ctx.into_sourced::<types::Iter>(iter_source.with(d)).await?.unwrap();
                            Ok(v.await?.owned())
                        }
                    })
                    .try_flatten();

                Ok(types::Iter::from_stream(new_iter))
            })
            .into()
        }
    )
    .into()
}

fn map_fn() -> Value {
    ergo_function!(std::iter::map,
    r"Apply a function to each value in an iterator.

Arguments: `(Function :func) (Into<Iter> :iter)`
Returns a new iterator where each element is the result of applying `func` on each value in `iter`.",
    |ctx,args| {
        let func = args.next().ok_or("map function not provided")?;
        let iter = args.next().ok_or("map iterator not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let iter = ctx.into_sourced::<types::Iter>(iter).await?;

        make_value!([*func.value,*iter] {
            let (iter_source, iter) = iter.take();
            let iter = iter.await?.owned();
            let new_iter = iter
                .and_then(move |d| {
                    let func = func.clone();
                    let iter_source = iter_source.clone();
                    async move {
                        let v = func.bind(
                                iter_source.clone().with(types::Args {
                                    args: Arguments::positional(vec![iter_source.with(d)]).unchecked()
                                }.into())
                            ).await?;
                        Ok(v.unwrap())
                    }
                });

            Ok(types::Iter::from_stream(new_iter))
        }).into()
    }).into()
}

fn map_all_fn() -> Value {
    ergo_function!(std::iter::map_all,
    r"Apply a function to each value in an iterator concurrently.

Arguments: `(Function :func) (Into<Iter> :iter)`
Returns a new iterator where each element is the result of applying `func` on each value in `iter`.

This differs from Iter:map in that it applies the function to all values in the iterator concurrently.",
    |ctx,args| {
        let func = args.next().ok_or("map function not provided")?;
        let iter = args.next().ok_or("map iterator not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let iter = ctx.into_sourced::<types::Iter>(iter).await?;

        let task = ctx.task.clone();
        make_value!([*func.value,*iter] {
            let (iter_source, iter) = iter.take();
            let iter = iter.await?.owned();
            let vals: Vec<_> = iter.try_collect().await?;
            let new_iter = task.join_all(vals
                .into_iter()
                .map(|d| func.bind(
                            iter_source.clone().with(types::Args {
                                args: Arguments::positional(vec![iter_source.clone().with(d)]).unchecked()
                            }.into())
                        ).map(|res| res.map(|v| v.unwrap())))).await?.into_iter();

            Ok(types::Iter::from_iter(new_iter))
        }).into()
    }).into()
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
            t.assert_content_eq("self:iter:filter (fn :v -> !self:match :v (self:type:String -> self:bool:true) (_ -> self:bool:false)) [a,b,[],c,(),(),d,e]", "self:iter:from [a,b,c,d,e]");
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
        fn map_all(t) {
            t.assert_content_eq("self:iter:map-all (fn :a -> { mapped = :a }) [2,3]", "self:iter:from [{mapped = 2},{mapped = 3}]");
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
            t.assert_content_eq("self:iter:skip-while (fn :v -> !self:match :v (self:type:String -> self:bool:true) (_ -> self:bool:false)) [a,b,c,d,(),e,f,g]", "self:iter:from [(),e,f,g]");
            t.assert_content_eq("self:iter:skip-while (fn :v -> !self:match :v (self:type:String -> self:bool:true) (_ -> self:bool:false)) [a,b,c,d]", "self:iter:from []");
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
            t.assert_content_eq("self:iter:take-while (fn :v -> !self:match :v (self:type:String -> self:bool:true) (_ -> self:bool:false)) [a,b,c,d,(),e,f,g]", "self:iter:from [a,b,c,d]");
            t.assert_content_eq("self:iter:take-while (fn :v -> !self:match :v (self:type:String -> self:bool:true) (_ -> self:bool:false)) [(),e]", "self:iter:from []");
            t.assert_content_eq("self:iter:take-while (fn :v -> !self:match :v (self:type:String -> self:bool:true) (_ -> self:bool:false)) [a,b]", "self:iter:from [a,b]");
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
                "self:iter:zip :x :y :z = self:iter:from [[a,1,x],[b,2],[c,3,y,q],[d,self:type:Unset:,z]]; [:x, :y, :z]",
                "[self:iter:from [a,b,c,d],self:iter:from [1,2,3],self:iter:from [x,y,z]]"
            );
        }
    }
}
