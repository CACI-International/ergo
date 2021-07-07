//! Iterator functions.

use ergo_runtime::{
    depends, metadata::Source, nsid, traits, try_result, types, value::match_value, Value,
};
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
    try_result!(traits::into::<types::Iter>(CONTEXT, value).await).into()
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
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    // FIXME early exit on error?

    let mut acc = acc;
    let vals = try_result!(iter.to_owned().collect::<Vec<_>>(CONTEXT).await);
    for v in vals {
        acc = traits::bind(
            CONTEXT,
            func.clone(),
            Source::imbue(
                ARGS_SOURCE.clone().with(
                    types::Args {
                        args: types::args::Arguments::positional(vec![acc, v]).unchecked(),
                    }
                    .into(),
                ),
            ),
        )
        .await;
    }

    acc
}

#[types::ergo_fn]
/// Filter an iterator to unique values (by identity).
///
/// Arguments: `(Into<Iter> :iter)`
///
/// Returns a new iterator containing only the unique values of `iter` (where the first unique value is
/// retained).
async fn unique(iter: _) -> Value {
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::unique), iter];

    let iter = iter.to_owned();

    #[derive(Clone)]
    struct Unique {
        iter: types::Iter,
        seen: BTreeSet<u128>,
    }

    ergo_runtime::ImplGenerator!(Unique => |self, ctx| {
        while let Some(v) = self.iter.next(ctx).await? {
            if self.seen.insert(v.id()) {
                return Ok(Some(v));
            }
        }
        Ok(None)
    });

    types::Iter::new(
        Unique {
            iter,
            seen: Default::default(),
        },
        deps,
    )
    .into()
}

#[types::ergo_fn]
/// Filter the values of an iterator according to a function.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` for which `func` returned a value
/// which was `true` when converted to Bool.
async fn filter(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::filter), iter];

    let iter = iter.to_owned();

    #[derive(Clone)]
    struct Filter {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
    }

    ergo_runtime::ImplGenerator!(Filter => |self, ctx| {
        while let Some(v) = self.iter.next(ctx).await? {
            let res = traits::bind(
                ctx,
                self.func.clone(),
                Source::imbue(
                    self.args_source.clone().with(
                        types::Args {
                            args: types::args::Arguments::positional(vec![v.clone()]).unchecked(),
                        }
                        .into(),
                    ),
                ),
            )
            .await;

            let b = traits::into::<types::Bool>(ctx, res).await?;
            if b.as_ref().0 {
                return Ok(Some(v));
            }
        }
        Ok(None)
    });

    types::Iter::new(
        Filter {
            iter,
            func,
            args_source: ARGS_SOURCE,
        },
        deps,
    )
    .into()
}

fn zip() -> Value {
    types::Unbound::new(|ctx, arg| {
        async move {
            let arg_source = Source::get(&arg);
            match_value! { arg,
                types::Args { mut args } => {
                    let iters: Vec<_> = (&mut args).collect();

                    try_result!(args.unused_arguments());

                    let iters_typed = try_result!(ctx.task.join_all(
                            iters.into_iter().map(|i| traits::into::<types::Iter>(ctx, i))
                        ).await);

                    let deps = depends![nsid!(std::iter::zip), ^@iters_typed];

                    let new_iter = if iters_typed.is_empty() {
                        types::Iter::from_iter(std::iter::empty())
                    } else {
                        #[derive(Clone)]
                        struct Zip {
                            iters: Vec<types::Iter>,
                            arg_source: ergo_runtime::Source<()>,
                        }

                        ergo_runtime::ImplGenerator!(Zip => |self, ctx| {
                            let mut arr = abi_stable::std_types::RVec::with_capacity(self.iters.len());
                            for v in self.iters.iter_mut() {
                                match v.next(ctx).await? {
                                    None => return Ok(None),
                                    Some(v) => arr.push(v),
                                }
                            }
                            Ok(Some(Source::imbue(self.arg_source.clone().with(types::Array(arr).into()))))
                        });

                        types::Iter::from_generator(Zip {
                            iters: iters_typed.into_iter().map(|v| v.to_owned()).collect(),
                            arg_source,
                        })
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
                            let arg = try_result!(ctx.eval_as::<types::Iter>(arg).await);
                            let arg_source = Source::get(&arg);
                            let iter_id = arg.id();
                            let iter = arg.to_owned();

                            let items: Vec<_> = try_result!(iter.collect(ctx).await);
                            let arrays = try_result!(ctx.task.join_all(items.into_iter().map(|v| ctx.eval_as::<types::Array>(v))).await);

                            let shared_arrays = ergo_runtime::abi_stable::stream::shared_async_stream::SharedAsyncStream::new(futures::stream::iter(arrays.into_iter()));
                            let mut errs = Vec::new();
                            for (i, out) in iter_outs.into_iter().enumerate() {
                                let arrays = shared_arrays.clone();
                                let filtered = arrays.filter_map(move |arr| {
                                        ready(match arr.as_ref().0.get(i) {
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
                                let to_bind = Source::imbue(arg_source.clone().with(types::Iter::new_stream(filtered, deps).into()));
                                if let Err(e) = traits::bind_no_error(ctx, out, to_bind).await {
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
                v => traits::bind_error(ctx, v).into()
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
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::skip_while), func, iter];

    #[derive(Clone)]
    struct SkipWhile {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
        skip: bool,
    }

    ergo_runtime::ImplGenerator!(SkipWhile => |self, ctx| {
        if !self.skip {
            return self.iter.next(ctx).await;
        }

        while let Some(v) = self.iter.next(ctx).await? {
            let res = traits::bind(
                ctx,
                self.func.clone(),
                Source::imbue(
                    self.args_source.clone().with(
                        types::Args {
                            args: types::args::Arguments::positional(vec![v.clone()])
                                .unchecked(),
                        }
                        .into(),
                    ),
                ),
            )
            .await;

            let b = traits::into::<types::Bool>(ctx, res).await?;
            if !b.as_ref().0 {
                self.skip = false;
                return Ok(Some(v))
            }
        }
        Ok(None)
    });

    let iter = iter.to_owned();
    types::Iter::new(
        SkipWhile {
            iter,
            func,
            args_source: ARGS_SOURCE,
            skip: true,
        },
        deps,
    )
    .into()
}

#[types::ergo_fn]
/// Skip the first `n` consecutive values.
///
/// Arguments: `(Into<Number> :n) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` after the first `n`.
async fn skip(n: _, iter: _) -> Value {
    let n = try_result!(traits::into::<types::Number>(CONTEXT, n).await);
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);
    let deps = depends![nsid!(std::iter::skip), n, iter];

    let n = try_result!(Source::extract(n)
        .map(|n| n.as_ref().to_usize().ok_or("expected unsigned integer"))
        .transpose_err()
        .map_err(|e| e.into_error()));

    #[derive(Clone)]
    struct Skip {
        iter: types::Iter,
        n: usize,
    }

    ergo_runtime::ImplGenerator!(Skip => |self, ctx| {
        while self.n > 0 {
            if let None = self.iter.next(ctx).await? {
                return Ok(None);
            }
            self.n -= 1;
        }
        self.iter.next(ctx).await
    });

    let iter = iter.to_owned();
    types::Iter::new(Skip { iter, n }, deps).into()
}

#[types::ergo_fn]
/// Take the consecutive values for which a function returns true.
///
/// Arguments: `(Function :func) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the values from `iter` preceding the first
/// value for which `func` returned a value which was `false` when converted to Bool.
async fn take_while(func: _, iter: _) -> Value {
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::take_while), func, iter];

    #[derive(Clone)]
    struct TakeWhile {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
    }

    ergo_runtime::ImplGenerator!(TakeWhile => |self, ctx| {
        if let Some(v) = self.iter.next(ctx).await? {
            let res = traits::bind(
                ctx,
                self.func.clone(),
                Source::imbue(
                    self.args_source.clone().with(
                        types::Args {
                            args: types::args::Arguments::positional(vec![v.clone()])
                                .unchecked(),
                        }
                        .into(),
                    ),
                ),
            )
            .await;

            let b = traits::into::<types::Bool>(ctx, res).await?;
            Ok(if b.as_ref().0 {
                Some(v)
            } else {
                None
            })
        } else {
            Ok(None)
        }
    });

    let iter = iter.to_owned();
    types::Iter::new(
        TakeWhile {
            iter,
            func,
            args_source: ARGS_SOURCE,
        },
        deps,
    )
    .into()
}

#[types::ergo_fn]
/// Take the first `n` consecutive values.
///
/// Arguments: `(Into<Number> :n) (Into<Iter> :iter)`
///
/// Returns a new iterator containing only the first `n` values from `iter`.
async fn take(n: _, iter: _) -> Value {
    let n = try_result!(traits::into::<types::Number>(CONTEXT, n).await);
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::take), n, iter];

    let n = try_result!(Source::extract(n)
        .map(|n| n.as_ref().to_usize().ok_or("expected unsigned integer"))
        .transpose_err()
        .map_err(|e| e.into_error()));

    #[derive(Clone)]
    struct Take {
        iter: types::Iter,
        n: usize,
    }

    ergo_runtime::ImplGenerator!(Take => |self, ctx| {
        if self.n > 0 {
            self.n -= 1;
            self.iter.next(ctx).await
        } else {
            Ok(None)
        }
    });

    let iter = iter.to_owned();
    types::Iter::new(Take { iter, n }, deps).into()
}

#[types::ergo_fn]
/// Flatten the values of an iterator.
///
/// Arguments: `(Into<Iter> :iter)`
///
/// Returns a new iterator with each subsequent nested value in the values of `iter`. The values of
/// `iter` must be `Into<Iter>` themselves.
async fn flatten(iter: _) -> Value {
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::flatten), iter];

    #[derive(Clone)]
    struct Flatten {
        iter: types::Iter,
        current: Option<types::Iter>,
    }

    ergo_runtime::ImplGenerator!(Flatten => |self, ctx| {
        loop {
            match &mut self.current {
                None => match self.iter.next(ctx).await? {
                    Some(v) => {
                        self.current = Some(traits::into::<types::Iter>(ctx, v).await?.to_owned());
                    }
                    None => break Ok(None),
                },
                Some(v) => match v.next(ctx).await? {
                    Some(v) => break Ok(Some(v)),
                    None => {
                        self.current = None;
                    }
                },
            }
        }
    });

    let iter = iter.to_owned();
    types::Iter::new(
        Flatten {
            iter,
            current: None,
        },
        deps,
    )
    .into()
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
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::map), iter];

    let vals: Vec<_> = try_result!(iter.to_owned().collect(CONTEXT).await);
    let new_iter = CONTEXT
        .task
        .join_all(vals.into_iter().map(|d| {
            traits::bind(
                CONTEXT,
                func.clone(),
                Source::imbue(
                    ARGS_SOURCE.clone().with(
                        types::Args {
                            args: types::args::Arguments::positional(vec![d]).unchecked(),
                        }
                        .into(),
                    ),
                ),
            )
            .map(Ok)
        }))
        .await
        .unwrap()
        .into_iter();

    types::Iter::new_iter(new_iter, deps).into()
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
    let iter = try_result!(traits::into::<types::Iter>(CONTEXT, iter).await);

    let deps = depends![nsid!(std::iter::map_lazy), iter];

    #[derive(Clone)]
    struct Map {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
    }

    ergo_runtime::ImplGenerator!(Map => |self, ctx| {
        match self.iter.next(ctx).await? {
            None => Ok(None),
            Some(v) => {
                Ok(Some(traits::bind(
                    ctx,
                    self.func.clone(),
                    Source::imbue(
                        self.args_source.clone().with(
                            types::Args {
                                args: types::args::Arguments::positional(vec![v]).unchecked(),
                            }
                            .into(),
                        ),
                    ),
                )
                .await))
            }
        }
    });

    let iter = iter.to_owned();
    types::Iter::new(
        Map {
            iter,
            func,
            args_source: ARGS_SOURCE,
        },
        deps,
    )
    .into()
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
