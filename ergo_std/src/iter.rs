//! Iterator functions.

use ergo_runtime::{
    depends, error::DiagnosticInfo, metadata::Source, nsid, traits, try_result, types,
    value::match_value, Context, Value,
};
use futures::{
    future::{ready, FutureExt},
    stream::StreamExt,
};
use std::collections::BTreeSet;

pub fn module() -> Value {
    crate::make_string_map! {
        "count" = count(),
        "filter" = filter(),
        "flatten" = flatten(),
        "fold" = fold(),
        "from" = from(),
        "map" = map(),
        "map-lazy" = map_lazy(),
        "no-errors" = no_errors(),
        "order" = order(),
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
    traits::into::<types::Iter>(value).await?.into()
}

#[types::ergo_fn]
/// Check that an iterator contains no errors.
///
/// Arguments: `(Into<Iter> :iter)`
///
/// Returns either the original Iter, or an aggregate Error if the iterator contained any errors.
async fn no_errors(iter: _) -> Value {
    let iter = traits::into::<types::Iter>(iter).await?;

    let vals = iter.clone().to_owned().collect::<Vec<_>>().await?;

    let mut errs = Vec::new();
    for v in vals {
        match v.as_type::<types::Error>() {
            Ok(err) => errs.push(err.to_owned()),
            _ => (),
        }
    }

    if errs.is_empty() {
        let mut iter = iter;
        let deps = depends![nsid!(std::iter::no_errors::result), iter];
        iter.set_dependencies(deps);
        iter.into()
    } else {
        types::Error::aggregate(errs).into()
    }
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
    let iter = traits::into::<types::Iter>(iter).await?;

    // FIXME early exit on error?

    let mut acc = acc;
    let vals = iter.to_owned().collect::<Vec<_>>().await?;
    for v in vals {
        acc = traits::bind(
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::unique), iter];

    let iter = iter.to_owned();

    #[derive(Clone)]
    struct Unique {
        iter: types::Iter,
        seen: BTreeSet<u128>,
    }

    ergo_runtime::ImplGenerator!(Unique => |self| {
        while let Some(v) = self.iter.next().await? {
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::filter), func, iter];

    let iter = iter.to_owned();

    #[derive(Clone)]
    struct Filter {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
    }

    ergo_runtime::ImplGenerator!(Filter => |self| {
        while let Some(v) = self.iter.next().await? {
            let res = traits::bind(
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

            let b = traits::into::<types::Bool>(res).await?;
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
    types::Unbound::new(|arg| {
        async move {
            let arg_source = Source::get(&arg);
            match_value! { arg,
                types::Args { mut args } => {
                    let iters: Vec<_> = (&mut args).collect();

                    try_result!(args.unused_arguments());

                    let iters_typed = try_result!(Context::global().task.join_all(
                            iters.into_iter().map(|i| traits::into::<types::Iter>(i))
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

                        ergo_runtime::ImplGenerator!(Zip => |self| {
                            let mut arr = abi_stable::std_types::RVec::with_capacity(self.iters.len());
                            for v in self.iters.iter_mut() {
                                match v.next().await? {
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

                    types::Unbound::new_no_doc(move |arg| {
                        let iter_outs = iter_outs.clone();
                        async move {
                            let arg = try_result!(Context::eval_as::<types::Iter>(arg).await);
                            let arg_source = Source::get(&arg);
                            let iter_id = arg.id();
                            let iter = arg.to_owned();

                            let items: Vec<_> = try_result!(iter.collect().await);
                            let arrays = try_result!(Context::global().task.join_all(items.into_iter().map(|v| Context::eval_as::<types::Array>(v))).await);

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
                                if let Err(e) = traits::bind_no_error(out, to_bind).await {
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
                v => traits::bind_error(v).into()
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::skip_while), func, iter];

    #[derive(Clone)]
    struct SkipWhile {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
        skip: bool,
    }

    ergo_runtime::ImplGenerator!(SkipWhile => |self| {
        if !self.skip {
            return self.iter.next().await;
        }

        while let Some(v) = self.iter.next().await? {
            let res = traits::bind(
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

            let b = traits::into::<types::Bool>(res).await?;
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
    let n = traits::into::<types::Number>(n).await?;
    let iter = traits::into::<types::Iter>(iter).await?;
    let deps = depends![nsid!(std::iter::skip), n, iter];

    let n = n
        .as_ref()
        .to_usize()
        .add_primary_label(Source::get(&n).with("expected this to be unsigned integer"))?;

    #[derive(Clone)]
    struct Skip {
        iter: types::Iter,
        n: usize,
    }

    ergo_runtime::ImplGenerator!(Skip => |self| {
        while self.n > 0 {
            if let None = self.iter.next().await? {
                return Ok(None);
            }
            self.n -= 1;
        }
        self.iter.next().await
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::take_while), func, iter];

    #[derive(Clone)]
    struct TakeWhile {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
    }

    ergo_runtime::ImplGenerator!(TakeWhile => |self| {
        if let Some(v) = self.iter.next().await? {
            let res = traits::bind(
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

            let b = traits::into::<types::Bool>(res).await?;
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
    let n = traits::into::<types::Number>(n).await?;
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::take), n, iter];

    let n = n
        .as_ref()
        .to_usize()
        .add_primary_label(Source::get(&n).with("expected this to be an unsigned integer"))?;

    #[derive(Clone)]
    struct Take {
        iter: types::Iter,
        n: usize,
    }

    ergo_runtime::ImplGenerator!(Take => |self| {
        if self.n > 0 {
            self.n -= 1;
            self.iter.next().await
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::flatten), iter];

    #[derive(Clone)]
    struct Flatten {
        iter: types::Iter,
        current: Option<types::Iter>,
    }

    ergo_runtime::ImplGenerator!(Flatten => |self| {
        loop {
            match &mut self.current {
                None => match self.iter.next().await? {
                    Some(v) => {
                        self.current = Some(traits::into::<types::Iter>(v).await?.to_owned());
                    }
                    None => break Ok(None),
                },
                Some(v) => match v.next().await? {
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let vals: Vec<_> = iter.to_owned().collect().await?;
    let new_iter = Context::global()
        .task
        .join_all(vals.into_iter().map(|d| {
            traits::bind(
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
        .unwrap();

    let deps = depends![^@new_iter];

    types::Iter::new_iter(new_iter.into_iter(), deps).into()
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
    let iter = traits::into::<types::Iter>(iter).await?;

    let deps = depends![nsid!(std::iter::map_lazy), func, iter];

    #[derive(Clone)]
    struct Map {
        iter: types::Iter,
        func: Value,
        args_source: ergo_runtime::Source<()>,
    }

    ergo_runtime::ImplGenerator!(Map => |self| {
        match self.iter.next().await? {
            None => Ok(None),
            Some(v) => {
                Ok(Some(traits::bind(
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

#[types::ergo_fn]
/// Count the number of items in an iterator.
///
/// Arguments: `(Into<Iter> :iter)`
///
/// Returns the number of items in the iterator as a Number.
async fn count(iter: _) -> Value {
    let iter = traits::into::<types::Iter>(iter).await?;

    let vals: Vec<_> = iter.to_owned().collect().await?;
    types::Number::from_usize(vals.len()).into()
}

async fn compare(
    cmp: &ergo_runtime::Source<Value>,
    a: &Value,
    b: &Value,
) -> ergo_runtime::Result<std::cmp::Ordering> {
    let result = traits::bind(
        cmp.value().clone(),
        Source::imbue(
            cmp.source().with(
                types::Args {
                    args: types::args::Arguments::positional(vec![a.clone(), b.clone()])
                        .unchecked(),
                }
                .into(),
            ),
        ),
    )
    .await;

    Context::eval_as::<super::cmp::Order>(result)
        .await
        .map(|v| v.to_owned().into())
}

async fn partition(
    v: &mut [Value],
    cmp: &ergo_runtime::Source<Value>,
) -> ergo_runtime::Result<usize> {
    let pivot = v.len() / 2;
    let end = v.len() - 1;
    v.swap(pivot, end);

    let mut part = 0;
    for i in 0..end {
        if compare(cmp, &v[i], &v[end]).await? == std::cmp::Ordering::Less {
            v.swap(i, part);
            part += 1;
        }
    }
    v.swap(part, end);
    Ok(part)
}

fn quicksort<'a>(
    v: &'a mut [Value],
    cmp: &'a ergo_runtime::Source<Value>,
) -> futures::future::BoxFuture<'a, ergo_runtime::Result<()>> {
    futures::future::FutureExt::boxed(async move {
        if v.len() >= 2 {
            let p = partition(v, cmp).await?;
            quicksort(&mut v[..p], cmp).await?;
            quicksort(&mut v[p + 1..], cmp).await?;
        }
        Ok(())
    })
}

#[types::ergo_fn]
/// Order the items in an iterator.
///
/// Arguments: `(Function :f) (Into<Iter> :iter)`
///
/// Uses `f` to order the items in `iter`. `f` is applied to two items at a time, and should return
/// a `std:Order`.
///
/// Returns an iterator with items from `iter` ordered according to `f`.
async fn order(func: _, iter: _) -> Value {
    let iter = traits::into::<types::Iter>(iter).await?;

    let mut vals: Vec<_> = iter.to_owned().collect().await?;

    quicksort(&mut vals, &ARGS_SOURCE.with(func)).await?;

    let deps = depends![^@vals];
    types::Iter::new_iter(vals.into_iter(), deps).into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn count(t) {
            t.assert_content_eq("self:iter:count [a,b,1,2]", "self:number:from 4");
            t.assert_content_eq("self:iter:count []", "self:number:from 0");
        }

        fn fold(t) {
            t.assert_content_eq("self:iter:fold (fn :r :a -> [:a,^:r]) [init] [a,b,c]", "[c,b,a,init]");
        }

        fn filter(t) {
            t.assert_content_eq("self:iter:filter (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,[],c,(),(),d,e]", "self:iter:from [a,b,c,d,e]");
        }

        fn flatten(t) {
            t.assert_content_eq("self:iter:flatten [[a,b],[],[],[c,d,e,f],[g]]", "self:iter:from [a,b,c,d,e,f,g]");
        }

        fn map(t) {
            t.assert_content_eq("self:iter:map (fn :a -> { mapped = :a }) [2,3]", "self:iter:from [{mapped = 2},{mapped = 3}]");
        }

        fn map_lazy(t) {
            t.assert_content_eq("self:iter:map-lazy (fn :a -> { mapped = :a }) [2,3]", "self:iter:from [{mapped = 2},{mapped = 3}]");
        }

        fn order(t) {
            t.assert_content_eq("self:iter:order self:string:compare [b,c,w,d,g,a]", "self:iter:from [a,b,c,d,g,w]");
            t.assert_content_eq("self:iter:order self:string:compare [b,a,a,b,c]", "self:iter:from [a,a,b,b,c]");
            t.assert_content_eq("self:iter:order self:string:compare []", "self:iter:from []");
        }

        fn skip(t) {
            t.assert_content_eq("self:iter:skip 5 [a,b,c,d,e,f,g]", "self:iter:from [f,g]");
            t.assert_content_eq("self:iter:skip 5 [a,b]", "self:iter:from []");
        }

        fn skip_while(t) {
            t.assert_content_eq("self:iter:skip-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,c,d,(),e,f,g]", "self:iter:from [(),e,f,g]");
            t.assert_content_eq("self:iter:skip-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,c,d]", "self:iter:from []");
        }

        fn take(t) {
            t.assert_content_eq("self:iter:take 4 [a,b,c,d,e,f,g]", "self:iter:from [a,b,c,d]");
            t.assert_content_eq("self:iter:take 4 [a]", "self:iter:from [a]");
        }

        fn take_while(t) {
            t.assert_content_eq("self:iter:take-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b,c,d,(),e,f,g]", "self:iter:from [a,b,c,d]");
            t.assert_content_eq("self:iter:take-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [(),e]", "self:iter:from []");
            t.assert_content_eq("self:iter:take-while (fn :v -> self:match :v [self:type:String -> self:bool:true, _ -> self:bool:false]) [a,b]", "self:iter:from [a,b]");
        }

        fn unique(t) {
            t.assert_content_eq("self:iter:unique [1,2,3,2,40,5,6,5]", "self:iter:from [1,2,3,40,5,6]");
        }

        fn zip(t) {
            t.assert_content_eq("self:iter:zip [a,b,c,d] [1,2,3,4]", "self:iter:from [[a,1],[b,2],[c,3],[d,4]]");
            t.assert_content_eq("self:iter:zip [a,b,c,d] [1,2]", "self:iter:from [[a,1],[b,2]]");
            t.assert_content_eq("self:iter:zip [a,b] [1,2,3,4]", "self:iter:from [[a,1],[b,2]]");
            t.assert_content_eq("self:iter:zip ^[]", "self:iter:from []");
            t.assert_content_eq("self:iter:zip [a,b,c]", "self:iter:from [[a],[b],[c]]");
            t.assert_content_eq("self:iter:zip [a,b] [1,2] [x,y,z]", "self:iter:from [[a,1,x],[b,2,y]]");
            t.assert_content_eq("self:iter:zip :x :y = self:iter:from [[a,1],[b,2]]; [:x, :y]", "[self:iter:from [a,b],self:iter:from [1,2]]");
            t.assert_content_eq(
                "self:iter:zip :x :y :z = self:iter:from [[a,1,x],[b,2],[c,3,y,q],[d,:unset,z]]; [:x, :y, :z]",
                "[self:iter:from [a,b,c,d],self:iter:from [1,2,3],self:iter:from [x,y,z]]"
            );
        }
    }
}
