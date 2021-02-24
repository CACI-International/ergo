//! Functions over collections.

use abi_stable::std_types::RVec;
use ergo_runtime::{ergo_function, traits, types, Arguments, ContextExt};
use futures::future::{ok, FutureExt, TryFutureExt};
use grease::{depends, make_value, match_value, value::Value};

pub fn module() -> Value {
    crate::grease_string_map! {
        "entries" = entries_fn(),
        "fold" = fold_fn(),
        "get" = get_fn(),
        "has" = has_fn(),
        "map" = map_fn()
    }
}

fn fold_fn() -> Value {
    ergo_function!(std::collection::fold,
    r"Apply a function over each value in an array.

Arguments: <function> <base value> <array>
The function will be applied on each value in the array from first to last, specifically as
`f <current value> <array value>`. The value the function produces will be the `<current value>` used in the next call.
The final value will be the last `<current value>`, which may be the `<base value>` if there are no elements in the
array.",
    |ctx,args| {
        let func = args.next().ok_or("fold function not provided")?;
        let orig = args.next().ok_or("fold base value not provided")?;
        let vals = args.next().ok_or("fold values not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let vals = ctx.source_value_as::<types::Array>(vals);
        let vals = vals.await?;

        let deps = depends![*func.value, *orig, *vals];

        Value::dyn_new(
            async move {
                let (vals_source, vals) = vals.take();
                let vals = vals.await?;
                let val = vals.owned().0.into_iter().fold(ok(orig).boxed(), move |acc, v| {
                    let src = vals_source.clone();
                    let func = func.clone();
                    acc.and_then(move |accv| {
                        let func = func.clone();
                        let src = src.clone();
                        async move {
                            func.bind(src.clone().with(types::Args {
                                args: Arguments::positional(vec![accv, src.clone().with(v)]).unchecked()
                            }.into())).await
                        }
                    })
                    .boxed()
                });

                Ok(val.await?.unwrap().into_any_value())
            },
            deps,
        )
    })
    .into()
}

fn map_fn() -> Value {
    ergo_function!(std::collection::map,
    r"Apply a function to each element in an array.

Arguments: <function> <array>
Returns a new array where each element is the result of applying `<function>` on the corresponding element in `<array>`.",
    |ctx,args| {
        let func = args.next().ok_or("map function not provided")?;
        let arr = args.next().ok_or("map array not provided")?;

        args.unused_arguments()?;

        let func = traits::delay_bind(ctx, func).await?;

        let arr = ctx.source_value_as::<types::Array>(arr);
        let arr = arr.await?;

        let task = ctx.task.clone();
        make_value!([*func.value,*arr] {
            let (arr_source, arr) = arr.take();
            let arr = arr.await?;
            let arr = task.join_all(arr.owned().0
                .into_iter()
                .map(|d| func.bind(
                            arr_source.clone().with(types::Args {
                                args: Arguments::positional(vec![arr_source.clone().with(d)]).unchecked()
                            }.into())
                        )));

            Ok(types::Array(arr.await?.into_iter().map(|v| v.unwrap()).collect()))
        }).into()
    }).into()
}

fn entries_fn() -> Value {
    ergo_function!(std::collection::entries,
    r"Get the entries of a map.

Arguments: <map>
Returns an array where each element is a map with `key` and `value` set to a key-value pair in `<map>`.

For example, `entries {a=1,b=2}` evalutes to `[{key=a,value=1},{key=b,value=2}]`.",
    |ctx,args| {
        let value = args.next().ok_or("map not provided")?;

        args.unused_arguments()?;

        let map = ctx.source_value_as::<types::Map>(value);
        let map = map.await?.unwrap();

        make_value!([map] {
            let mut vals = RVec::new();
            for (k,v) in map.await?.owned().0 {
                vals.push(crate::grease_string_map! {
                    "key" = k,
                    "value" = v
                });
            }

            Ok(types::Array(vals))
        })
        .into()
    })
    .into()
}

fn has_fn() -> Value {
    ergo_function!(
        std::collection::has,
        r"Return whether a map or array has a specific index.

Arguments: <map-or-array> <index>",
        |ctx, args| {
            let value = args.next().ok_or("value not provided")?.unwrap();
            let index = args.next().ok_or("index not provided")?;

            args.unused_arguments()?;

            use ergo_runtime::context_ext::AsContext;
            let ctx: grease::runtime::Context = ctx.as_context().clone();

            make_value!([value, *index] {
                match_value!(value => {
                    types::Array => |val| {
                        let index = ctx.source_value_as::<types::String>(index);
                        let index = index.await?.unwrap().await?;
                        use std::str::FromStr;
                        let i = usize::from_str(index.as_ref())?;
                        types::Bool(i < val.await?.0.len())
                    }
                    types::Map => |val| {
                        types::Bool(val.await?.0.get(&index.unwrap()).is_some())
                    }
                    => |_| Err("non-indexable value")?
                }).await
            })
            .into()
        }
    )
    .into()
}

fn get_fn() -> Value {
    ergo_function!(std::collection::get,
    r"Get an index in a map or array.

Arguments: <map-or-array> <index>
Unlike normal indexing, this returns `()` if the index does not exist.",
    |ctx,args| {
        let value = args.next().ok_or("value not provided")?.unwrap();
        let index = args.next().ok_or("index not provided")?;

        args.unused_arguments()?;

        use ergo_runtime::context_ext::AsContext;
        let ctx: grease::runtime::Context = ctx.as_context().clone();

        let deps = depends![value, *index];

        Value::dyn_new(
            async move {
                match_value!(value => {
                    types::Array => |val| {
                        let index = ctx.source_value_as::<types::String>(index);
                        let index = index.await?.unwrap().await?;
                        use std::str::FromStr;
                        let i = usize::from_str(index.as_ref())?;
                        val.await?.0.get(i).cloned().unwrap_or(types::Unit.into()).into_any_value()
                    }
                    types::Map => |val| {
                        val.await?.0.get(&index.unwrap()).cloned().unwrap_or(types::Unit.into()).into_any_value()
                    }
                    => |_| Err("non-indexable value")?
                })
                .await
            },
            deps,
        )
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn entries(t) {
            // This test only has one entry to not rely on ordering.
            t.assert_content_eq("self:collection:entries {:a = 1}", "[{:key = a, :value = 1}]");
        }
    }

    ergo_script::test! {
        fn fold(t) {
            t.assert_content_eq("self:collection:fold (fn :r :a -> [:a,^:r]) [init] [a,b,c]", "[c,b,a,init]");
        }
    }

    ergo_script::test! {
        fn get(t) {
            t.assert_content_eq("self:collection:get {:a = 1} a", "1");
            t.assert_content_eq("self:collection:get {:a = 1} b", "()");
            t.assert_content_eq("self:collection:get [a] 0", "a");
            t.assert_content_eq("self:collection:get [a] 1", "()");
        }
    }

    ergo_script::test! {
        fn has(t) {
            t.assert_content_eq("if (self:collection:has {:a = 1} a) t f", "t");
            t.assert_content_eq("if (self:collection:has {:a = 1} b) t f", "f");
            t.assert_content_eq("if (self:collection:has [a] 0) t f", "t");
            t.assert_content_eq("if (self:collection:has [a] 1) t f", "f");
        }
    }

    ergo_script::test! {
        fn map(t) {
            t.assert_content_eq("self:collection:map (fn :a -> { :mapped = :a }) [2,3]", "[{:mapped = 2},{:mapped = 3}]");
        }
    }
}
