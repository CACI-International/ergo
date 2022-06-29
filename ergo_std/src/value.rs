//! Value-related intrinsics.

use ergo_runtime::{
    metadata::{Runtime, Source},
    nsid, traits, types, Context, Value,
};
use futures::future::{BoxFuture, FutureExt};

pub fn module() -> Value {
    crate::make_string_map! {
        "debug" = debug(),
        "dynamic" = crate::make_string_map! {
            "get" = dynamic_binding_get(),
            "eval" = dynamic_binding_set()
        },
        "equal" = equal(),
        "identity" = identity(),
        "index" = index(),
        "merge" = merge(),
        "meta" = crate::make_string_map! {
            "eval" = meta_eval(),
            "get" = meta_get(),
            "set" = meta_set()
        },
        "source-copy" = source_copy(),
        "source-path" = source_path(),
        "variable" = variable()
    }
}

#[types::ergo_fn]
#[eval_for_id]
/// Change the identity of a value.
///
/// Arguments: `:value`
///
/// Keyed Arguments:
/// * `:depends`: A value whose identity will be used to set the identity of the returned value.
///
/// Returns a value identical to the argument, but with a different identity. If `depends` is not set, the value will have
/// a fixed identity derived from nothing else.
async fn variable(mut value: _, (depends): [_]) -> Value {
    if let Some(v) = depends {
        value.set_identity(v);
    } else {
        value.set_identity(0);
    };

    value
}

#[types::ergo_fn]
/// Print a value's type, identity, and (if typed) display output to the debug log.
///
/// Arguments: `:value`
///
/// Returns the argument as-is.
async fn debug(value: _) -> Value {
    let name = traits::type_name(&value);

    let rest = if value.is_evaluated() {
        let mut s = String::from(", value: ");
        {
            let mut formatter = traits::Formatter::new(&mut s);
            if let Err(e) = traits::display(value.clone(), &mut formatter).await {
                drop(formatter);
                s.push_str("<error: ");
                s.push_str(&e.to_string());
                s.push('>');
            }
        }
        s
    } else {
        Default::default()
    };

    Context::global().log.debug(
        Source::get(&value)
            .with(format!(
                "type: {}{}, id: {:032x}",
                name,
                rest,
                value.id().await
            ))
            .to_string(),
    );

    value
}

#[types::ergo_fn]
/// Get the identity of a value.
///
/// Arguments: `:value`
///
/// Returns the identity as a 32-character hex string.
async fn identity(value: _) -> Value {
    types::String::from(format!("{:032x}", value.id().await)).into()
}

#[types::ergo_fn]
/// Evaluate a value until certain metadata is available.
///
/// Arguments: `:metadata-key :value`
///
/// Returns a map with the following keys:
/// * `metadata-value` - the associated metadata value, if any
/// * `result` - the evaluated value that had the metadata value, or the final value resulting from
/// evaluation if no matching metadata was found.
async fn meta_eval(metadata_key: _, mut value: _) -> Value {
    let key = metadata_key.id().await;
    while value.get_metadata(&Runtime { key }).is_none() && value.eval_once().await {}
    crate::make_string_map! { source ARGS_SOURCE,
        "metadata-value" = value.get_metadata(&Runtime { key }).map(|v| v.as_ref().clone()).unwrap_or_else(|| types::Unset.into()),
        "result" = value
    }
}

#[types::ergo_fn]
/// Get metadata of a value.
///
/// Arguments: `:metadata-key :value`
///
/// Returns the metadata value or `Unset` if no key is set.
async fn meta_get(metadata_key: _, value: _) -> Value {
    let key = metadata_key.id().await;
    match value.get_metadata(&Runtime { key }) {
        Some(v) => v.as_ref().clone(),
        None => types::Unset.into(),
    }
}

#[types::ergo_fn]
/// Set metadata of a value.
///
/// Arguments: `:metadata-key :metadata-value :value`
///
/// You may have an `Unset` `metadata-value` to remove a metadata key.
async fn meta_set(metadata_key: _, metadata_value: _, mut value: _) -> Value {
    let key = Runtime {
        key: metadata_key.id().await,
    };

    if metadata_value.is_type::<types::Unset>() {
        value.clear_metadata(&key);
    } else {
        value.set_metadata(&key, metadata_value);
    }
    value
}

#[types::ergo_fn]
/// Get the source path of a value.
///
/// Arguments: `:value`
///
/// Returns the Path of the script file from which the value originates, or Unset if no path is
/// available.
async fn source_path(value: _) -> Value {
    match Source::get_origin_option(&value).and_then(|s| Context::source_path(&s)) {
        None => types::Unset.into(),
        Some(p) => types::Path::from(p).into(),
    }
}

#[types::ergo_fn]
/// Copy the source path of one value to another.
///
/// Arguments: `:from :to`
///
/// Returns `to` with its source set to that of `from`.
async fn source_copy(from: _, mut to: _) -> Value {
    if let Some(src) = Source::get_origin_option(&from) {
        Source::set(&mut to, src);
    } else {
        to.clear_metadata(&Source);
    }
    to
}

#[types::ergo_fn]
#[eval_for_id]
/// Get a dynamic binding.
///
/// Arguments: `:key`
///
/// Returns the dynamic binding corresponding to `key`, or `Unset` if none exists.
async fn dynamic_binding_get(key: _) -> Value {
    let key = key.as_identified().await;
    let mut ret = ergo_runtime::lazy_value! {
        #![id(CALL_DEPENDS)]
        #![eval_for_id]
        match Context::with(|ctx| ctx.dynamic_scope.get(&key)) {
            None => types::Unset.into(),
            Some(r) => (*r).clone().into(),
        }
    };
    ret.impure(true);
    ret
}

// TODO make dynamic bindings a first-class citizen of Value

#[types::ergo_fn]
#[eval_for_id]
/// Evaluate a value with the given dynamic bindings.
///
/// Arguments: `(Map :bindings) :eval`
///
/// Returns the result of evaluating `eval` with all `bindings` set in the dynamic scope.
async fn dynamic_binding_set(bindings: types::Map, mut eval: _) -> Value {
    let entries = bindings.into_owned().0;
    Context::fork(
        |ctx| {
            for (k, v) in entries {
                ctx.dynamic_scope
                    .set(&Source::extract(k.as_identified()), v);
            }
        },
        async move {
            drop(Context::eval(&mut eval).await);
            eval
        },
    )
    .await
}

#[types::ergo_fn]
/// Merge two values together recursively.
///
/// Arguments: `:a :b`
///
/// Keyed Arguments:
/// * `:array-merge` - if present, arrays are element-wise deeply merged rather than appended.
///
/// Returns the result of merging `b` into `a` according to the following rules:
/// * If `a` and `b` are both:
///   * Maps - The entries of `b` are added to those in `a`. When a key is present in both, the
///   values are deeply merged.
///   * Arrays - The values of `b` are appended to the end of those in `a`. If `array-merge` is
///   specified, arrays are element-wise deeply merged.
///   * Args - The keyed arguments are merged as Maps and the positional arguments are merged as
///   Arrays.
/// * Otherwise, `b` is preferred and returned.
async fn merge(a: _, b: _, (array_merge): [_]) -> Value {
    let array_merge = array_merge.is_some();

    struct Merger {
        array_merge: bool,
    }

    use ergo_runtime::abi_stable::{bst::BstMap, std_types::RVec};
    use ergo_runtime::EvaluatedValue;

    impl Merger {
        fn merge<'a>(&'a self, mut a: Value, mut b: Value) -> BoxFuture<'a, Value> {
            async move {
                drop(Context::eval(&mut a).await);
                drop(Context::eval(&mut b).await);

                if a.ergo_type().unwrap() == b.ergo_type().unwrap() {
                    ergo_runtime::value::match_value! { a,
                        types::Map(mut a) => {
                            let types::Map(b) = b.as_type::<types::Map>().unwrap().into_owned();
                            self.merge_map(&mut a, b).await;
                            return types::Map(a).into();
                        },
                        types::Array(mut a) => {
                            let types::Array(b) = b.as_type::<types::Array>().unwrap().into_owned();
                            self.merge_array(&mut a, b).await;
                            return types::Array(a).into();
                        },
                        types::Args { args: mut a } => {
                            let types::Args { args: mut b } = b.as_type::<types::Args>().unwrap().into_owned();
                            self.merge_map(&mut a.keyed, b.keyed).await;
                            // Positional args are stored in reverse order for efficient mutation.
                            a.positional.reverse();
                            b.positional.reverse();
                            self.merge_array(&mut a.positional, b.positional).await;
                            a.positional.reverse();
                            return types::Args { args: a }.into();
                        },
                        _ => ()
                    }
                }
                b
            }
            .boxed()
        }

        async fn merge_map(
            &self,
            a: &mut BstMap<EvaluatedValue, Value>,
            b: BstMap<EvaluatedValue, Value>,
        ) {
            for (k, bv) in b {
                let v = if let Some(av) = a.remove(&k) {
                    self.merge(av, bv).await
                } else {
                    bv
                };
                a.insert(k, v);
            }
        }

        async fn merge_array(&self, a: &mut RVec<Value>, b: RVec<Value>) {
            if self.array_merge {
                let merge_len = std::cmp::min(a.len(), b.len());
                let mut biter = b.into_iter();
                for (av, bv) in a.iter_mut().zip(biter.by_ref().take(merge_len)) {
                    *av = self.merge(av.clone(), bv).await;
                }
                a.extend(biter);
            } else {
                a.extend(b);
            }
        }
    }

    Merger { array_merge }.merge(a, b).await
}

#[types::ergo_fn]
/// Check whether two values are equal.
///
/// Arguments: `:a :b`
///
/// Keyed Arguments:
/// * `:exact` - If present, do not evaluate `a` nor `b` prior to comparison.
///
/// Equality is based on value identity.
///
/// Returns a Bool indicating whether `a` is equal to `b`.
async fn equal(mut a: _, mut b: _, (exact): [_]) -> Value {
    let exact = exact.is_some();

    if !exact {
        drop(Context::eval(&mut a).await);
        drop(Context::eval(&mut b).await);
    }
    types::Bool(a.id().await == b.id().await).into()
}

#[types::ergo_fn]
/// Get indices of a value.
///
/// Arguments: `(Map :indices) -> :value`
///
/// Indexes `value` with each key of `indices`, and binds the result to each corresponding value of
/// `indices`.
async fn index(indices: types::Map) -> Value {
    types::unbound_value! {
        #![depends(const nsid!(std::index))]
        #![contains(indices)]
        Context::eval(&mut ARG).await?;
        for (k, v) in &indices.as_ref().0 {
            let result = traits::bind(
                ARG.clone(),
                Source::copy(&k, types::Index(k.clone().into()).into()),
            )
            .await;
            traits::bind_no_error(v.clone(), result).await?;
        }
        types::Unit.into()
    }
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn source_path(t) {
            t.assert_eq("x = 1; self:value:source-path $x", "$unset");
        }

        fn dynamic_binding(t) {
            t.assert_eq("self:value:dynamic:get something", "$unset");
            t.assert_eq("v = self:value:dynamic:get something; self:value:dynamic:eval { something = value } $v", "value");
            t.assert_eq("f = fn :x -> <| self:value:dynamic:get my_func |> $x
                say_hello = fn :name -> \"hi, $name\"
                self:value:dynamic:eval { my_func = $say_hello } <| f dude", "\"hi, dude\"");
        }

        fn dynamic_binding_multiple_scopes(t) {
            t.assert_eq("the-value = self:value:dynamic:get the-value
                [self:value:dynamic:eval {the-value = 10} $the-value, self:value:dynamic:eval {the-value = hi} $the-value]", "[10,hi]");
        }

        fn meta(t) {
            t.assert_eq("v = self:value:meta:set mkey mvalue value; { result = :v2, ^_ } = self:value:meta:eval mkey $v; self:value:meta:get mkey $v2", "mvalue");
        }

        fn merge(t) {
            t.assert_eq("self:value:merge [1,2] [3,4]", "[1,2,3,4]");
            t.assert_eq("self:value:merge hi ()", "()");
            t.assert_eq("self:value:merge {a = [1,2,3], b = { x = 1, y = 2 }, c = hi} {a = [4], b = { x = 42, z = 3 }}",
                "{a = [1,2,3,4], b = { x = 42, y = 2, z = 3 }, c = hi}");
            t.assert_eq("self:value:merge ~array-merge {a = [{z=1},2,3], b = [1]} {a = [{y=4}], b = [4,5,6]}",
                "{a = [{y=4,z=1},2,3], b = [4,5,6]}");
            t.assert_eq("self:value:merge (fn 1 2 ~key ~a) (fn 3 4 ~key=k ~b)", "fn 1 2 3 4 ~key=k ~a ~b");
        }

        fn equal(t) {
            t.assert_eq("self:value:equal a a", "self:Bool:true");
            t.assert_eq("self:value:equal \"$\"abc\"$\"def\"\" \"$\"a\"bcdef\"", "self:Bool:true");
            t.assert_eq("self:value:equal a b", "self:Bool:false");
            t.assert_eq("self:value:equal a {\"a\"}", "self:Bool:true");
            t.assert_eq("self:value:equal ~exact a {\"a\"}", "self:Bool:false");
            t.assert_eq("self:value:equal ~exact {\"a\"} {\"a\"}", "self:Bool:true");
        }

        fn index(t) {
            t.assert_eq("self:value:index {:a,:b} = {a = 1, b = 2}", "{a = 1, b = 2}");
            t.assert_eq("self:value:index {:a} = {a = 1, b = 2}", "{a = 1}");
            t.assert_eq("self:value:index {:a,:c} = {a = 1, b = 2}", "{a = 1}");
        }
    }
}
