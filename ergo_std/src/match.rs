//! The match function.

use ergo_runtime::{metadata::Source, traits, types, Context, Value};

#[types::ergo_fn]
/// Attempt to match a value over multiple bindings.
///
/// Arguments: `:value (Array :bindings)`
///
/// Keyed Arguments:
/// * `:fallback` - A fallback value to bind the match value to if no other bindings match. This
/// differs from adding a final `:a -> ...` case in error handling: if binding to the fallback
/// value results in an error it is returned, whereas if binding to an equivalent final case
/// results in an error it is interpreted as not matching (which results in a generic error).
///
/// Returns the value resulting from the first value in `bindings` that doesn't produce an error
/// when bound with `value`.
pub async fn function(mut value: _, bindings: types::Array, (fallback): [_]) -> Value {
    let bindings_source = Source::get(&bindings);
    let bindings = bindings.to_owned().0;

    let result = Context::fork(
        // Do not propagate errors while trying the bindings
        |ctx| ctx.error_scope = ergo_runtime::context::ErrorScope::new(|_| ()),
        async move {
            drop(Context::eval(&mut value).await);
            for b in bindings {
                let result = traits::bind(b, value.clone()).await;
                if !result.is_type::<types::Error>() {
                    return Ok(result);
                }
            }

            // If a fallback is provided, bind it prior to inspecting the argument.
            if let Some(fallback) = fallback {
                return Ok(ergo_runtime::try_value!(
                    traits::bind(fallback, value).await
                ));
            }

            // If the value is an error and no bindings match, return it directly.
            let err = match value.as_type::<types::Error>() {
                Ok(e) => e.to_owned(),
                Err(v) => ergo_runtime::error! {
                    labels: [
                        primary(Source::get(&v).with("")),
                        secondary(bindings_source.with("bindings which failed to match"))
                    ],
                    error: "no bindings matched the value"
                },
            };
            Err(err)
        },
    )
    .await;

    match result {
        Ok(v) => v,
        Err(e) => {
            Context::with(|ctx| ctx.error_scope.error(&e));
            e.into()
        }
    }
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn match_expr(t) {
            t.assert_content_eq("self:match [1,2,3] [{^:keys} -> :keys, [:a,:b] -> :a, [:a,:b,:c] -> :c]", "3");
            t.assert_content_eq("self:match [1,2,3] [:a -> :a, [:a,:b,:c] -> :b]", "[1,2,3]");
            t.assert_content_eq("self:match str [a -> a, str -> success]", "success");
        }

        fn match_failure(t) {
            t.assert_fail("self:match {:a=[1,2]} [{b} -> :b, [:a,:b] -> :b]");
        }

        fn match_case_body_failure(t) {
            t.assert_eq("self:match 1 [1 -> !self:types:Error: NO, 1 -> 2]", "2");
        }

        fn match_case_body_bind_failure(t) {
            t.assert_fail("self:match 1 [1 -> {fn x -> () |> y}, 1 -> 1]");
        }

        fn match_fallback(t) {
            t.assert_content_eq("a = _ -> !self:type:Error: doh; self:type:Error _ = self:match (fallback=:a) hi []; ()", "()");
            t.assert_fail("a = _ -> !self:type:Error: doh; self:match hi [:a]; ()");
        }
    }
}
