//! The match function.

use ergo_runtime::{metadata::Source, traits, types, Value};

#[types::ergo_fn]
/// Attempt to match a value over multiple bindings.
///
/// Arguments: `:value (Array :bindings)`
///
/// Returns the value resulting from the first value in `bindings` that doesn't produce a pattern
/// error when bound with `value`.
pub async fn function(mut value: _, bindings: types::Array) -> Value {
    let bindings_source = Source::get(&bindings);
    let bindings = bindings.to_owned().0;

    let result = CONTEXT
        .fork(
            // Do not propagate errors while trying the bindings
            |ctx| ctx.error_scope = ergo_runtime::context::ErrorScope::new(|_| ()),
            |ctx| async move {
                let log = ctx.log.sublog("match");

                drop(ctx.eval(&mut value).await);
                for b in bindings {
                    let b_src = Source::get(&b);
                    let result = traits::bind(&ctx, b, value.clone()).await;
                    match result.as_type::<types::Error>() {
                        Ok(err) => {
                            log.debug(
                                b_src
                                    .with("didn't match because of error when binding")
                                    .into_error()
                                    .with_context(err.to_owned()),
                            );
                        }
                        Err(v) => return Ok(v),
                    }
                }

                let err = match value.as_type::<types::Error>() {
                    Ok(e) => e.to_owned(),
                    Err(v) => Source::get(&v)
                        .with("no bindings matched the value")
                        .into_error()
                        .with_context(bindings_source.with("bindings which failed to match")),
                };
                Err(err)
            },
        )
        .await;

    match result {
        Ok(v) => v,
        Err(e) => {
            CONTEXT.error_scope.error(&e);
            e.into()
        }
    }
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn match_expr(t) {
            t.assert_content_eq("self:match [1,2,3] [{^:keys} -> :keys, [:a,:b] -> :a, [:a,:b,:c] -> :c]", "3");
            t.assert_content_eq("self:match [1,2,3] [:a -> :a, [:a,:b,:c] -> :b]", "[1,2,3]");
            t.assert_content_eq("self:match str [a -> a, str -> success]", "success");
        }
    }

    ergo_script::test! {
        fn match_failure(t) {
            t.assert_fail("self:match {:a=[1,2]} [{b} -> :b, [:a,:b] -> :b]");
        }
    }

    ergo_script::test! {
        fn match_case_body_failure(t) {
            //TODO
            t.assert_fail("self:match 1 [1 -> self:error:throw NO, 1 -> 1]");
        }
    }

    ergo_script::test! {
        fn match_case_body_bind_failure(t) {
            t.assert_fail("self:match 1 [1 -> {fn x -> () |> y}, 1 -> 1]");
        }
    }
}
