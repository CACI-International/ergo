//! The match function.

use ergo_runtime::{error::PatternError, traits, types, Value};

#[types::ergo_fn]
/// Attempt to match a value over multiple bindings.
///
/// Arguments: `:value (Array :bindings)`
///
/// Returns the value resulting from the first value in `bindings` that doesn't produce a pattern
/// error when bound with `value`.
pub async fn function(mut value: _, bindings: types::Array) -> Value {
    let (bindings_source, bindings) = bindings.take();
    let bindings = bindings.to_owned().0;

    // Do not propagate errors while trying the bindings
    let ctx = CONTEXT.with_error_handler(|_| ());
    let mut had_error = None;
    for b in bindings {
        let result = traits::bind(&ctx, b, value.clone()).await.unwrap();
        match result.as_type::<types::Error>() {
            Ok(err) => {
                if !PatternError::only_pattern_errors(err.as_ref()) {
                    had_error = Some(err.to_owned());
                    break;
                }
            }
            Err(v) => return v,
        }
    }

    let err = match had_error {
        Some(e) => e,
        None => {
            if let Err(e) = CONTEXT.eval(&mut value).await {
                e
            } else {
                bindings_source
                    .with("no bindings matched the value")
                    .into_error()
            }
        }
    };

    CONTEXT.error_scope.error(&err);
    err.into()
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
