//! The match function.

use ergo_runtime::{metadata::Source, traits, types, Context, Value};

#[types::ergo_fn]
/// Attempt to match a value over multiple bindings.
///
/// Arguments: `:value (Array :bindings)`
///
/// Keyed Arguments:
/// * `:fallback` - A fallback value to bind the match value to if no other bindings match. This
///   differs from adding a final `:a -> ...` case in error handling: if binding to the fallback
///   value results in an error it is returned, whereas if binding to an equivalent final case
///   results in an error it is interpreted as not matching (which results in a generic error).
/// * `:allow-error` - If present, Error values will be allowed to be matched (normally they will
///   be propagated immediately).
///
/// Returns the value resulting from the first value in `bindings` that doesn't produce an Error
/// when bound with `value`.
pub async fn function(
    mut value: _,
    bindings: types::Array,
    (fallback): [_],
    (allow_error): [_],
) -> Value {
    let bindings = bindings.into_owned().0;
    let allow_error = allow_error.is_some();

    // Do not propagate errors while trying the bindings
    let result = Context::ignore_errors(async move {
        let eval_result = Context::eval(&mut value).await;
        if allow_error {
            drop(eval_result)
        } else {
            eval_result?;
        }
        let value_source = Source::get(&value);
        let mut err_labels = Vec::new();
        for (i, b) in bindings.into_iter().enumerate() {
            let b_source = Source::get(&b);
            let result = traits::bind(b, value.clone()).await;
            match result.as_type::<types::Error>() {
                Ok(err) => {
                    let diagnostics = ergo_runtime::error::Diagnostics::from(err.as_ref());
                    let mut has_primary = false;
                    for d in &diagnostics {
                        for l in &d.labels {
                            // Disregard labels on the main value, they will stack up quickly.
                            if ergo_runtime::source::Source::total_eq(
                                &l.label.source(),
                                &value_source,
                            ) {
                                continue;
                            }
                            if !has_primary && !l.secondary && b_source.contains(&l.label) {
                                err_labels.push(ergo_runtime::error::Label {
                                    label: l.label.clone().with(
                                        if l.label.value().is_empty() {
                                            format!("({}) {}", i + 1, d.message)
                                        } else {
                                            format!(
                                                "({}) {}: {}",
                                                i + 1,
                                                d.message,
                                                l.label.value()
                                            )
                                        }
                                        .into(),
                                    ),
                                    secondary: false,
                                });
                                has_primary = true;
                            } else {
                                err_labels.push(ergo_runtime::error::Label {
                                    label: l
                                        .label
                                        .as_ref()
                                        .map(|s| format!("({}) {}", i + 1, s).into()),
                                    secondary: true,
                                });
                            }
                        }
                        if !has_primary && d.severity == ergo_runtime::error::Severity::Error {
                            err_labels.push(ergo_runtime::error::Label {
                                label: b_source.with(format!("({}) {}", i + 1, d.message).into()),
                                secondary: false,
                            });
                            has_primary = true;
                        }
                    }
                }
                Err(v) => return Ok(v),
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
            Ok(e) => e.into_owned(),
            Err(v) => {
                use ergo_runtime::error::{Diagnostic, DiagnosticInfo};
                let mut diag = Diagnostic::from("no bindings matched the value")
                    .add_value_info("value", &v)
                    .await;
                for l in err_labels {
                    diag.labels.push(l);
                }
                diag.into()
            }
        };
        Err(err)
    })
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
            t.assert_eq("self:match [1,2,3] [{^:keys} -> $keys, [:a,:b] -> $a, [:a,:b,:c] -> $c]", "3");
            t.assert_eq("self:match [1,2,3] [:a -> $a, [:a,:b,:c] -> $b]", "[1,2,3]");
            t.assert_eq("self:match str [a -> a, str -> success]", "success");
        }

        fn match_error(t) {
            t.assert_fail("self:match (self:Error:new doh) [a -> a]");
            t.assert_eq("self:match ~allow-error (self:Error:new doh) [self:Error _ -> error]", "error");
        }

        fn match_failure(t) {
            t.assert_fail("self:match {:a=[1,2]} [{:b} -> $b, [:a,:b] -> $b]");
        }

        fn match_case_body_error(t) {
            t.assert_eq("self:Type:get ~allow-error <| self:match 1 [1 -> self:Error:new NO, 1 -> 2]", "self:Error");
        }

        fn match_case_body_bind_failure(t) {
            t.assert_fail("self:match 1 [1 -> {fn x -> () |> y}, 1 -> 1]");
        }

        fn match_fallback(t) {
            t.assert_eq("a = _ -> self:Error:new doh; self:Error _ = self:match (fallback=$a) hi [p -> ()]; ()", "()");
        }
    }
}
