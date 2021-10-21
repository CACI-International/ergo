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
    let bindings = bindings.to_owned().0;

    let result = Context::fork(
        // Do not propagate errors while trying the bindings
        |ctx| ctx.error_scope = ergo_runtime::context::ErrorScope::new(|_| ()),
        async move {
            let orig_source = Source::get(&value);
            drop(Context::eval(&mut value).await);
            let mut err_labels = Vec::new();
            for b in bindings {
                let b_source = Source::get(&b);
                let result = traits::bind(b, value.clone()).await;
                match result.as_type::<types::Error>() {
                    Ok(err) => {
                        let diagnostics = ergo_runtime::error::Diagnostics::from(err.as_ref());
                        'labelled: for d in &diagnostics {
                            for l in &d.labels {
                                if !l.secondary && b_source.contains(&l.label) {
                                    err_labels.push(l.label.clone().with(
                                        if l.label.value().is_empty() {
                                            d.message.to_string()
                                        } else {
                                            format!("{}: {}", d.message, l.label.value())
                                        },
                                    ));
                                    break 'labelled;
                                }
                            }
                            if d.severity == ergo_runtime::error::Severity::Error {
                                err_labels.push(b_source.with(d.message.to_string()));
                                break 'labelled;
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
                Ok(e) => e.to_owned(),
                Err(v) => {
                    use ergo_runtime::error::{Diagnostic, DiagnosticInfo};
                    let tp = traits::type_name(&v);
                    let mut diag = Diagnostic::from("no bindings matched the value")
                        .add_primary_label(orig_source.with(""))
                        .add_secondary_label(Source::get_origin(&v).with("value created here"))
                        .add_note(match traits::to_string(v).await {
                            Ok(s) => {
                                if s.len() > 80 {
                                    format!("value was {}: `{}...`", tp, &s[..80])
                                } else {
                                    format!("value was {}: `{}`", tp, s)
                                }
                            }
                            Err(_) => format!("value was {}", tp),
                        });
                    for e in err_labels {
                        diag = diag.add_secondary_label(e);
                    }
                    diag.into()
                }
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
