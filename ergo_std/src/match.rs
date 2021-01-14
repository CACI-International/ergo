//! The match function.

use ergo_runtime::{ergo_function, error::BindError, traits};
use grease::{depends, value::Errored, Value};

pub fn function() -> Value {
    ergo_function!(std::match,
        r"Attempt to match a value over multiple bindings.

Arguments: <value> <bindings>...

Returns a dynamic value which will evaluate to the result of binding `value` to the first binding
which does not result in a binding error.",
    |ctx, args|
    {
        let val = args.next().ok_or("missing value")?;
        let mut bindings = Vec::new();
        let mut deps = depends![*val];
        for a in args.by_ref() {
            deps += depends![*a];
            bindings.push(traits::delay_bind(ctx, a).await?);
        }

        args.unused_arguments()?;

        if bindings.is_empty() {
            return Err("no bindings provided".into());
        }

        Value::dyn_new(async move {
            for b in bindings {
                match Errored::ignore(b.bind(val.clone())).await {
                    Ok(v) => return Ok(v.unwrap().into_any_value()),
                    Err(e) => if BindError::only_within(&e) {
                        continue
                    } else {
                        return Err(e)
                    }
                }
            }
            Err(val.with("no bindings matched the value").into_grease_error())
        }, deps)
    })
    .into()
}

#[cfg(test)]
mod test {
    ergo_script::test! {
        fn match_expr(t) {
            t.assert_content_eq("self:match [1,2,3] ({^:keys} -> :keys) ([:a,:b] -> :a) ([:a,:b,:c] -> :c)", "3");
            t.assert_content_eq("self:match [1,2,3] (:a -> :a) ([:a,:b,:c] -> :b)", "[1,2,3]");
        }
    }

    ergo_script::test! {
        fn match_failure(t) {
            t.assert_fail("self:match {:a=[1,2]} ({b} -> :b) ([:a,:b] -> :b)");
        }
    }

    ergo_script::test! {
        fn forced_match(t) {
            t.assert_eq("self:match {:a=[1,2]} ({b} -> :b) ([:a,:b] -> :b); ()", "()");
            t.assert_script_fail("!self:match {:a=[1,2]} ({b} -> :b) ([:a,:b] -> :b)");
        }
    }
}
