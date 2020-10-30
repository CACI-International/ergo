//! If function.

use ergo_runtime::{ergo_function, source::Source, ContextExt};
use grease::{
    depends,
    value::{IntoValue, Value},
};

pub fn function() -> Value {
    ergo_function!(independent std::if, |ctx| {
        let cond = ctx.args.next().ok_or("no condition specified")?;
        let if_true = ctx.args.next().ok_or("no truth value specified")?;
        let if_false = ctx.args.next();

        ctx.unused_arguments()?;

        let to_sourced = ctx.into_sourced::<bool>(cond);
        let cond = to_sourced.await?.unwrap();

        let if_true = if_true.unwrap();
        let if_false = if_false.map(Source::unwrap).unwrap_or(().into_value());

        let deps = depends![cond, if_true, if_false];
        // If types are immediately available and match, produce a value with the given type.
        match (if_true.grease_type_immediate(), if_false.grease_type_immediate()) {
            (Some(a), Some(b)) if a == b =>
                unsafe {
                    Value::new(abi_stable::std_types::RArc::new(a.clone()), async move {
                        let c = cond.await?;
                        if *c.as_ref() {
                            if_true.await
                        } else {
                            if_false.await
                        }
                    }, deps)
                }
            _ => Value::dyn_new(async move {
                    let c = cond.await?;
                    if *c.as_ref() {
                        if_true.make_any_value().await
                    } else {
                        if_false.make_any_value().await
                    }
                }, deps)
        }
    })
    .into()
}
