//! Execute commands in order.

use super::{Data, DataFunction, FunctionContext, FunctionError};
use grease::future::{FutureExt, TryFutureExt};
use grease::{make_value, Context};

pub fn do_builtin() -> Data {
    Data::Function(DataFunction::BuiltinFunction(Box::new(do_f)).into())
}

fn do_f(ctx: &mut Context<FunctionContext>) -> Result<Data, FunctionError> {
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut args = args.into_iter();
    let arr = args
        .next()
        .ok_or(FunctionError::from("no commands provided"))?;
    if args.next().is_some() {
        return Err("extraneous arguments to do".into());
    }

    let arr = match arr {
        Data::Array(arr) => arr,
        _ => return Err("argument to do must be an array".into()),
    };

    let mut fut = grease::future::ok(()).boxed();
    for a in arr {
        fut = fut.and_then(move |()| a).boxed();
    }

    Ok(Data::Value(make_value!(fut.await).into()))
}
