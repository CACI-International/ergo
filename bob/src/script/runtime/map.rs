//! Mapping over arrays.

use super::{Data, DataFunction, FunctionContext, FunctionError};
use grease::{Context, Plan, SplitInto};

pub fn map_builtin() -> Data {
    Data::Function(DataFunction::BuiltinFunction(Box::new(map)).into())
}

fn map(ctx: &mut Context<FunctionContext>) -> Result<Data, FunctionError> {
    let mut args = Vec::new();
    std::mem::swap(&mut args, &mut ctx.inner.args);

    let mut args = args.into_iter();
    let func = args.next().ok_or("map function not provided".to_owned())?;
    let arr = args.next().ok_or("map array not provided".to_owned())?;
    if args.next().is_some() {
        return Err("extraneous argument to map".into());
    }

    let func = match func {
        Data::Function(f) => f,
        _ => return Err("first argument must be a function".into()),
    };
    let arr = match arr {
        Data::Array(arr) => arr,
        _ => return Err("second argument must be an array".into()),
    };

    let arr = arr
        .into_iter()
        .map(|d| {
            ctx.split_map(|ctx: &mut Context<super::Context>| func.clone().plan_join(ctx, vec![d]))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Data::Array(arr))
}
