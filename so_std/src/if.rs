//! If function.

use super::builtin_function_prelude::*;
use super::Source;
use grease::{depends, make_value, GetValueType, IntoValue, TraitImpl, ValueType};

def_builtin!(ctx => {
    let cond = ctx.args.next().ok_or("no condition specified")?;
    let if_true = ctx.args.next().ok_or("no truth value specified")?;
    let if_false = ctx.args.next();

    if ctx.unused_arguments() {
        return Ok(Eval::Error);
    }

    let to_bool: Option<grease::IntoTyped<bool>> = ctx.traits.get(&cond);
    match to_bool {
        Some(t) => {
            let cond = t.into_typed(cond.unwrap());
            let vals = vec![if_false.map(Source::unwrap).unwrap_or(().into_value()), if_true.unwrap()];

            let val: grease::Value = make_value!([cond] Ok(ScriptEither(*cond.await? as usize))).into();
            Ok(Eval::Value(val.with_metadata("either", vals)))
        }
        None => {
            // Immediately evaluate to true type
            Ok(Eval::Value(if_true.unwrap()))
        }
    }
});

pub(crate) fn trait_generator(tp: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    if *tp == ScriptEither::value_type() {
        vec![TraitImpl::for_trait::<grease::IntoTyped<bool>>(|v| {
            let vals: Vec<Value> = todo!(); //v.metadata("either");
            let to_bools: Vec<_> = vals
                .into_iter()
                .map(|v| match todo!() /*traits.get::<grease::IntoTyped<bool>>(&v).await*/ {
                    Some(t) => t.into_typed(v),
                    None => true.into(),
                })
                .collect();
            let v = v.typed::<ScriptEither>().unwrap();
            let deps = depends![v];
            make_value!([v, ^to_bools] {
                let ind = v.await?.0;
                to_bools.swap_remove(ind).await
            })
        })]
    } else {
        vec![]
    }
}
