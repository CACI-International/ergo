//! Number functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "from" = from(),
        "compare" = compare()
    }
}

#[types::ergo_fn]
/// Convert a value into a Number.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    traits::into::<types::Number>(value).await?.into()
}

#[types::ergo_fn]
/// Compare two numbers.
///
/// Arguments: `(Number :a) (Number :b)`
///
/// Returns a `std:Order` indicating the comparison of `a` to `b`.
async fn compare(a: types::Number, b: types::Number) -> Value {
    super::cmp::Order::from(a.as_ref().cmp(b.as_ref())).into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_content_eq("self:type:Number: 1/4", "self:number:from 0.25");
            t.assert_ne("self:type:Number: 1/3", "self:number:from 0.333333333333333333333333");
        }

        fn compare(t) {
            t.assert_content_eq("self:number:compare (self:number:from -1) (self:number:from 200)", "self:cmp:less");
            t.assert_content_eq("self:number:compare (self:number:from 0.25) (self:number:from 0.125)", "self:cmp:greater");
            t.assert_content_eq("self:number:compare (self:number:from 100/4) (self:number:from 25)", "self:cmp:equal");
        }
    }
}
