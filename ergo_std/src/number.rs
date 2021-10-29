//! Number functions.

use ergo_runtime::types::number::Zero;
use ergo_runtime::{metadata::Source, traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "from" = from(),
        "compare" = compare(),
        "+" = add(),
        "-" = subtract(),
        "*" = multiply(),
        "/" = divide(),
        "%" = modulus()
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

#[types::ergo_fn]
/// Sum zero or more numbers.
///
/// Arguments: `(Into<Number> :n)...`
///
/// Returns the sum of all numbers.
async fn add(...) -> Value {
    let mut ret = types::Number::from_usize(0).num();
    while let Some(arg) = REST.next() {
        ret += traits::into::<types::Number>(arg).await?.as_ref().num();
    }
    types::Number::from(ret).into()
}

#[types::ergo_fn]
/// Negate one number or subtract one or more numbers from another.
///
/// Arguments: `(Into<Number> :a) (Into<Number> :b)...`
///
/// If one argument is given, returns the negation of the number. If more than one argument is
/// given, returns the result of subtracting all following numbers from the first number.
async fn subtract(base: _, ...) -> Value {
    let mut ret = traits::into::<types::Number>(base).await?.as_ref().num();

    let mut negate = true;
    while let Some(arg) = REST.next() {
        negate = false;
        ret -= traits::into::<types::Number>(arg).await?.as_ref().num();
    }
    if negate {
        ret *= types::Number::from_isize(-1).num();
    }
    types::Number::from(ret).into()
}

#[types::ergo_fn]
/// Multiply zero or more numbers.
///
/// Arguments: `(Into<Number> :n)...`
///
/// Returns the result of multiplying all numbers.
async fn multiply(...) -> Value {
    let mut ret = types::Number::from_usize(1).num();
    while let Some(arg) = REST.next() {
        ret *= traits::into::<types::Number>(arg).await?.as_ref().num();
    }
    types::Number::from(ret).into()
}

#[types::ergo_fn]
/// Invert one number or divide one or more numbers from another.
///
/// Arguments: `(Into<Number> :a) (Into<Number> :b)...`
///
/// If one argument is given, returns the multiplicative inverse of the number. If more than one
/// argument is given, returns the result of dividing all following numbers from the first number.
async fn divide(base: _, ...) -> Value {
    let base_source = Source::get(&base);
    let mut ret = traits::into::<types::Number>(base).await?.as_ref().num();

    let mut invert = true;
    while let Some(arg) = REST.next() {
        invert = false;
        let arg_source = Source::get(&arg);
        let v = traits::into::<types::Number>(arg).await?.as_ref().num();
        if v.is_zero() {
            Err(ergo_runtime::error!(
                labels: [ primary(arg_source.with("while dividing by this Number")) ],
                error: "division by zero"
            ))?;
        }
        ret /= v;
    }
    if invert {
        if ret.is_zero() {
            Err(ergo_runtime::error!(
                labels: [ primary(base_source.with("while inverting this Number")) ],
                error: "cannot invert zero"
            ))?;
        }
        ret = ret.recip();
    }
    types::Number::from(ret).into()
}

#[types::ergo_fn]
/// Get the modulus of a number.
///
/// Arguments: `(Into<Number> :a) (Into<Number> :b)`
///
/// Returns the result of a modulo b.
async fn modulus(a: _, b: _) -> Value {
    let mut ret = traits::into::<types::Number>(a).await?.as_ref().num();
    let b_source = Source::get(&b);
    let b = traits::into::<types::Number>(b).await?.as_ref().num();

    if b.is_zero() {
        Err(ergo_runtime::error!(
            labels: [ primary(b_source.with("while taking the modulus with this Number")) ],
            error: "modulus by zero"
        ))?;
    }

    ret %= b;
    types::Number::from(ret).into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_content_eq("self:type:Number: 1/4", "self:number:from 0.25");
            t.assert_ne("self:type:Number: 1/3", "self:number:from 0.333333333333333333333333");
            t.assert_fail("self:type:Number: 1/0");
        }

        fn compare(t) {
            t.assert_content_eq("self:number:compare (self:number:from -1) (self:number:from 200)", "self:cmp:less");
            t.assert_content_eq("self:number:compare (self:number:from 0.25) (self:number:from 0.125)", "self:cmp:greater");
            t.assert_content_eq("self:number:compare (self:number:from 100/4) (self:number:from 25)", "self:cmp:equal");
        }

        fn math(t) {
            t.assert_eq("self:number:+ 1 2 3", "self:number:from 6");
            t.assert_eq("self:number:- 4 3 2", "self:number:from -1");
            t.assert_eq("self:number:- 2", "self:number:from -2");
            t.assert_eq("self:number:* 4 3 2", "self:number:from 24");
            t.assert_eq("self:number:/ 4 3 2", "self:number:from 2/3");
            t.assert_eq("self:number:/ 2/3", "self:number:from 3/2");
            t.assert_fail("self:number:/ 4 2 0");
            t.assert_fail("self:number:/ 0");
            t.assert_eq("self:number:% 4 2", "self:number:from 0");
            t.assert_eq("self:number:% 15 4", "self:number:from 3");
            t.assert_eq("self:number:% 4 15", "self:number:from 4");
            t.assert_eq("self:number:% 1/2 1/3", "self:number:from 1/6");
            t.assert_fail("self:number:% 1 0");
        }
    }
}
