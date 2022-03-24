//! Number functions.

use ergo_runtime::types::number::Zero;
use ergo_runtime::{metadata::Source, traits, type_system::ErgoType, types, Value};

pub fn r#type() -> Value {
    types::Type {
        tp: types::Number::ergo_type(),
        index: crate::make_string_map! {
            "new" = new(),
            "from" = from(),
            "compare" = compare(),
            "+" = add(),
            "-" = subtract(),
            "*" = multiply(),
            "/" = divide(),
            "%" = modulus()
        },
    }
    .into()
}

#[types::ergo_fn]
/// Create a new Number.
///
/// Arguments: `(String :rep)`
///
/// Numbers are arbitrarily large rationals. `rep` may be expressed as an integer, decimal, or
/// a rational such as `1/2`.
async fn new(rep: types::String) -> Value {
    rep.as_ref()
        .0
        .as_str()
        .parse::<types::Number>()
        .map_err(|e| {
            ergo_runtime::error! {
                labels: [
                    primary(Source::get(&rep).with("while parsing this value as a number"))
                ],
                notes: [
                    format_args!("value was '{}'", rep.as_ref().0.as_str())
                ],
                error: e
            }
        })
        .into()
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
    super::order::Order::from(a.as_ref().cmp(b.as_ref())).into()
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
        fn new(t) {
            t.assert_success("self:Number:new 1");
            t.assert_success("self:Number:new -3.14");
            t.assert_success("self:Number:new 1/2");
            t.assert_success("self:Number:new -3/4");
            t.assert_success("self:Number:new 0.5");
            t.assert_success("self:Number:new -0.3");
            t.assert_eq("self:Number:new 0.5", "self:Number:new 1/2");
            t.assert_fail("self:Number:new ()");
            t.assert_success("self:Number _ = self:Number:new 0");
        }

        fn from(t) {
            t.assert_eq("self:Number:from 1/4", "self:Number:from 0.25");
            t.assert_ne("self:Number:from 1/3", "self:Number:from 0.333333333333333333333333");
            t.assert_fail("self:Number:from 1/0");
        }

        fn compare(t) {
            t.assert_eq("self:Number:compare (self:Number:from -1) (self:Number:from 200)", "self:Order:less");
            t.assert_eq("self:Number:compare (self:Number:from 0.25) (self:Number:from 0.125)", "self:Order:greater");
            t.assert_eq("self:Number:compare (self:Number:from 100/4) (self:Number:from 25)", "self:Order:equal");
        }

        fn math(t) {
            t.assert_eq("self:Number:+ 1 2 3", "self:Number:from 6");
            t.assert_eq("self:Number:- 4 3 2", "self:Number:from -1");
            t.assert_eq("self:Number:- 2", "self:Number:from -2");
            t.assert_eq("self:Number:* 4 3 2", "self:Number:from 24");
            t.assert_eq("self:Number:/ 4 3 2", "self:Number:from 2/3");
            t.assert_eq("self:Number:/ 2/3", "self:Number:from 3/2");
            t.assert_fail("self:Number:/ 4 2 0");
            t.assert_fail("self:Number:/ 0");
            t.assert_eq("self:Number:% 4 2", "self:Number:from 0");
            t.assert_eq("self:Number:% 15 4", "self:Number:from 3");
            t.assert_eq("self:Number:% 4 15", "self:Number:from 4");
            t.assert_eq("self:Number:% 1/2 1/3", "self:Number:from 1/6");
            t.assert_fail("self:Number:% 1 0");
        }
    }
}
