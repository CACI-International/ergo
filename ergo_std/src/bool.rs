//! Bool functions.

use ergo_runtime::{traits, types, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "true" = true_val(),
        "false" = false_val(),
        "from" = from(),
        "and" = and(),
        "or" = or(),
        "not" = not()
    }
}

fn true_val() -> Value {
    types::Bool(true).into()
}

fn false_val() -> Value {
    types::Bool(false).into()
}

#[types::ergo_fn]
/// Convert a value into a Bool.
///
/// Arguments: `:value`
async fn from(value: _) -> Value {
    traits::into::<types::Bool>(value).await?.into()
}

#[types::ergo_fn]
/// Return the boolean conjuction of all values.
///
/// Arguments: `(Into<Bool> :v)...`
///
/// Returns immediately as soon as a false value is encountered.
async fn and(...) -> Value {
    while let Some(arg) = REST.next() {
        if !traits::into::<types::Bool>(arg).await?.as_ref().0 {
            return Ok(types::Bool(false).into());
        }
    }
    types::Bool(true).into()
}

#[types::ergo_fn]
/// Return the boolean disjuction of all values.
///
/// Arguments: `(Into<Bool> :v)...`
///
/// Returns immediately as soon as a true value is encountered.
async fn or(...) -> Value {
    while let Some(arg) = REST.next() {
        if traits::into::<types::Bool>(arg).await?.as_ref().0 {
            return Ok(types::Bool(true).into());
        }
    }
    types::Bool(false).into()
}

#[types::ergo_fn]
/// Return the boolean complement of the given value.
///
/// Arguments: `(Into<Bool> :v)`
///
/// Returns the complement of `v`.
async fn not(v: _) -> Value {
    types::Bool(!traits::into::<types::Bool>(v).await?.as_ref().0).into()
}

#[cfg(test)]
mod test {
    ergo_script::tests! {
        fn from(t) {
            t.assert_eq("self:bool:false", "self:bool:from :unset");
            t.assert_eq("self:bool:true", "self:bool:from hello");
            t.assert_ne("self:bool:false", "self:bool:from ()"); // () is true, too
        }
    }
}
