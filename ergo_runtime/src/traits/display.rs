//! The Display ergo trait and helper utilities.

use super::type_name;
use crate as ergo_runtime;
use crate::type_system::ergo_trait;
use crate::{Context, Value};
use abi_stable::{std_types::RBox, DynTrait, StableAbi};
use std::fmt::Write;

#[derive(StableAbi)]
#[sabi(impl_InterfaceType(Send, FmtWrite))]
#[repr(C)]
struct FormatterInterface;

/// A display Formatter.
#[derive(StableAbi)]
#[repr(C)]
pub struct Formatter<'a>(DynTrait<'a, RBox<()>, FormatterInterface>);

impl<'a> Formatter<'a> {
    pub fn new<W: Write + Send>(w: &'a mut W) -> Self {
        Formatter(DynTrait::from_borrowing_value(w))
    }

    // Duplicate std::fmt::Write functions, so that `write!` macros work without a `use
    // std::fmt::Write`.
    pub fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
        Write::write_str(self, s)
    }

    pub fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result<(), std::fmt::Error> {
        Write::write_fmt(self, args)
    }
}

impl<'a> Write for Formatter<'a> {
    fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
        self.0.write_str(s)
    }
}

/// The Display ergo trait.
#[ergo_trait]
pub trait Display {
    async fn fmt(&self, f: &mut Formatter<'_>) -> crate::RResult<()>;
}

/// Display the given value to the Formatter.
pub async fn display(mut v: Value, f: &mut Formatter<'_>) -> crate::Result<()> {
    Context::eval(&mut v).await?;
    match Context::get_trait::<Display>(&v) {
        None => {
            crate::error_info!(
                labels: [primary(crate::metadata::Source::get(&v).with("while displaying this value"))],
                {write!(f, "<cannot display values of type {}>", type_name(&v))}
            )?;
            Ok(())
        }
        Some(t) => t.fmt(v, f).await.into_result(),
    }
}

/// Display the given value to the Formatter, regardless of whether the value is an Error or not.
pub async fn display_any(mut v: Value, f: &mut Formatter<'_>) -> crate::Result<()> {
    drop(Context::eval(&mut v).await);
    match Context::get_trait::<Display>(&v) {
        None => {
            crate::error_info!(
                labels: [primary(crate::metadata::Source::get(&v).with("while displaying this value"))],
                {write!(f, "<cannot display values of type {}>", type_name(&v))}
            )?;
            Ok(())
        }
        Some(t) => t.fmt(v, f).await.into_result(),
    }
}

/// Display a value to a string.
pub async fn to_string(v: Value) -> crate::Result<String> {
    let mut s = String::new();
    display(v, &mut Formatter::new(&mut s)).await?;
    Ok(s)
}

#[macro_export]
macro_rules! ergo_display_basic {
    ( $traits:expr, $t:ty ) => {
        $traits.add_impl_for_type::<$t, $crate::traits::Display>(
            $crate::type_system::ergo_trait_impl! {
                impl $crate::traits::Display for $t {
                    async fn fmt(&self, f: &mut $crate::traits::Formatter) -> $crate::RResult<()> {
                        $crate::error_info!({
                            write!(f, "{}", self)
                        }).into()
                    }
                }
            },
        );
    };
}
