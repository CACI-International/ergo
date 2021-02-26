//! The Display grease trait and helper utilities.
//!
//! TODO: Change this trait to use something that supports Write rather than putting everything
//! into a String. This is a bit difficult with regard to how the application works; it tries to
//! display the final value, but

use super::type_name;
use crate::types;
use abi_stable::{std_types::RBox, DynTrait, StableAbi};
use grease::path::PathBuf;
use grease::runtime::Context;
use grease::value::Value;
use grease::{grease_trait, grease_traits_fn};
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
        Formatter(DynTrait::from_borrowing_value(w, FormatterInterface))
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

/// The Display grease trait.
#[grease_trait]
pub trait Display {
    async fn fmt(&self, f: &mut Formatter<'_>);
}

/// Display the given value to the Write instance.
pub async fn display(ctx: &Context, v: Value, f: &mut Formatter<'_>) -> grease::Result<()> {
    let trt = ctx
        .get_trait::<Display, _, _>(&v, |_| async {
            Ok(grease::grease_trait_impl! {
                impl Display for _ {
                    async fn fmt(&self, f: &mut Formatter) {
                        let name = type_name(CONTEXT, SELF_TYPE).await?;
                        write!(f, "<cannot display values of type {}>", name)?;
                    }
                }
            })
        })
        .await;
    match trt {
        Err(e) => Err(e),
        Ok(mut t) => Ok(t.fmt(v, f).await?.into()),
    }
}

#[macro_export]
macro_rules! grease_display_basic {
    ( $traits:expr, $t:ty ) => {
        $traits.add_impl_for_type::<$t, $crate::traits::Display>(grease::grease_trait_impl! {
            impl $crate::traits::Display for $t {
                async fn fmt(&self, f: &mut $crate::traits::Formatter) {
                    write!(f, "{}", self)?
                }
            }
        });
    };
}

grease_traits_fn! {
    impl Display for types::Unit {
        async fn fmt(&self, _f: &mut Formatter) {
        }
    }

    impl Display for PathBuf {
        async fn fmt(&self, f: &mut Formatter) {
            write!(f, "{}", self.as_ref().display())?
        }
    }

    impl Display for types::Array {
        async fn fmt(&self, f: &mut Formatter) {
            let mut iter = self.0.iter();
            if let Some(v) = iter.next() {
                display(CONTEXT, v.clone(), f).await?;
            }
            for v in iter {
                write!(f, "\n")?;
                display(CONTEXT, v.clone(), f).await?;
            }
        }
    }

    impl Display for types::Map {
        async fn fmt(&self, f: &mut Formatter) {
            let mut iter = self.0.iter();
            if let Some((k,v)) = iter.next() {
                display(CONTEXT, k.clone(), f).await?;
                write!(f, " -> ")?;
                display(CONTEXT, v.clone(), f).await?;
            }

            for (k,v) in iter {
                write!(f, "\n")?;
                display(CONTEXT, k.clone(), f).await?;
                write!(f, " -> ")?;
                display(CONTEXT, v.clone(), f).await?;
            }
        }
    }

    impl Display for types::MapEntry {
        async fn fmt(&self, f: &mut Formatter) {
            display(CONTEXT, self.key.clone(), f).await?;
            write!(f, " -> ")?;
            display(CONTEXT, self.value.clone(), f).await?;
        }
    }

    grease_display_basic!(traits, types::Bool);
    grease_display_basic!(traits, types::String);
}
