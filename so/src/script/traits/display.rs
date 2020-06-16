//! Display and TypeName implementations for script types.

use crate::script::runtime::script_types::{ScriptArray, ScriptMap};
use grease::{display, impl_display, trait_generator, GreaseDisplay, Traits};
use std::fmt;

impl GreaseDisplay for ScriptArray {
    fn fmt(&self, traits: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, v) in self.0.iter().enumerate() {
            write!(
                f,
                "{}{}",
                display(traits, v),
                if i < self.0.len() - 1 { "\n" } else { "" }
            )?;
        }
        Ok(())
    }
}

impl GreaseDisplay for ScriptMap {
    fn fmt(&self, traits: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, (k, v)) in self.0.iter().enumerate() {
            write!(
                f,
                "{}:\n{}{}",
                k,
                display(traits, v),
                if i < self.0.len() - 1 { "\n" } else { "" }
            )?;
        }
        Ok(())
    }
}

trait_generator!(impl_display(ScriptArray, ScriptMap));
