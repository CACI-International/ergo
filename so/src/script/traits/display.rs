//! Display and TypeName implementations for script types.

use crate::script::runtime::script_types::{ScriptArray, ScriptMap};
use grease::{display, impl_display, trait_generator, GreaseDisplay, Traits};
use std::fmt;

impl GreaseDisplay for ScriptArray {
    fn fmt(&self, traits: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        for v in &self.0 {
            writeln!(f, "{}", display(traits, v))?;
        }
        Ok(())
    }
}

impl GreaseDisplay for ScriptMap {
    fn fmt(&self, traits: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        for (k, v) in &self.0 {
            writeln!(f, "{}:\n{}", k, display(traits, v))?;
        }
        Ok(())
    }
}

trait_generator!(impl_display(ScriptArray, ScriptMap));
