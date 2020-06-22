//! Display and TypeName implementations for script types.

use crate::script::runtime::script_types::{ScriptArray, ScriptMap};
use grease::{display, impl_display, trait_generator, GreaseDisplay, Traits};
use std::fmt;

struct Empty<'a, 'b>(grease::Displayed<'a, 'b>);

impl<'a, 'b> fmt::Display for Empty<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self.0.to_string();
        if s.is_empty() {
            write!(f, "(empty)")
        } else {
            write!(f, "{}", s)
        }
    }
}

impl GreaseDisplay for ScriptArray {
    fn fmt(&self, traits: &Traits, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, v) in self.0.iter().enumerate() {
            write!(
                f,
                "{}{}",
                Empty(display(traits, v)),
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
                Empty(display(traits, v)),
                if i < self.0.len() - 1 { "\n\n" } else { "" }
            )?;
        }
        Ok(())
    }
}

trait_generator!(impl_display(ScriptArray, ScriptMap));
