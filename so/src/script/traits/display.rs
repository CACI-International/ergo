//! Display and TypeName implementations for script types.

use crate::script::runtime::script_types::{ScriptArray, ScriptFunction, ScriptMap};
use grease::{
    display, grease_type_name, impl_display, impl_type_name, trait_generator, GreaseDisplay,
    TraitImpl, Traits, ValueType,
};
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

grease_type_name!(ScriptArray, "script array");
grease_type_name!(ScriptFunction, "script function");
grease_type_name!(ScriptMap, "script array");

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

trait_generator!(display_trait_generator impl_display(ScriptArray, ScriptMap));
trait_generator!(typename_trait_generator impl_type_name(ScriptArray,ScriptFunction,ScriptMap));

pub fn trait_generator(v: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    let mut impls = display_trait_generator(v.clone());
    impls.extend(typename_trait_generator(v.clone()));
    impls
}
