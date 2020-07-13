//! The ContentValue grease trait.

use crate::script::runtime::script_types::*;
use crate::script::runtime::Error;
use grease::{
    depends, match_value_type, trait_generator, Trait, TraitImpl, TraitRef, Traits, Value, ValueData,
    ValueType,
};
use std::sync::Arc;
use std::collections::BTreeMap;

#[derive(Clone, Trait)]
pub struct ContentValueTrait {
    content_value: fn(&Traits, &Arc<ValueType>, &Arc<ValueData>) -> Result<Value, Error>,
}

impl ContentValueTrait {
    pub fn content_value(&self, traits: &Traits, tp: &Arc<ValueType>, data: &Arc<ValueData>) -> Result<Value, Error> {
        (self.content_value)(traits, tp, data)
    }
}

TraitRef! {
    /// The content value grease trait reference.
    pub struct ContentValue(ContentValueTrait);
}

impl ContentValue {
    pub fn content_value(&self, v: Value) -> Result<Value, Error> {
        let data = v.get()?;
        self.storage.content_value(&self.traits, &self.value_type, &data)
    }
}

/// Implement ContentDependencyTrait for the given type.
pub fn impl_content_dependency<T: std::hash::Hash + Sync>() -> TraitImpl {
    TraitImpl::for_trait::<ContentValue>(ContentValueTrait {
        content_value: |_,tp,data| {
            let deps = depends![unsafe { data.as_ref().as_ref::<T>() }];
            // TODO default metadata?
            Ok(Value::from_raw(tp.clone(), data.clone(), Default::default(), 0).set_dependencies(deps))
        }
    })
}

trait_generator!(hashed_trait_generator
    impl_content_dependency(ScriptUnit, ScriptString, std::path::PathBuf, Vec<u8>, bool)
);

pub fn trait_generator(v: std::sync::Arc<ValueType>) -> Vec<TraitImpl> {
    let mut ret = Vec::new();
    ret.extend(hashed_trait_generator(v.clone()));
    match_value_type!(*v => {
        ScriptArray => ret.push(TraitImpl::for_trait::<ContentValue>(ContentValueTrait {
            content_value: |traits,_,data| {
                let ScriptArray(vals) = unsafe { data.as_ref().as_ref::<ScriptArray>() };
                let vals: Result<Vec<_>, Error> = vals.iter().map(|v| match traits.get::<ContentValue>(v) {
                    Some(t) => Ok(t.content_value(v.clone())?),
                    None => Err(Error::GenericError(format!("ContentValue not implemented for {}", grease::type_name(traits,&v.value_type()))))
                }).collect();
                Ok(ScriptArray(vals?).into())
            }
        })),
        ScriptMap => ret.push(TraitImpl::for_trait::<ContentValue>(ContentValueTrait {
            content_value: |traits,_,data| {
                let ScriptMap(vals) = unsafe { data.as_ref().as_ref::<ScriptMap>() };
                let vals: Result<BTreeMap<_,_>, Error> = vals.iter().map(|(k,v)| match traits.get::<ContentValue>(v) {
                    Some(t) => Ok((k.clone(),t.content_value(v.clone())?)),
                    None => Err(Error::GenericError(format!("ContentValue not implemented for {}", grease::type_name(traits,&v.value_type()))))
                }).collect();
                Ok(ScriptMap(vals?).into())
            }
        }))
        => ()
    });
    ret
}
