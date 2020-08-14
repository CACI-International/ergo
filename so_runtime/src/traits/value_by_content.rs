//! The ValueByContent grease trait.

use crate::{types, ResultIterator};
use abi_stable::{
    std_types::{RArc, RResult, RVec},
    StableAbi,
};
use grease::{
    bst::BstMap,
    depends,
    path::PathBuf,
    runtime::Traits,
    traits::GreaseTrait,
    type_erase::Erased,
    types::GreaseType,
    value::{Error, Value},
};

/// A grease trait which identifies a value by its content.
#[derive(Clone, GreaseTrait, StableAbi)]
#[repr(C)]
pub struct ValueByContent {
    value_by_content: extern "C" fn(&Traits, Value, &RArc<Erased>) -> RResult<Value, Error>,
}

impl ValueByContent {
    /// Get the value identified by content.
    pub unsafe fn value_by_content_unsafe(
        &self,
        traits: &Traits,
        v: Value,
        data: &RArc<Erased>,
    ) -> Result<Value, Error> {
        (self.value_by_content)(traits, v, data).into()
    }

    /// Get the given value by content.
    /// The value _must_ have already been forced (and likewise for nested values).
    pub fn value_by_content(&self, traits: &Traits, v: Value) -> Result<Value, Error> {
        unsafe { self.value_by_content_unsafe(traits, v.clone(), &v.forced_value()) }
    }

    /// Implement ValueByContent for the given type.
    pub fn add_impl<T: GreaseType + std::hash::Hash + Sync>(traits: &mut Traits) {
        extern "C" fn value_by_content<T: std::hash::Hash + Sync>(
            _: &Traits,
            v: Value,
            data: &RArc<Erased>,
        ) -> RResult<Value, Error> {
            let deps = depends![unsafe { data.as_ref().as_ref::<T>() }];
            RResult::ROk(v.set_dependencies(deps))
        }

        traits.add_impl_for_type::<T, ValueByContent>(ValueByContent {
            value_by_content: value_by_content::<T>,
        });
    }
}

pub fn traits(traits: &mut Traits) {
    ValueByContent::add_impl::<types::Unit>(traits);
    ValueByContent::add_impl::<types::String>(traits);
    ValueByContent::add_impl::<bool>(traits);
    ValueByContent::add_impl::<PathBuf>(traits);
    ValueByContent::add_impl::<RVec<u8>>(traits);

    // types::Array
    {
        extern "C" fn value_by_content(
            traits: &Traits,
            _: Value,
            data: &RArc<Erased>,
        ) -> RResult<Value, Error> {
            let types::Array(vals) = unsafe { data.as_ref().as_ref::<types::Array>() };
            let vals: Result<RVec<_>, Error> = vals
                .iter()
                .map(|v| match traits.get::<ValueByContent>(v) {
                    Some(t) => Ok(t.value_by_content(traits, v.clone())?),
                    None => Err(format!(
                        "ValueByContent not implemented for {}",
                        super::type_name(traits, v.grease_type().as_ref())
                    )
                    .into()),
                })
                .collect_result();
            vals.map(|vals| types::Array(vals).into()).into()
        }

        traits
            .add_impl_for_type::<types::Array, ValueByContent>(ValueByContent { value_by_content });
    }

    // types::Map
    {
        extern "C" fn value_by_content(
            traits: &Traits,
            _: Value,
            data: &RArc<Erased>,
        ) -> RResult<Value, Error> {
            let types::Map(vals) = unsafe { data.as_ref().as_ref::<types::Map>() };
            let vals: Result<BstMap<_, _>, Error> = vals
                .iter()
                .map(|(k, v)| match traits.get::<ValueByContent>(v) {
                    Some(t) => Ok((k.clone(), t.value_by_content(traits, v.clone())?)),
                    None => Err(format!(
                        "ValueByContent not implemented for {}",
                        super::type_name(traits, &v.grease_type())
                    )
                    .into()),
                })
                .collect_result();
            vals.map(|vals| types::Map(vals).into()).into()
        }

        traits.add_impl_for_type::<types::Map, ValueByContent>(ValueByContent { value_by_content });
    }

    // types::Either
    {
        extern "C" fn value_by_content(
            traits: &Traits,
            _: Value,
            data: &RArc<Erased>,
        ) -> RResult<Value, Error> {
            let e = unsafe { data.as_ref().as_ref::<types::Either>() };
            let v = e.value();
            match traits.get::<ValueByContent>(&v) {
                Some(t) => t.value_by_content(traits, v),
                None => Err(format!(
                    "ValueByContent not implemented for {}",
                    super::type_name(traits, &v.grease_type())
                )
                .into()),
            }
            .into()
        }

        traits.add_impl_for_type::<types::Either, ValueByContent>(ValueByContent {
            value_by_content,
        });
    }
}
