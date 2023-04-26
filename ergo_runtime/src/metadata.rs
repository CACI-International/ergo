//! Common value metadata keys.

use crate as ergo_runtime;
use crate::abi_stable::{rvec, std_types::RVec, uuid::Uuid, StableAbi};
use crate::context::DynamicScopeKey;
use crate::type_system::ErgoType;
use crate::value::{match_value, MetadataKey, TypedValue, Value};

/// Documentation metadata key.
pub struct Doc;

impl MetadataKey for Doc {
    fn id(&self) -> u128 {
        crate::nsid!(meta::doc).as_u128()
    }
}

/// The dynamic scope key used to set the doc value when evaluating the doc string.
pub struct DocValueKey;

impl DynamicScopeKey for DocValueKey {
    type Value = Value;

    fn id(&self) -> u128 {
        nsid!(doc::value).as_u128()
    }
}

impl Doc {
    /// Get the documentation value for the given value.
    ///
    /// This retrieves the documentation.
    ///
    /// If no documentation is available, a string indicating the (possibly dynamic) type will be returned.
    pub async fn get(value: &Value) -> crate::Result<String> {
        let mut value = value.clone();
        while value.get_metadata(&Doc).is_none() && value.eval_once().await {}

        if let Some(v) = value.get_metadata(&Doc) {
            let mut v = v.clone();
            let doc_v = value.clone();
            crate::Context::fork(
                |ctx| {
                    ctx.dynamic_scope
                        .set(&Source::get(&value).with(DocValueKey), doc_v)
                },
                crate::Context::eval(&mut v),
            )
            .await?;

            match_value! {v,
                crate::types::String(s) => return Ok(s.into()),
                _ => ()
            }
        }
        match value.as_type::<crate::types::Error>() {
            Ok(e) => Err(e.into_owned()),
            Err(v) => Ok(format!(
                "value with type '{}'",
                crate::traits::type_name(&v)
            )),
        }
    }

    /// Set a documentation string for the given value.
    pub fn set_string<S: Into<crate::types::String>>(v: &mut Value, s: S) {
        Self::set(v, s.into().into());
    }

    /// Set documentation for the given value.
    pub fn set(v: &mut Value, doc: Value) {
        v.set_metadata(&Doc, doc);
    }
}

/// Source metadata key.
pub struct Source;

#[derive(Clone, Debug, ErgoType, StableAbi, PartialEq, Eq, Hash)]
#[repr(C)]
struct ValueSources {
    sources: RVec<crate::Source<()>>,
}

crate::ConstantDependency!(ValueSources);

impl From<ValueSources> for TypedValue<ValueSources> {
    fn from(vs: ValueSources) -> Self {
        Self::constant(vs)
    }
}

impl MetadataKey for Source {
    fn id(&self) -> u128 {
        crate::nsid!(meta::source).as_u128()
    }
}

impl Source {
    /// Get all source information for the given value.
    pub fn get_sources(value: &Value) -> Option<&RVec<crate::Source<()>>> {
        value
            .get_metadata(&Source)
            .map(|v| &v.as_ref::<ValueSources>().unwrap().sources)
    }

    /// Get the source of the given value, if any.
    pub fn get_option(value: &Value) -> Option<crate::Source<()>> {
        Self::get_sources(value).map(|v| v.last().unwrap().clone())
    }

    /// Get the origin source of the given value, if any.
    pub fn get_origin_option(value: &Value) -> Option<crate::Source<()>> {
        Self::get_sources(value).map(|v| v.first().unwrap().clone())
    }

    /// Get some source for the given value.
    ///
    /// Returns a missing source if no source is available.
    pub fn get(value: &Value) -> crate::Source<()> {
        match Self::get_option(value) {
            Some(v) => v,
            None => crate::Source::missing(()),
        }
    }

    /// Get the origin source for the given value.
    ///
    /// This is the oldest source available for the value.
    pub fn get_origin(value: &Value) -> crate::Source<()> {
        match Self::get_origin_option(value) {
            Some(v) => v,
            None => crate::Source::missing(()),
        }
    }

    /// Get the source immediately prior to the last source for the given value, if any.
    pub fn get_prior(value: &Value) -> crate::Source<()> {
        match Self::get_sources(value) {
            None => crate::Source::missing(()),
            Some(v) => v.iter().nth_back(1).or_else(|| v.last()).unwrap().clone(),
        }
    }

    /// Extract the source of the given value and wrap the value with it.
    pub fn extract<T>(value: T) -> crate::Source<T>
    where
        T: std::borrow::Borrow<Value>,
    {
        Self::get(value.borrow()).with(value)
    }

    /// Set the source for the given value.
    pub fn set(v: &mut Value, src: crate::Source<()>) {
        v.set_metadata(
            &Self,
            ValueSources {
                sources: rvec![src],
            }
            .into(),
        );
    }

    /// Add the latest source for the given value.
    pub fn update(v: &mut Value, src: crate::Source<()>) {
        let mut sources = match Self::get_sources(v) {
            None => rvec![],
            Some(v) => v.clone(),
        };
        sources.push(src);
        v.set_metadata(&Source, ValueSources { sources }.into());
    }

    /// Set the source for the given value if no other source is set.
    pub fn set_if_missing(v: &mut Value, src: crate::Source<()>) {
        if v.get_metadata(&Source).is_none() {
            Self::set(v, src)
        }
    }

    /// Imbue a source into a value.
    pub fn imbue(v: crate::Source<Value>) -> Value {
        let (src, mut v) = v.take();
        Self::set(&mut v, src);
        v
    }

    /// Copy all source information from one value to another.
    pub fn copy(from: &Value, mut to: Value) -> Value {
        if let Some(sources) = from.get_metadata(&Self) {
            to.set_metadata(&Self, sources.clone());
        }
        to
    }
}

/// Metadata key for any runtime value.
pub struct Runtime {
    /// The id to use as the metadata key.
    pub key: u128,
}

impl MetadataKey for Runtime {
    fn id(&self) -> u128 {
        let mut id = crate::nsid!(meta::any);
        id = Uuid::new_v5(&id, self.key.to_string().as_bytes());
        id.as_u128()
    }
}
