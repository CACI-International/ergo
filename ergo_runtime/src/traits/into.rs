//! The IntoTyped grease trait.

use super::type_name;
use crate::source::Source;
use crate::ContextExt;
use abi_stable::{std_types::ROption, StableAbi};
use grease::closure::*;
use grease::path::PathBuf;
use grease::runtime::{Context, Traits};
use grease::traits::*;
use grease::type_erase::Erased;
use grease::types::{GreaseType, Type, TypeParameters};
use grease::value::{TypedValue, Value};
use grease::*;

/// A grease trait for converting to different types.
#[grease_trait]
pub trait IntoTyped<T: StableAbi + 'static> {
    async fn into_typed(self) -> Value;
}

impl<T: GreaseType + StableAbi + 'static> IntoTyped<T> {
    /// Add an implementation of the IntoTyped grease trait.
    pub fn add_impl<U>(traits: &mut Traits)
    where
        U: GreaseType + Send + Sync + Clone + 'static,
        T: From<U> + GreaseType + Send + Sync + 'static,
    {
        traits.add_impl_for_type::<U, IntoTyped<T>>(grease_trait_impl! {
            impl<T, U> IntoTyped<T> for U
                where T: From<U> + GreaseType + Send + Sync + 'static,
                      U: GreaseType + Send + Sync + Clone + 'static
            {
                async fn into_typed(self) -> Value {
                    let v = self;
                    make_value!([v] {
                        Ok(T::from(v.await?.owned()))
                    }).into()
                }
            }
        });
    }
}

/// Convert the given value into another type.
pub async fn into_sourced<T: GreaseType + StableAbi + Send + Sync + 'static>(
    ctx: &Context,
    v: Source<Value>,
) -> grease::Result<Source<TypedValue<T>>> {
    v.map_async(|v| into::<T>(ctx, v))
        .await
        .transpose()
        .map_err(|e| e.into_grease_error())
}

/// Convert the given value into another type.
pub async fn into<T: GreaseType + StableAbi + Send + Sync + 'static>(
    ctx: &Context,
    v: Value,
) -> grease::Result<TypedValue<T>> {
    let t_ctx = ctx.clone();
    let mut t = ctx
        .get_trait::<IntoTyped<T>, _, _>(&v, move |t| {
            let t = t.clone();
            let ctx = t_ctx.clone();
            async move {
                let from_t = type_name(&ctx, &t).await?;
                let to_t = type_name(&ctx, &T::grease_type()).await?;
                Err(format!("cannot convert {} into {}", from_t, to_t).into())
            }
        })
        .await?;
    let ctx = ctx.clone();
    let deps = depends![v];
    let val = Value::dyn_new(
        async move {
            let v = t.into_typed(v).await?;
            Ok(v.into_any_value())
        },
        deps,
    );

    val.typed::<T, _, _>(move |t| {
        let ctx = ctx.clone();
        let t = t.clone();
        async move {
            let into_t = match type_name(&ctx, &T::grease_type()).await {
                Ok(v) => v,
                Err(e) => return e,
            };
            let from_t = match type_name(&ctx, &t).await {
                Ok(v) => v,
                Err(e) => return e,
            };
            format!("bad IntoTyped<{}> implementation, got {}", into_t, from_t).into()
        }
    })
    .await
}

grease_traits_fn! {
    // Anything -> bool (true)
    {
        extern "C" fn to_bool<'a>(_data: &'a grease::type_erase::Erased, _ctx: &'a grease::runtime::Context, v: Value) ->
            grease::future::BoxFuture<'a, grease::error::RResult<Value>> {
            grease::future::BoxFuture::new(async move {
                grease::error::RResult::ROk(v.then(TypedValue::constant(true).into()))
            })
        }
        traits.add_generator_by_trait_for_trait::<IntoTyped<bool>>(|_traits, _type| {
            ROption::RSome(IntoTypedImpl::<bool> {
                into_typed: unsafe { FnPtr::new(to_bool) },
                grease_trait_data: Default::default(),
                _phantom0: Default::default(),
            })
        });
    }

    // Identity: T -> T
    {
        extern "C" fn id_f<'a>(_data: &'a grease::type_erase::Erased, _ctx: &'a grease::runtime::Context, v: Value) ->
            grease::future::BoxFuture<'a, grease::error::RResult<Value>> {
            grease::future::BoxFuture::new(async move {
                grease::error::RResult::ROk(v)
            })
        }
        extern "C" fn id(_traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == IntoTyped::<()>::grease_trait().id {
                let TypeParameters(trait_types) = trt.data.clone().into();
                if tp == &trait_types[0] {
                    return ROption::RSome(Erased::new(IntoTypedImpl::<()> {
                        into_typed: unsafe { FnPtr::new(id_f) },
                        grease_trait_data: Default::default(),
                        _phantom0: Default::default(),
                    }));
                }
            }
            ROption::RNone
        }
        unsafe { traits.add_generator(id) };
    }

    // Unset -> bool (false)
    impl IntoTyped<bool> for crate::types::Unset {
        async fn into_typed(self) -> Value {
            TypedValue::constant(false).into()
        }
    }

    // Path -> String
    impl IntoTyped<crate::types::String> for PathBuf {
        async fn into_typed(self) -> Value {
            self.map(|v| crate::types::String::from(v.as_ref().as_ref().to_string_lossy().into_owned())).into()
        }
    }

    // Array -> Iter
    impl IntoTyped<crate::types::Iter> for crate::types::Array {
        async fn into_typed(self) -> Value {
            self.map(|v| crate::types::Iter::from_iter(v.owned().0.into_iter())).into()
        }
    }

    // Iter -> Array
    impl IntoTyped<crate::types::Array> for crate::types::Iter {
        async fn into_typed(self) -> Value {
            self.map(|v| crate::types::Array(v.owned().collect())).into()
        }
    }

    // Map -> Iter
    impl IntoTyped<crate::types::Iter> for crate::types::Map {
        async fn into_typed(self) -> Value {
            self.map(|v| {
                let key_k: Value = crate::types::String::from("key").into();
                let value_k: Value = crate::types::String::from("value").into();
                crate::types::Iter::from_iter(v.owned().0.into_iter().map(move |(k,v)| {
                    let mut map = bst::BstMap::new();
                    map.insert(key_k.clone(), k);
                    map.insert(value_k.clone(), v);
                    crate::types::Map(map).into_value_no_doc()
                }))
            }).into()
        }
    }

    // Iter -> Map
    impl IntoTyped<crate::types::Map> for crate::types::Iter {
        async fn into_typed(self) -> Value {
            let v = self;
            let ctx = CONTEXT.clone();
            make_value!([v] {
                let iter = v.await?.owned();
                let mut ret = bst::BstMap::default();
                let key_k: Value = crate::types::String::from("key").into();
                let value_k: Value = crate::types::String::from("value").into();
                for i in iter {
                    /// XXX do this in a better way (with an actual source)
                    let crate::types::Map(mut m) = ctx.source_value_as::<crate::types::Map>(Source::builtin(i))
                        .await?.unwrap().await?.owned();
                    let key = m.remove(&key_k);
                    let value = m.remove(&value_k);
                    if !m.is_empty() {
                        return Err("invalid map iterator, extra keys in items".into());
                    } else {
                        match (key,value) {
                            (None, None) => return Err("missing key and value in iterator".into()),
                            (None, _) => return Err("missing key in iterator".into()),
                            (_, None) => return Err("missing value in iterator".into()),
                            (Some(key), Some(value)) => {
                                ret.insert(key, value);
                            }
                        }
                    }
                }
                Ok(crate::types::Map(ret))
            }).into()
        }
    }
}
