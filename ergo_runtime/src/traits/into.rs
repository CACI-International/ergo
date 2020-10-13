//! The IntoTyped grease trait.

use super::type_name;
use crate::source::Source;
use abi_stable::{
    std_types::{RArc, ROption, RString},
    StableAbi,
};
use grease::closure::*;
use grease::path::PathBuf;
use grease::runtime::{Context, Traits};
use grease::traits::*;
use grease::type_erase::Erased;
use grease::types::{GreaseType, Type, TypeParameters};
use grease::value::{IntoValue, TypedValue, Value};

#[derive(GreaseTrait, StableAbi)]
#[repr(C)]
pub struct IntoTyped<T: StableAbi + 'static> {
    into: Closure<Args2<Context, Value>, Value>,
    _phantom: std::marker::PhantomData<extern "C" fn() -> *const T>,
}

impl<T: GreaseType + StableAbi + 'static> IntoTyped<T> {
    pub fn into_typed(&self, c: &Context, v: Value) -> TypedValue<T> {
        self.into.call(c.clone(), v).typed::<T>().unwrap()
    }

    /// Add an implementation of the IntoTyped grease trait.
    pub fn add_impl<U>(traits: &mut Traits)
    where
        U: GreaseType + Send + Sync + Clone + 'static,
        T: From<U> + GreaseType + Send + Sync + 'static,
    {
        traits.add_impl_for_type::<U, IntoTyped<T>>(Self::new(|_, v| {
            v.typed::<U>().unwrap().map(|v| T::from(v.owned()))
        }))
    }

    /// Create a new IntoTyped implementation using the given function.
    pub fn new<F: Fn(&Context, Value) -> TypedValue<T> + Send + Sync + 'static>(f: F) -> Self {
        IntoTyped {
            into: Closure::from(move |c: Context, v: Value| f(&c, v).into()),
            _phantom: Default::default(),
        }
    }
}

/// Convert the given value into another type.
pub fn into_sourced<T: GreaseType + StableAbi + 'static>(
    ctx: &Context,
    v: Source<Value>,
) -> Result<Source<TypedValue<T>>, grease::value::Error> {
    v.map(|v| into::<T>(ctx, v))
        .transpose()
        .map_err(|e| e.into_grease_error())
}

/// Convert the given value into another type.
pub fn into<T: GreaseType + StableAbi + 'static>(
    ctx: &Context,
    v: Value,
) -> Result<TypedValue<T>, String> {
    match ctx.traits.get::<IntoTyped<T>>(&v) {
        None => Err(format!(
            "cannot convert {} into {}",
            type_name(&ctx.traits, &v.grease_type()),
            type_name(&ctx.traits, &T::grease_type())
        )),
        Some(t) => Ok(t.into_typed(ctx, v)),
    }
}

pub fn traits(traits: &mut Traits) {
    // Identity: T -> T
    {
        extern "C" fn id(_traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == IntoTyped::<()>::grease_trait().id {
                let TypeParameters(trait_types) = trt.data.clone().into();
                if tp == &trait_types[0] {
                    return ROption::RSome(Erased::new(IntoTyped::<()> {
                        into: (|_: Context, v: Value| v).into(),
                        _phantom: Default::default(),
                    }));
                }
            }
            ROption::RNone
        }
        unsafe { traits.add_generator(id) };
    }

    // Anything -> bool (true)
    {
        traits.add_generator_by_trait_for_trait::<IntoTyped<bool>>(|_traits, _type| {
            ROption::RSome(IntoTyped {
                into: (|_, v: Value| v.then(true.into_value())).into(),
                _phantom: Default::default(),
            })
        });
    }

    // () -> bool (false)
    {
        traits.add_impl_for_type::<(), IntoTyped<bool>>(IntoTyped {
            into: (|_, v: Value| v.then(false.into_value())).into(),
            _phantom: Default::default(),
        });
    }

    // TODO ExitStatus -> bool (ExitStatus::success())

    // Path -> String
    {
        traits.add_impl_for_type::<PathBuf, IntoTyped<RString>>(IntoTyped::new(|_, v: Value| {
            v.typed::<PathBuf>()
                .unwrap()
                .map(|v| RString::from(v.as_ref().as_ref().to_string_lossy().into_owned()))
        }));
    }

    // types::Either
    {
        use crate::types::Either;

        extern "C" fn gen(traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            // We only want cases with any either type and any IntoTyped trait.
            if !Either::matches_grease_type(tp) || trt.id != IntoTyped::<()>::grease_trait().id {
                return ROption::RNone;
            }

            let params = TypeParameters::from(tp.data.clone());
            let impls: Option<Vec<_>> = params.0.iter().map(|t| traits.get_impl(t, trt)).collect();
            let to_type = TypeParameters::from(trt.data.clone())
                .0
                .get(0)
                .unwrap()
                .clone();

            impls
                .map(move |impls| {
                    let impls = std::sync::Arc::new(impls);
                    Erased::new(IntoTyped::<()> {
                        into: Closure::from(move |c, v: Value| {
                            let v = v.typed::<Either>().unwrap();
                            let deps = grease::depends![v];
                            let impls = impls.clone();
                            Value::new(
                                RArc::new(to_type.clone()),
                                async move {
                                    let e = v.await?;
                                    let t = unsafe {
                                        impls
                                            .get(e.index())
                                            .unwrap()
                                            .as_ref()
                                            .as_ref::<IntoTyped<()>>()
                                    };
                                    t.into.call(c, e.value()).await
                                },
                                deps,
                            )
                        }),
                        _phantom: Default::default(),
                    })
                })
                .into()
        }

        unsafe {
            traits.add_generator(gen);
        }
    }
}
