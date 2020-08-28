//! The IntoTyped grease trait.

use abi_stable::{
    std_types::{RArc, ROption, RString},
    StableAbi,
};
use grease::closure::*;
use grease::path::PathBuf;
use grease::runtime::Traits;
use grease::traits::*;
use grease::type_erase::Erased;
use grease::types::{GreaseType, Type};
use grease::value::{IntoValue, TypedValue, Value};

#[derive(GreaseTrait, StableAbi)]
#[repr(C)]
pub struct IntoTyped<T: StableAbi + 'static> {
    into: Closure<Args1<Value>, Value>,
    _phantom: std::marker::PhantomData<extern "C" fn() -> *const T>,
}

impl<T: GreaseType + StableAbi + 'static> IntoTyped<T> {
    pub fn into_typed(&self, v: Value) -> TypedValue<T> {
        self.into.call(v).typed::<T>().unwrap()
    }

    /// Add an implementation of the IntoTyped grease trait.
    pub fn add_impl<U>(traits: &mut Traits)
    where
        U: GreaseType + Send + Sync + Clone + 'static,
        T: From<U> + GreaseType + Send + Sync + 'static,
    {
        traits.add_impl_for_type::<U, IntoTyped<T>>(IntoTyped {
            into: Closure::from(|v: Value| {
                v.typed::<U>().unwrap().map(|v| T::from(v.owned())).into()
            }),
            _phantom: Default::default(),
        })
    }
}

pub fn traits(traits: &mut Traits) {
    // Identity: T -> T
    {
        extern "C" fn id(_traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            if trt.id == grease_trait_uuid(b"so_types::traits::IntoTyped") {
                let trait_type: Type = trt.data.clone().into();
                if tp == &trait_type {
                    return ROption::RSome(Erased::new(IntoTyped::<()> {
                        into: (|v: Value| v).into(),
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
                into: (|v: Value| v.then(true.into_value())).into(),
                _phantom: Default::default(),
            })
        });
    }

    // () -> bool (false)
    {
        traits.add_impl_for_type::<(), IntoTyped<bool>>(IntoTyped {
            into: (|v: Value| v.then(false.into_value())).into(),
            _phantom: Default::default(),
        });
    }

    // TODO ExitStatus -> bool (ExitStatus::success())

    // Path -> String
    {
        traits.add_impl_for_type::<PathBuf, IntoTyped<RString>>(IntoTyped {
            into: (|v: Value| {
                v.typed::<PathBuf>()
                    .unwrap()
                    .map(|v| RString::from(v.as_ref().as_ref().to_string_lossy().into_owned()))
                    .into()
            })
            .into(),
            _phantom: Default::default(),
        });
    }

    // types::Either
    {
        use crate::types::Either;
        use grease::types::TypeParameters;

        extern "C" fn gen(traits: &Traits, tp: &Type, trt: &Trait) -> ROption<Erased> {
            if *tp != Either::grease_type() || trt.id != IntoTyped::<()>::grease_trait().id {
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
                        into: Closure::from(move |v: Value| {
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
                                    t.into.call(e.value()).await
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
