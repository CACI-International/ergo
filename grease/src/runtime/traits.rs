//! Runtime trait tracking.

use crate::traits as trt;
use crate::type_erase::Erased;
use crate::types::*;
use crate::value::Value;
use abi_stable::{
    external_types::RRwLock,
    std_types::{RArc, RHashMap, ROption, RVec},
    StableAbi,
};
use futures::future::FutureExt;

/// A trait generator which has a fixed type.
pub type TraitGeneratorByType = extern "C" fn(&Traits, &trt::Trait) -> ROption<Erased>;
/// A trait generator which has a fixed trait.
pub type TraitGeneratorByTrait = extern "C" fn(&Traits, &Type) -> ROption<Erased>;
/// A trait generator.
pub type TraitGenerator = extern "C" fn(&Traits, &Type, &trt::Trait) -> ROption<Erased>;

#[derive(StableAbi)]
#[repr(C)]
struct InternalTraitGeneratorByType(
    extern "C" fn(*const Traits, *const trt::Trait) -> ROption<Erased>,
);

impl InternalTraitGeneratorByType {
    pub fn call(&self, trts: &Traits, trt: &trt::Trait) -> ROption<Erased> {
        (self.0)(trts as *const _, trt as *const _)
    }
}

impl std::fmt::Debug for InternalTraitGeneratorByType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[function]")
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct InternalTraitGeneratorByTrait(
    extern "C" fn(*const (), *const Traits, *const Type) -> ROption<Erased>,
    *const (),
);

unsafe impl Send for InternalTraitGeneratorByTrait {}
unsafe impl Sync for InternalTraitGeneratorByTrait {}

impl InternalTraitGeneratorByTrait {
    pub fn call(&self, trts: &Traits, tp: &Type) -> ROption<Erased> {
        (self.0)(self.1, trts as *const _, tp as *const _)
    }
}

impl std::fmt::Debug for InternalTraitGeneratorByTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[function]")
    }
}

#[derive(StableAbi)]
#[repr(C)]
struct InternalTraitGenerator(
    extern "C" fn(*const Traits, *const Type, *const trt::Trait) -> ROption<Erased>,
);

impl InternalTraitGenerator {
    pub fn call(&self, trts: &Traits, tp: &Type, trt: &trt::Trait) -> ROption<Erased> {
        (self.0)(trts as *const _, tp as *const _, trt as *const _)
    }
}

impl std::fmt::Debug for InternalTraitGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[function]")
    }
}

extern "C" fn apply_trait_generator_by_trait(
    f: *const (),
    traits: &Traits,
    tp: &Type,
) -> ROption<Erased> {
    (unsafe { std::mem::transmute::<*const (), TraitGeneratorByTrait>(f) })(traits, tp)
}

extern "C" fn trait_generator_to_erased<Trt: trt::GreaseTrait>(
    f: *const (),
    traits: *const Traits,
    tp: *const Type,
) -> ROption<Erased> {
    (unsafe { std::mem::transmute::<*const (), fn(&Traits, &Type) -> ROption<Trt::Impl>>(f) })(
        unsafe { traits.as_ref() }.unwrap(),
        unsafe { tp.as_ref() }.unwrap(),
    )
    .map(Erased::new)
}

/// Trait interface.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct Traits {
    inner: RArc<Inner>,
}

/// Trait interface implementation.
#[derive(StableAbi)]
#[repr(C)]
struct Inner {
    traits: RRwLock<trt::Traits>,
    generators: RRwLock<Generators>,
}

#[derive(Debug, Default, StableAbi)]
#[repr(C)]
struct Generators {
    by_type: RHashMap<Type, RVec<InternalTraitGeneratorByType>>,
    by_trait: RHashMap<trt::Trait, RVec<InternalTraitGeneratorByTrait>>,
    general: RVec<InternalTraitGenerator>,
}

impl std::fmt::Debug for Inner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Inner")
            .field("traits", &*self.traits.read())
            .field("generators", &self.generators.read())
            .finish()
    }
}

impl Default for Inner {
    fn default() -> Self {
        Inner {
            traits: RRwLock::new(Default::default()),
            generators: RRwLock::new(Default::default()),
        }
    }
}

impl Default for Traits {
    fn default() -> Self {
        Traits {
            inner: RArc::new(Default::default()),
        }
    }
}

enum TraitInner<T> {
    Ref(trt::Ref<T>),
    Fut(futures::future::BoxFuture<'static, crate::Result<trt::Ref<T>>>),
}

pub struct Trait<T> {
    inner: TraitInner<T>,
}

impl<T> Trait<T> {
    fn new_ref(r: trt::Ref<T>) -> Self {
        Trait {
            inner: TraitInner::Ref(r),
        }
    }

    fn new_fut(fut: futures::future::BoxFuture<'static, crate::Result<trt::Ref<T>>>) -> Self {
        Trait {
            inner: TraitInner::Fut(fut),
        }
    }
}

impl<T> From<trt::Ref<T>> for Trait<T> {
    fn from(r: trt::Ref<T>) -> Self {
        Trait::new_ref(r)
    }
}

impl<T: Sync> Trait<T> {
    pub async fn as_ref(&mut self) -> crate::Result<trt::Ref<T>> {
        Ok(match &mut self.inner {
            TraitInner::Ref(r) => r.clone(),
            TraitInner::Fut(f) => f.await?.clone(),
        })
    }
}

impl Traits {
    /// Create an empty traits runtime.
    pub fn new() -> Self {
        Default::default()
    }

    /// Get a trait for a particular Value's type, if it is implemented.
    pub fn get<Trt: trt::GreaseTrait>(&self, v: &Value) -> Option<Trait<Trt::Impl>> {
        // If the type is available immediately, use it, otherwise make an async trait.
        match v.grease_type_immediate() {
            Some(tp) => self.get_type::<Trt>(tp).map(Trait::new_ref),
            None => {
                let mut v = v.clone();
                let traits = self.clone();
                Some(Trait::new_fut(
                    async move {
                        traits
                            .get_type::<Trt>(v.grease_type().await?)
                            .ok_or("trait not implemented".into())
                    }
                    .boxed(),
                ))
            }
        }
    }

    /// Get a trait for a Type, if it is implemented.
    pub fn get_type<Trt: trt::GreaseTrait>(&self, tp: &Type) -> Option<trt::Ref<Trt::Impl>> {
        self.get_impl(tp, &Trt::grease_trait())
            .map(|r| unsafe { trt::Ref::new(r) })
    }

    /// Get a trait, if implemented, as the erased implementation.
    pub fn get_impl(&self, tp: &Type, trt: &trt::Trait) -> Option<RArc<Erased>> {
        // If we leave this expression in the match expression, it will not drop the read lock until
        // the match expression is closed.
        let imp = self.inner.traits.read().get_impl(tp, trt);
        match imp {
            Some(v) => v,
            None => {
                // Try generators to find implementation.
                if let Some(imp) = self.try_generators(tp, trt) {
                    unsafe {
                        self.inner
                            .traits
                            .write()
                            .insert_unchecked(tp.clone(), trt.clone(), imp);
                    }
                } else {
                    self.inner
                        .traits
                        .write()
                        .insert_empty(tp.clone(), trt.clone());
                }
                self.inner
                    .traits
                    .read()
                    .get_impl(tp, trt)
                    .expect("trait implementation must exist")
            }
        }
    }

    /// Try all generators with the given type and trait.
    fn try_generators(&self, tp: &Type, trt: &trt::Trait) -> Option<Erased> {
        let generators = self.inner.generators.read();
        if let Some(gens) = generators.by_type.get(tp) {
            for g in gens {
                if let ROption::RSome(imp) = g.call(self, trt) {
                    return Some(imp);
                }
            }
        }
        if let Some(gens) = generators.by_trait.get(trt) {
            for g in gens {
                if let ROption::RSome(imp) = g.call(self, tp) {
                    return Some(imp);
                }
            }
        }
        for g in &generators.general {
            if let ROption::RSome(imp) = g.call(self, tp, trt) {
                return Some(imp);
            }
        }
        None
    }

    /// Add a trait implementation for the given rust type.
    pub fn add_impl_for_type<Tp, Trt>(&mut self, implementation: Trt::Impl)
    where
        Tp: GreaseType,
        Trt: trt::GreaseTrait,
    {
        self.inner
            .traits
            .write()
            .insert_for_type::<Tp, Trt>(implementation);
    }

    /// Add a trait implementation for the given type.
    pub fn add_impl<Trt: trt::GreaseTrait>(&mut self, tp: Type, implementation: Trt::Impl) {
        self.inner.traits.write().insert::<Trt>(tp, implementation);
    }

    /// Add a trait generator.
    ///
    /// Trait generators may provide an implementation of arbitrary type/trait combinations.
    pub unsafe fn add_generator(&mut self, gen: TraitGenerator) {
        self.inner
            .generators
            .write()
            .general
            .push(InternalTraitGenerator(std::mem::transmute(gen)));
    }

    /// Add a trait generator by type.
    ///
    /// Trait generators by type may provide an implemention of traits for the given type.
    pub unsafe fn add_generator_by_type(&mut self, tp: Type, gen: TraitGeneratorByType) {
        self.inner
            .generators
            .write()
            .by_type
            .entry(tp)
            .or_default()
            .push(InternalTraitGeneratorByType(std::mem::transmute(gen)));
    }

    /// Add a trait generator by rust type.
    ///
    /// Trait generators by type may provide an implemention of traits for the given type.
    pub unsafe fn add_generator_by_type_for_type<Tp: GreaseType>(
        &mut self,
        gen: TraitGeneratorByType,
    ) {
        self.add_generator_by_type(Tp::grease_type(), gen);
    }

    /// Add a trait generator by trait.
    ///
    /// Trait generators by trait may provide an implementation of the given trait for many types.
    pub unsafe fn add_generator_by_trait(&mut self, trt: trt::Trait, gen: TraitGeneratorByTrait) {
        self.inner
            .generators
            .write()
            .by_trait
            .entry(trt)
            .or_default()
            .push(InternalTraitGeneratorByTrait(
                std::mem::transmute(&apply_trait_generator_by_trait),
                gen as *const (),
            ));
    }

    /// Add a trait generator by trait impl.
    ///
    /// Trait generators by trait may provide an implementation of the given trait for many types.
    pub fn add_generator_by_trait_for_trait<Trt: trt::GreaseTrait>(
        &mut self,
        gen: fn(&Traits, &Type) -> ROption<Trt::Impl>,
    ) {
        self.inner
            .generators
            .write()
            .by_trait
            .entry(Trt::grease_trait())
            .or_default()
            .push(InternalTraitGeneratorByTrait(
                trait_generator_to_erased::<Trt>,
                gen as *const (),
            ));
    }
}
