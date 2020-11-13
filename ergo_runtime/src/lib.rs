//! The ergo runtime support crate.
//!
//! All types (where necessary) are ABI-stable.

use abi_stable::{
    external_types::RMutex,
    std_types::{RArc, RHashMap, RVec},
    StableAbi,
};
use futures::future::{BoxFuture, FutureExt};
use grease::bst::BstMap;
use grease::path::PathBuf;
use grease::runtime::Context;
use grease::type_erase::{Eraseable, Erased, Ref};
use grease::{Error, Value};
use std::collections::BTreeMap;
use std::iter::FromIterator;

pub use futures::future;

pub mod context_ext;
pub mod error;
pub mod source;
pub mod traits;
pub mod types;

pub use context_ext::ContextExt;

pub use source::Source;

pub use ergo_runtime_macro::plugin_entry;

/// The Result type.
pub type Result<T> = grease::Result<T>;

/// The ABI-safe RResult type.
pub type ResultAbi<T> = grease::error::RResult<T>;

pub trait ResultIterator<T> {
    /// Collect values into a Result, where errors will be aggregated.
    fn collect_result<R: FromIterator<T>>(self) -> Result<R>;
}

impl<T, I> ResultIterator<T> for I
where
    I: IntoIterator<Item = Result<T>>,
{
    fn collect_result<R: FromIterator<T>>(self) -> Result<R> {
        let mut errs = Vec::new();
        let r = R::from_iter(self.into_iter().filter_map(|v| match v {
            Err(e) => {
                errs.push(e);
                None
            }
            Ok(v) => Some(v),
        }));
        if errs.is_empty() {
            Ok(r)
        } else {
            Err(Error::aggregate(errs))
        }
    }
}

/// Runtime evaluation result.
pub type EvalResult = Result<Source<Value>>;

/// ABI-safe runtime evaluation result.
pub type EvalResultAbi = ResultAbi<Source<Value>>;

/// Script environment bindings.
pub type ScriptEnv = BstMap<Value, EvalResultAbi>;

#[derive(Clone, StableAbi)]
#[repr(C)]
/// The script runtime context.
pub struct Runtime {
    context: Context,
    /// The global env exists as an optimization to exclude values that will
    /// always be accessible from being wrapped in capture environments. It also makes
    /// clearing the environment when loading separate scripts easier.
    global_env: ScriptEnv,
    env: RVec<ScriptEnv>,
    pub loading: RArc<RMutex<RVec<PathBuf>>>,
    pub load_cache: RArc<RMutex<BstMap<PathBuf, EvalResultAbi>>>,
    pub lint: bool,
    lifetime: RArc<RMutex<RVec<Erased>>>,
    keyed_lifetime: RArc<RMutex<RHashMap<grease::types::Type, RArc<Erased>>>>,
}

impl std::ops::Deref for Runtime {
    type Target = Context;

    fn deref(&self) -> &Self::Target {
        &self.context
    }
}

impl std::ops::DerefMut for Runtime {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.context
    }
}

impl context_ext::AsContext for Runtime {
    fn as_context(&self) -> &Context {
        &self.context
    }
}

/// Script function call context.
#[derive(StableAbi)]
#[repr(C)]
pub struct FunctionCall<'a> {
    runtime: &'a mut Runtime,
    pub args: FunctionArguments,
    pub call_site: Source<()>,
}

impl<'a> FunctionCall<'a> {
    /// Create a new FunctionCall context.
    pub fn new(ctx: &'a mut Runtime, args: FunctionArguments, call_site: Source<()>) -> Self {
        FunctionCall {
            runtime: ctx,
            args,
            call_site,
        }
    }
}

impl std::ops::Deref for FunctionCall<'_> {
    type Target = Runtime;

    fn deref(&self) -> &Self::Target {
        self.runtime
    }
}

impl std::ops::DerefMut for FunctionCall<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.runtime
    }
}

impl context_ext::AsContext for FunctionCall<'_> {
    fn as_context(&self) -> &Context {
        self.runtime.as_context()
    }
}

/// Function call arguments interface.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct UncheckedFunctionArguments {
    /// Positional arguments in this vec are in reverse order; use Iterator to access them.
    pub positional: RVec<Source<Value>>,
    pub non_positional: BstMap<Source<Value>, Source<Value>>,
}

/// Function call arguments.
///
/// Checks whether all arguments have been consumed when dropped.
#[derive(Debug, Default, StableAbi)]
#[repr(C)]
pub struct FunctionArguments {
    inner: UncheckedFunctionArguments,
}

impl Runtime {
    /// Create a new runtime.
    pub fn new(context: Context, global_env: ScriptEnv) -> Self {
        Runtime {
            context,
            global_env,
            env: Default::default(),
            loading: RArc::new(RMutex::new(Default::default())),
            load_cache: RArc::new(RMutex::new(Default::default())),
            lint: false,
            lifetime: RArc::new(RMutex::new(Default::default())),
            keyed_lifetime: RArc::new(RMutex::new(Default::default())),
        }
    }

    /// Get the current scoped environment.
    fn env_current(&mut self) -> &mut ScriptEnv {
        self.env
            .last_mut()
            .expect("invalid env access; no environment stack")
    }

    /// Insert a binding into the current scoped environment.
    pub fn env_insert<T: Into<EvalResult>>(&mut self, k: Value, v: T) {
        self.env_current().insert(k.into(), v.into().into());
    }

    /// Extend the current scoped environment.
    pub fn env_extend<T: IntoIterator<Item = (Value, EvalResultAbi)>>(&mut self, v: T) {
        self.env_current()
            .extend(v.into_iter().map(|(k, v)| (k, v.into())));
    }

    /// Remove a binding from the current scoped environment.
    pub fn env_remove<Q: ?Sized>(&mut self, k: &Q) -> Option<EvalResult>
    where
        Value: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env_current().remove(k).map(|v| v.into())
    }

    /// Get a binding from the current environment.
    pub fn env_get<Q: ?Sized>(&self, k: &Q) -> Option<std::result::Result<&Source<Value>, &Error>>
    where
        Value: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env
            .iter()
            .rev()
            .find_map(|m| m.get(k))
            .or_else(|| self.global_env.get(k))
            .map(|v| v.as_ref().into_result())
    }

    /// Flatten the current environment.
    pub fn env_flatten(&self) -> ScriptEnv {
        self.env.iter().fold(Default::default(), |mut e, m| {
            e.append(&mut m.clone());
            e
        })
    }

    /// Store a value for the duration of the runtime's lifetime.
    pub fn lifetime<T: Eraseable>(&mut self, v: T) {
        self.lifetime.lock().push(Erased::new(v));
    }

    /// Load or create and load a value in the runtime.
    ///
    /// The passed closure is called when the value has not yet been loaded.
    pub fn shared_state<T: Eraseable + grease::types::GreaseType, F>(
        &mut self,
        missing: F,
    ) -> grease::Result<Ref<T, RArc<Erased>>>
    where
        F: FnOnce() -> grease::Result<T>,
    {
        let mut guard = self.keyed_lifetime.lock();
        if !guard.contains_key(&T::grease_type()) {
            guard.insert(T::grease_type(), RArc::new(Erased::new(missing()?)));
        }
        Ok(unsafe {
            Ref::new(
                guard
                    .get(&T::grease_type())
                    .expect("type must have been inserted")
                    .clone(),
            )
        })
    }

    /// Clear the environment of all values.
    pub fn clear_env(&mut self) {
        self.global_env.clear();
        self.env.clear();
    }

    /// Clear the local environment in a new copy of the runtime to be used in
    /// a delayed context.
    pub fn delayed(&self) -> Self {
        Runtime {
            context: self.context.clone(),
            global_env: self.global_env.clone(),
            env: Default::default(),
            loading: self.loading.clone(),
            load_cache: self.load_cache.clone(),
            lint: self.lint,
            lifetime: self.lifetime.clone(),
            keyed_lifetime: self.keyed_lifetime.clone(),
        }
    }
}

impl FunctionCall<'_> {
    /// Return whether there were unused non-positional arguments, and add errors for each unused
    /// argument to the context errors.
    pub fn unused_non_positional(&mut self) -> std::result::Result<(), Error> {
        let kw = std::mem::take(&mut self.args.non_positional);
        if kw.is_empty() {
            Ok(())
        } else {
            Err(Error::aggregate(kw.into_iter().map(|(k, v)| {
                crate::source::IntoSource::into_source((k.source(), v.source()))
                    .with(error::UnexpectedNonPositionalArgument)
                    .into()
            })))
        }
    }

    /// Return whether there were unused positional arguments, and add errors for each unused
    /// argument to the context errors.
    pub fn unused_positional(&mut self) -> std::result::Result<(), Error> {
        if self.args.is_empty() {
            Ok(())
        } else {
            let mut vec: Vec<Error> = Vec::new();
            while let Some(v) = self.args.next() {
                vec.push(v.with(error::UnexpectedPositionalArguments).into());
            }
            Err(Error::aggregate(vec))
        }
    }

    /// Return whether there were unused arguments (of any kind), and add errors for each unused
    /// argument to the context errors.
    pub fn unused_arguments(&mut self) -> std::result::Result<(), Error> {
        match (self.unused_positional(), self.unused_non_positional()) {
            (Err(a), Err(b)) => Err(Error::aggregate(vec![a, b])),
            (Err(a), _) => Err(a),
            (_, Err(b)) => Err(b),
            _ => Ok(()),
        }
    }

    /// Add the error context into the given value, using the function call site.
    pub fn imbue_error_context<S: ToString>(&self, v: Value, err: S) -> Value {
        self.call_site.clone().with(err).imbue_error_context(v)
    }
}

impl UncheckedFunctionArguments {
    fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<Source<Value>, Source<Value>>,
    ) -> Self {
        UncheckedFunctionArguments {
            positional: positional.into_iter().rev().collect(),
            non_positional: non_positional.into_iter().collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.positional.len()
    }

    pub fn kw(&mut self, key: &str) -> Option<Source<Value>> {
        self.kw_value(&crate::types::String::from(key).into())
    }

    pub fn kw_value(&mut self, key: &Value) -> Option<Source<Value>> {
        self.non_positional.remove(key)
    }

    pub fn peek(&mut self) -> Option<&Source<Value>> {
        self.positional.last()
    }

    pub fn clear(&mut self) {
        self.positional.clear();
        self.non_positional.clear()
    }
}

impl Default for UncheckedFunctionArguments {
    fn default() -> Self {
        Self::new(Default::default(), Default::default())
    }
}

impl Iterator for UncheckedFunctionArguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.positional.pop()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.positional.len()))
    }
}

impl From<&UncheckedFunctionArguments> for grease::value::Dependencies {
    fn from(args: &UncheckedFunctionArguments) -> Self {
        Self::ordered(
            args.positional
                .iter()
                .map(|v| grease::value::Dependency::from(&**v))
                .chain(
                    args.non_positional
                        .iter()
                        .map(|(k, _)| grease::value::Dependency::from(&**k)),
                )
                .chain(
                    args.non_positional
                        .iter()
                        .map(|(_, v)| grease::value::Dependency::from(&**v)),
                ),
        )
    }
}

impl FunctionArguments {
    pub fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<Source<Value>, Source<Value>>,
    ) -> Self {
        FunctionArguments {
            inner: UncheckedFunctionArguments::new(positional, non_positional),
        }
    }

    pub fn positional(positional: Vec<Source<Value>>) -> Self {
        Self::new(positional, Default::default())
    }

    pub fn unchecked(mut self) -> UncheckedFunctionArguments {
        std::mem::take(&mut self.inner)
    }
}

impl Iterator for FunctionArguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl std::ops::Deref for FunctionArguments {
    type Target = UncheckedFunctionArguments;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for FunctionArguments {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Drop for FunctionArguments {
    /// Asserts that the function arguments have all been consumed prior to being dropped.
    fn drop(&mut self) {
        assert!(self.inner.is_empty() && self.inner.non_positional.is_empty());
    }
}

impl From<UncheckedFunctionArguments> for FunctionArguments {
    fn from(inner: UncheckedFunctionArguments) -> Self {
        FunctionArguments { inner }
    }
}

pub trait GetEnv {
    fn get_env(&mut self) -> &mut RVec<ScriptEnv>;
}

impl GetEnv for Runtime {
    fn get_env(&mut self) -> &mut RVec<ScriptEnv> {
        &mut self.env
    }
}

impl GetEnv for FunctionCall<'_> {
    fn get_env(&mut self) -> &mut RVec<ScriptEnv> {
        &mut self.env
    }
}

pub trait ContextEnv: GetEnv {
    /// Call the given function while substituting the current environment.
    fn substituting_env<'b, F, R>(&'b mut self, mut env: RVec<ScriptEnv>, f: F) -> BoxFuture<'b, R>
    where
        F: for<'a> FnOnce(&'a mut Self) -> BoxFuture<'a, R> + Send + 'b,
        Self: Send,
    {
        async move {
            std::mem::swap(self.get_env(), &mut env);
            let ret = f(self).await;
            std::mem::swap(self.get_env(), &mut env);
            ret
        }
        .boxed()
    }

    /// Call the given function in a new, scoped environment.
    fn env_scoped<'b, F, R>(&'b mut self, env: ScriptEnv, f: F) -> BoxFuture<'b, (R, ScriptEnv)>
    where
        F: for<'a> FnOnce(&'a mut Self) -> BoxFuture<'a, R> + Send + 'b,
        Self: Send,
    {
        async move {
            self.get_env().push(env);
            let ret = f(self).await;
            let env = self.get_env().pop().expect("env push/pop is not balanced");
            (ret, env)
        }
        .boxed()
    }
}

impl<T: GetEnv> ContextEnv for T {}
