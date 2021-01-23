//! The ergo runtime support crate.
//!
//! All types (where necessary) are ABI-stable.

use abi_stable::{
    external_types::RMutex,
    rvec,
    std_types::{RArc, RHashMap, ROption, RVec},
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
pub mod metadata;
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

pub mod plugin {
    pub use plugin_tls::Context;
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
    env: RVec<RArc<RMutex<ScriptEnv>>>,
    env_set: ROption<RArc<RMutex<ScriptEnv>>>,
    pub loading: RArc<RMutex<RVec<PathBuf>>>,
    pub load_cache: RArc<RMutex<BstMap<PathBuf, EvalResultAbi>>>,
    initial_load_path: RVec<PathBuf>,
    pub mod_path: ROption<PathBuf>,
    pub current_load_path: RVec<PathBuf>,
    pub lint: bool,
    keyed_lifetime: RArc<RMutex<RHashMap<grease::types::Type, RArc<Erased>>>>,
    // It is important that lifetime is the last member of the runtime, so that when dropped it is
    // the last thing dropped. Other members of this struct, like env, load_cache, and
    // keyed_lifetime, may depend on something in this vec (like a loaded plugin).
    lifetime: RArc<RMutex<RVec<Erased>>>,
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

impl Runtime {
    /// Create a new runtime.
    pub fn new(
        context: Context,
        global_env: ScriptEnv,
        initial_load_path: Vec<std::path::PathBuf>,
    ) -> Self {
        Runtime {
            context,
            global_env,
            env: Default::default(),
            env_set: ROption::RNone,
            loading: RArc::new(RMutex::new(Default::default())),
            load_cache: RArc::new(RMutex::new(Default::default())),
            mod_path: ROption::RNone,
            initial_load_path: initial_load_path.into_iter().map(|p| p.into()).collect(),
            current_load_path: Default::default(),
            lint: false,
            keyed_lifetime: RArc::new(RMutex::new(Default::default())),
            lifetime: RArc::new(RMutex::new(Default::default())),
        }
    }

    /// Call a function on the current scoped environment.
    pub fn env_current<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut ScriptEnv) -> R,
    {
        let mut guard = self
            .env
            .last()
            .expect("invalid env access; no environment")
            .lock();
        f(&mut *guard)
    }

    fn env_current_set<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut ScriptEnv) -> R,
    {
        let mut guard = self
            .env_set
            .as_ref()
            .expect("invalid env access; no environment")
            .lock();
        f(&mut *guard)
    }

    /// Insert a binding into the current scoped environment.
    pub fn env_insert<T: Into<EvalResult>>(&self, k: Value, v: T) {
        self.env_current_set(|c| c.insert(k, v.into().into()));
    }

    /// Extend the current scoped environment.
    pub fn env_extend<T: IntoIterator<Item = (Value, EvalResultAbi)>>(&mut self, v: T) {
        self.env_current_set(|c| c.extend(v.into_iter().map(|(k, v)| (k, v.into()))));
    }

    /// Get the current set environment.
    ///
    /// Panics if no environment is set (with `env_set_to_current`).
    pub fn env_to_set(&self) -> RArc<RMutex<ScriptEnv>> {
        self.env_set
            .as_ref()
            .expect("invalid env access; no environment set")
            .clone()
    }

    /// Get a binding from the current environment.
    pub fn env_get<Q: ?Sized>(&self, k: &Q) -> Option<EvalResult>
    where
        Value: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env
            .iter()
            .rev()
            .find_map(|m| m.lock().get(k).map(|v| v.clone()))
            .or_else(|| self.global_env.get(k).map(|v| v.clone()))
            .map(|v| v.into_result())
    }

    /// Flatten the current environment.
    pub fn env_flatten(&self) -> ScriptEnv {
        self.env.iter().fold(Default::default(), |mut e, m| {
            e.append(&mut m.lock().clone());
            e
        })
    }

    /// Call the given function in a new scoped environment.
    pub fn env_scoped<'b, F, R>(&'b mut self, f: F) -> BoxFuture<'b, (R, ScriptEnv)>
    where
        F: for<'a> FnOnce(&'a mut Self) -> BoxFuture<'a, R> + Send + 'b,
        Self: Send,
    {
        async move {
            self.env.push(RArc::new(RMutex::new(Default::default())));
            let ret = f(self).await;
            let env = self.env.pop().expect("env push/pop is not balanced");
            let env = match RArc::try_unwrap(env) {
                Ok(v) => v.into_inner(),
                Err(e) => e.lock().clone(),
            };
            (ret, env)
        }
        .boxed()
    }

    /// Use the current environment scope as the env set target.
    pub fn env_set_to_current<'b, F, R>(&'b mut self, f: F) -> BoxFuture<'b, R>
    where
        F: for<'a> FnOnce(&'a mut Self) -> BoxFuture<'a, R> + Send + 'b,
        Self: Send,
    {
        async move {
            let prev_env_set = self
                .env_set
                .replace(self.env.last().expect("no environment set").clone());
            let ret = f(self).await;
            self.env_set = prev_env_set;
            ret
        }
        .boxed()
    }

    /// Return an env containing all the env set targets.
    pub fn env_set_gather<'b, F, R>(&'b mut self, f: F) -> BoxFuture<'b, (R, ScriptEnv)>
    where
        // instead of 'static, F should be 'b and 'a: 'b, but there's no way to constrain 'a
        F: for<'a> FnOnce(&'a mut Self) -> BoxFuture<'a, R> + Send + 'static,
        Self: Send,
    {
        self.env_scoped(move |ctx| ctx.env_set_to_current(f))
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

    /// Clear the local environment in a new copy of the runtime.
    pub fn empty(&self) -> Self {
        Runtime {
            env: rvec![RArc::new(RMutex::new(Default::default()))],
            ..self.clone()
        }
    }

    /// Get the effective directory of the current runtime instance.
    ///
    /// This will be the parent directory of mod_path if set, otherwise the program's current
    /// directory.
    pub fn mod_dir(&self) -> std::path::PathBuf {
        if let ROption::RSome(p) = &self.mod_path {
            p.as_ref()
                .parent()
                .expect("script file path invalid")
                .to_owned()
        } else {
            std::env::current_dir().expect("failed to get current directory")
        }
    }

    /// Set current_load_path to the mod_dir followed by the configured initial load path.
    pub fn reset_load_path(&mut self) {
        self.current_load_path = Default::default();
        self.current_load_path.push(self.mod_dir().into());
        self.current_load_path
            .extend(self.initial_load_path.clone());
    }
}

/// Command arguments interface.
#[derive(Clone, Debug, StableAbi)]
#[repr(C)]
pub struct UncheckedArguments {
    /// Positional arguments in this vec are in reverse order; use Iterator to access them.
    pub positional: RVec<Source<Value>>,
    pub non_positional: BstMap<Source<Value>, Source<Value>>,
}

impl UncheckedArguments {
    fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<Source<Value>, Source<Value>>,
    ) -> Self {
        UncheckedArguments {
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

    /// Return whether there were unused non-positional arguments, and add errors for each unused
    /// argument to the context errors.
    pub fn unused_non_positional(&mut self) -> Result<()> {
        let kw = std::mem::take(&mut self.non_positional);
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
    pub fn unused_positional(&mut self) -> Result<()> {
        if self.positional.is_empty() {
            Ok(())
        } else {
            let mut vec: Vec<Error> = Vec::new();
            while let Some(v) = self.next() {
                vec.push(v.with(error::UnexpectedPositionalArguments).into());
            }
            Err(Error::aggregate(vec))
        }
    }

    /// Return whether there were unused arguments (of any kind), and add errors for each unused
    /// argument to the context errors.
    pub fn unused_arguments(&mut self) -> Result<()> {
        match (self.unused_positional(), self.unused_non_positional()) {
            (Err(a), Err(b)) => Err(Error::aggregate(vec![a, b])),
            (Err(a), _) => Err(a),
            (_, Err(b)) => Err(b),
            _ => Ok(()),
        }
    }

    /// Create an Arguments, which checks for all values being consumed.
    pub fn checked(self) -> Arguments {
        self.into()
    }
}

impl Default for UncheckedArguments {
    fn default() -> Self {
        Self::new(Default::default(), Default::default())
    }
}

impl Iterator for UncheckedArguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.positional.pop()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.positional.len()))
    }
}

impl From<&UncheckedArguments> for grease::value::Dependencies {
    fn from(args: &UncheckedArguments) -> Self {
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

/// Command arguments.
///
/// Checks whether all arguments have been consumed when dropped.
#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct Arguments {
    inner: UncheckedArguments,
}

impl Arguments {
    pub fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<Source<Value>, Source<Value>>,
    ) -> Self {
        Arguments {
            inner: UncheckedArguments::new(positional, non_positional),
        }
    }

    pub fn positional(positional: Vec<Source<Value>>) -> Self {
        Self::new(positional, Default::default())
    }

    pub fn unchecked(mut self) -> UncheckedArguments {
        std::mem::take(&mut self.inner)
    }
}

impl Iterator for Arguments {
    type Item = Source<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl std::ops::Deref for Arguments {
    type Target = UncheckedArguments;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for Arguments {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Drop for Arguments {
    /// Asserts that the function arguments have all been consumed prior to being dropped.
    fn drop(&mut self) {
        assert!(self.inner.is_empty() && self.inner.non_positional.is_empty());
    }
}

impl From<UncheckedArguments> for Arguments {
    fn from(inner: UncheckedArguments) -> Self {
        Arguments { inner }
    }
}

impl From<&Arguments> for grease::value::Dependencies {
    fn from(args: &Arguments) -> Self {
        (&args.inner).into()
    }
}
