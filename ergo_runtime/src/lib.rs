//! The ergo runtime support crate.
//!
//! All types (where necessary) are ABI-stable.

use abi_stable::{
    external_types::RMutex,
    sabi_trait,
    sabi_trait::prelude::*,
    std_types::{RArc, RBox, ROption, RString, RVec},
    StableAbi,
};
use futures::future::{BoxFuture, FutureExt};
use grease::bst::BstMap;
use grease::path::PathBuf;
use grease::runtime::Context;
use grease::type_erase::{Eraseable, Erased};
use grease::{Error, Value};
use std::collections::BTreeMap;
use std::iter::FromIterator;

pub use futures::future;

pub mod error;
pub mod source;
pub mod traits;
pub mod types;

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
pub type ScriptEnv = BstMap<RString, EvalResultAbi>;

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
    pub loading: RVec<PathBuf>,
    pub load_cache: RArc<RMutex<BstMap<PathBuf, EvalResultAbi>>>,
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

impl AsRef<Context> for Runtime {
    fn as_ref(&self) -> &Context {
        &self.context
    }
}

impl AsMut<Context> for Runtime {
    fn as_mut(&mut self) -> &mut Context {
        &mut self.context
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

#[sabi_trait]
pub trait PeekIter: Debug + Send {
    type Item;

    fn next(&mut self) -> ROption<Self::Item>;

    fn peek(&mut self) -> ROption<&Self::Item>;

    #[sabi(last_prefix_field)]
    fn len(&self) -> usize;
}

/// Function call arguments interface.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct UncheckedFunctionArguments {
    pub positional: PeekIter_TO<'static, RBox<()>, Source<Value>>,
    pub non_positional: BstMap<RString, Source<Value>>,
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
            loading: Default::default(),
            load_cache: RArc::new(RMutex::new(Default::default())),
            lifetime: RArc::new(RMutex::new(Default::default())),
        }
    }

    /// Get the current scoped environment.
    fn env_current(&mut self) -> &mut ScriptEnv {
        self.env
            .last_mut()
            .expect("invalid env access; no environment stack")
    }

    /// Insert a binding into the current scoped environment.
    pub fn env_insert<T: Into<EvalResult>>(&mut self, k: String, v: T) {
        self.env_current().insert(k.into(), v.into().into());
    }

    /// Extend the current scoped environment.
    pub fn env_extend<T: IntoIterator<Item = (RString, EvalResultAbi)>>(&mut self, v: T) {
        self.env_current()
            .extend(v.into_iter().map(|(k, v)| (k, v.into())));
    }

    /// Remove a binding from the current scoped environment.
    pub fn env_remove<Q: ?Sized>(&mut self, k: &Q) -> Option<EvalResult>
    where
        RString: std::borrow::Borrow<Q>,
        Q: Ord,
    {
        self.env_current().remove(k).map(|v| v.into())
    }

    /// Get a binding from the current environment.
    pub fn env_get<Q: ?Sized>(&self, k: &Q) -> Option<std::result::Result<&Source<Value>, &Error>>
    where
        RString: std::borrow::Borrow<Q>,
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
                v.with(error::UnexpectedNonPositionalArgument(k.into()))
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

impl<I> PeekIter for std::iter::Peekable<I>
where
    I: Iterator + std::fmt::Debug + Send + ExactSizeIterator,
    I::Item: std::fmt::Debug + Send,
{
    type Item = I::Item;

    fn next(&mut self) -> ROption<Self::Item> {
        Iterator::next(self).into()
    }

    fn peek(&mut self) -> ROption<&Self::Item> {
        std::iter::Peekable::peek(self).into()
    }

    fn len(&self) -> usize {
        ExactSizeIterator::len(self)
    }
}

impl<T> Iterator for PeekIter_TO<'_, RBox<()>, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        PeekIter::next(self).into()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(PeekIter::len(self)))
    }
}

impl<T> ExactSizeIterator for PeekIter_TO<'_, RBox<()>, T> {
    fn len(&self) -> usize {
        PeekIter::len(self)
    }
}

impl UncheckedFunctionArguments {
    fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<String, Source<Value>>,
    ) -> Self {
        UncheckedFunctionArguments {
            positional: PeekIter_TO::from_value(positional.into_iter().peekable(), TU_Opaque),
            non_positional: non_positional
                .into_iter()
                .map(|(k, v)| (k.into(), v))
                .collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.positional.len()
    }

    pub fn kw(&mut self, key: &str) -> Option<Source<Value>> {
        self.non_positional.remove(key)
    }

    pub fn peek(&mut self) -> Option<&Source<Value>> {
        self.positional.peek().into()
    }

    pub fn clear(&mut self) {
        while self.positional.next().is_some() {}
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
        self.positional.next().into()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.positional.len()))
    }
}

impl FunctionArguments {
    pub fn new(
        positional: Vec<Source<Value>>,
        non_positional: BTreeMap<String, Source<Value>>,
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

#[macro_export]
macro_rules! source_value_as {
    ($val:expr , $ty:ty , $ctx:expr) => {
        match $val.map(|v| v.typed::<$ty>()).transpose() {
            Ok(v) => Ok(v),
            Err(e) => {
                let (source, v) = e.take();
                Err(source
                    .with(format!(
                        "expected {}, found {}",
                        $crate::traits::type_name(
                            &$ctx.traits,
                            &<$ty as grease::types::GreaseType>::grease_type()
                        ),
                        $crate::traits::type_name(&$ctx.traits, &*v.grease_type())
                    ))
                    .into_grease_error())
            }
        }
    };
}
