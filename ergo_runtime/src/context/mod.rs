//! The runtime context.

use crate as ergo_runtime;
use crate::abi_stable::{std_types::RArc, StableAbi};
use crate::{
    type_system::{ErgoTrait, ErgoType, Type},
    TypedValue, Value,
};
use std::fmt;

mod diagnostic_sources;
mod dynamic_scope;
mod error_scope;
mod evaluating;
mod log;
mod shared_state;
mod store;
mod task;
mod traits;

use self::log::EmptyLogTarget;

/// Create a literal item name.
///
/// Item names must contain only ascii alphanumeric characters.
pub use ergo_runtime_macro::item_name;

pub use self::log::{
    logger_ref, Log, LogEntry, LogLevel, LogTarget, LogTask, LogTaskKey, Logger, LoggerRef,
    RecordingWork, Work,
};
pub use diagnostic_sources::{SourceId, Sources};
pub use dynamic_scope::{DynamicScope, DynamicScopeKey, DynamicScopeRef};
pub use error_scope::ErrorScope;
pub use evaluating::Evaluating;
pub use shared_state::SharedState;
pub use store::{Item, ItemContent, ItemName, Store};
pub use task::{thread_id, LocalKey, TaskManager, TaskPermit};
pub use traits::{TraitGenerator, TraitGeneratorByTrait, TraitGeneratorByType, Traits};

/// Runtime context which is immutable.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct GlobalContext {
    /// The logging interface.
    pub log: Log,
    /// The shared state interface.
    pub shared_state: SharedState,
    /// The storage interface.
    pub store: Store,
    /// The task management interface.
    pub task: TaskManager,
    /// The type traits interface.
    pub traits: Traits,
}

impl GlobalContext {
    /// Get the shared set of diagnostic sources loaded in the runtime.
    pub fn diagnostic_sources(&self) -> shared_state::SharedStateRef<Sources> {
        self.shared_state
            .get::<Sources, _>(|| {
                Ok(Sources::new(
                    self.store.item(item_name!("diagnostic_sources")),
                ))
            })
            .unwrap()
    }
}

/// Runtime context.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct Context {
    global: RArc<GlobalContext>,
    /// The dynamic scoping interface.
    pub dynamic_scope: DynamicScope,
    /// The error propagation interface.
    pub error_scope: ErrorScope,
    /// The currently evaluating values.
    pub(crate) evaluating: Evaluating,
}

/// A builder for a Context.
#[derive(Default)]
pub struct ContextBuilder {
    logger: Option<LoggerRef>,
    store_dir: Option<std::path::PathBuf>,
    threads: Option<usize>,
    aggregate_errors: Option<bool>,
    error_scope: Option<ErrorScope>,
    detect_deadlock: bool,
}

trait Fork {
    /// Create a fork of a value.
    fn fork(&self) -> Self;

    /// Join with a forked value.
    fn join(&self, forked: Self);
}

impl<T: Clone> Fork for T {
    fn fork(&self) -> Self {
        self.clone()
    }

    fn join(&self, _forked: Self) {}
}

/// An error produced by the ContextBuilder.
#[derive(Debug)]
pub enum BuilderError {
    TaskManagerError(futures::io::Error),
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TaskManagerError(e) => write!(f, "task manager error: {}", e),
        }
    }
}

impl std::error::Error for BuilderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BuilderError::TaskManagerError(e) => Some(e),
        }
    }
}

impl ContextBuilder {
    /// Set the logger to use.
    pub fn logger<T: LogTarget + Send + 'static>(mut self, logger: T) -> Self {
        let logger = logger_ref(logger);
        self.logger = Some(logger.into());
        self
    }

    /// Set the storage directory.
    pub fn storage_directory(mut self, dir: std::path::PathBuf) -> Self {
        self.store_dir = Some(dir);
        self
    }

    /// Set the number of threads to use.
    pub fn threads(mut self, threads: Option<usize>) -> Self {
        self.threads = threads;
        self
    }

    /// Set whether a single error causes immediate completion or not.
    /// Default is false.
    pub fn keep_going(mut self, value: bool) -> Self {
        self.aggregate_errors = Some(value);
        self
    }

    /// Set the top-level error handler of the context.
    pub fn error_handler<F>(mut self, on_error: F) -> Self
    where
        F: Fn(crate::Error) + Send + Sync + 'static,
    {
        self.error_scope = Some(ErrorScope::new(on_error));
        self
    }

    /// Set whether deadlock detection is enabled.
    /// Default is false.
    pub fn detect_deadlock(mut self, value: bool) -> Self {
        self.detect_deadlock = value;
        self
    }

    /// Create a Context.
    pub fn build(self) -> Result<Context, BuilderError> {
        Ok(Context {
            global: RArc::new(GlobalContext {
                log: Log::new(
                    self.logger
                        .unwrap_or_else(|| logger_ref(EmptyLogTarget).into()),
                ),
                shared_state: SharedState::new(),
                store: Store::new(self.store_dir.unwrap_or(std::env::temp_dir())),
                task: TaskManager::new(self.threads, self.aggregate_errors.unwrap_or(false))
                    .map_err(BuilderError::TaskManagerError)?,
                traits: Default::default(),
            }),
            dynamic_scope: Default::default(),
            error_scope: self.error_scope.unwrap_or_default(),
            evaluating: Evaluating::new(self.detect_deadlock),
        })
    }
}

impl std::ops::Deref for Context {
    type Target = GlobalContext;

    fn deref(&self) -> &Self::Target {
        &self.global
    }
}

crate::task_local! {
    static CURRENT_CONTEXT: Context;
}

impl Context {
    /// Create a ContextBuilder.
    pub fn builder() -> ContextBuilder {
        Default::default()
    }

    /// Apply a function on the current context.
    ///
    /// Panics if there is no context set.
    pub fn with<F, R>(f: F) -> R
    where
        F: FnOnce(&Self) -> R,
    {
        CURRENT_CONTEXT.with(|v| f(v.unwrap()))
    }

    /// Evaluate a value.
    pub async fn eval(value: &mut Value) -> crate::Result<()> {
        let mut sources: Vec<crate::source::Source<()>> = Vec::new();
        if Self::with(|ctx| ctx.evaluating.deadlock_detect_enabled()) {
            let mut set = std::collections::HashSet::<u128>::default();
            while !value.is_evaluated() {
                sources.push(crate::metadata::Source::get(&value));
                if !set.insert(value.immediate_id().await.id) {
                    *value = crate::error! {
                        labels: [
                            primary(crate::metadata::Source::get(&value).with("while evaluating this value"))
                        ],
                        error: "circular evaluation detected"
                    }.into();
                    break;
                }
                value.eval_once().await;
            }
        } else {
            while !value.is_evaluated() {
                value.eval_once().await;
            }
        }
        sources.dedup_by(|a, b| crate::source::Source::total_eq(a, b));
        if value.is_type::<crate::types::Error>() {
            let source = crate::metadata::Source::get(&value);
            if let Some(error) = value.as_mut::<crate::types::Error>() {
                error.modify_diagnostics(|d| {
                    use crate::error::DiagnosticInfo;
                    if d.labels.is_empty() {
                        d.add_primary_label(source.with("while evaluating this value"));
                        let mut iter = sources.iter().rev().enumerate();
                        iter.next();
                        for (i, s) in iter {
                            d.add_secondary_label(
                                s.with(format!("({}) while evaluating this value", i)),
                            );
                        }
                    }
                });
            }
            let error = value
                .clone()
                .as_type::<crate::types::Error>()
                .unwrap()
                .to_owned();
            Self::with(|ctx| ctx.error_scope.error(&error));
            return Err(error);
        }
        Ok(())
    }

    /// Get an ergo trait for the given value's type.
    ///
    /// Always returns None if the value is not yet evaluated.
    pub fn get_trait<Trt: ErgoTrait>(v: &Value) -> Option<Trt> {
        Self::with(|ctx| ctx.traits.get::<Trt>(v).map(Trt::create))
    }

    /// Get an ergo trait for the given type.
    pub fn get_trait_for_type<Trt: ErgoTrait>(tp: &Type) -> Option<Trt> {
        Self::with(|ctx| ctx.traits.get_type::<Trt>(tp).map(Trt::create))
    }

    /// Get the global context.
    pub fn global() -> RArc<GlobalContext> {
        Self::with(|ctx| ctx.global.clone())
    }

    /// Get the source path of a source, if any.
    pub fn source_path<T>(src: &crate::source::Source<T>) -> Option<std::path::PathBuf> {
        Self::global()
            .diagnostic_sources()
            .name(src.source_id)
            .and_then(|name| match name {
                diagnostic_sources::SourceName::Path(p) => Some(p.clone().into()),
                _ => None,
            })
    }

    /// Evaluate a value once.
    pub async fn eval_once(value: &mut Value) {
        value.eval_once().await
    }

    /// Evaluate all values.
    pub async fn eval_all<'a, I>(values: I) -> crate::Result<()>
    where
        I: IntoIterator<Item = &'a mut Value>,
    {
        Self::global()
            .task
            .join_all(values.into_iter().map(|v| Self::eval(v)))
            .await
            .map(|_| ())
    }

    /// Evaluate a value expecting a specific type, returning an error if it does not evaluate to a
    /// value of that type or if it evaluates to an Error type.
    pub async fn eval_as<T: ErgoType>(mut value: Value) -> crate::Result<TypedValue<T>> {
        Self::eval(&mut value).await?;
        match value.as_type::<T>() {
            Ok(v) => Ok(v),
            Err(e) => Err(crate::traits::type_error_for::<T>(e).into()),
        }
    }

    /// Fork the context, allowing changes of context values.
    pub async fn fork<Alter, Fut, Ret>(alter: Alter, fut: Fut) -> Ret
    where
        Alter: FnOnce(&mut Self),
        Fut: std::future::Future<Output = Ret> + Send,
    {
        let mut new_ctx = Self::with(|ctx| ctx.fork());
        alter(&mut new_ctx);

        let (new_ctx, ret) = CURRENT_CONTEXT.final_scope(new_ctx, fut).await;

        Self::with(move |ctx| ctx.join(new_ctx));
        ret
    }

    /// Spawn a new task.
    ///
    /// This necessarily forks the context.
    pub async fn spawn<'a, Alter, Fut, Ret>(
        priority: u32,
        alter: Alter,
        fut: Fut,
    ) -> crate::Result<Ret>
    where
        Alter: FnOnce(&mut Self),
        Fut: std::future::Future<Output = crate::Result<Ret>> + Send + 'static,
        Ret: Send + 'static,
    {
        let mut new_ctx = Self::with(|ctx| ctx.fork());
        alter(&mut new_ctx);

        match Self::global()
            .task
            .spawn_basic(priority, CURRENT_CONTEXT.final_scope(new_ctx, fut))
            .await
        {
            Ok((new_ctx, ret)) => {
                Self::with(move |ctx| ctx.join(new_ctx));
                ret
            }
            // If the task is aborted, there's no reason for the context to be joined as it
            // shouldn't have been accessed.
            Err(_) => Err(crate::Error::aborted()),
        }
    }

    /// Block on the given future, setting the task-local context.
    pub fn block_on<F: std::future::Future + Send>(&self, fut: F) -> F::Output {
        self.task.block_on(async move {
            let new_ctx = self.fork();
            let (new_ctx, ret) = CURRENT_CONTEXT.final_scope(new_ctx, fut).await;
            self.join(new_ctx);
            ret
        })
    }
}

impl Fork for Context {
    fn fork(&self) -> Self {
        Context {
            global: self.global.fork(),
            dynamic_scope: self.dynamic_scope.fork(),
            error_scope: self.error_scope.fork(),
            evaluating: self.evaluating.fork(),
        }
    }

    fn join(&self, forked: Self) {
        self.global.join(forked.global);
        self.dynamic_scope.join(forked.dynamic_scope);
        self.error_scope.join(forked.error_scope);
        self.evaluating.join(forked.evaluating);
    }
}
