//! The runtime context.

use crate::abi_stable::{std_types::RArc, StableAbi};
use crate::{
    type_system::{ErgoTrait, ErgoType, Type},
    TypedValue, Value,
};
use std::fmt;

mod backtrace;
mod diagnostic_sources;
mod dynamic_scope;
mod env;
mod error_scope;
mod log;
mod progress;
mod shared_state;
pub(crate) mod task;
mod traits;

use self::log::EmptyLogTarget;

pub use self::log::{
    logger_ref, Log, LogEntry, LogLevel, LogTarget, LogTask, LogTaskKey, Logger, LoggerRef,
    RecordingWork, Work,
};
pub use backtrace::Backtrace;
pub use diagnostic_sources::{SourceId, Sources};
pub use dynamic_scope::{DynamicScope, DynamicScopeKey, DynamicScopeRef};
pub use env::Environment;
pub use error_scope::ErrorScope;
pub use progress::Progress;
pub use shared_state::SharedState;
pub use task::{LocalKey, TaskManager, TaskPermit};
pub use traits::{TraitGenerator, TraitGeneratorByTrait, TraitGeneratorByType, Traits};

/// Runtime context which is immutable.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct GlobalContext {
    /// The logging interface.
    pub log: Log,
    /// The shared state interface.
    pub shared_state: SharedState,
    /// The environment interface.
    pub env: Environment,
    /// The task management interface.
    pub task: TaskManager,
    /// The type traits interface.
    pub traits: Traits,
    /// The progress tracking interface.
    ///
    /// This is primarily used internally for deadlock detection.
    pub progress: Progress,
}

impl GlobalContext {
    /// Get the shared set of diagnostic sources loaded in the runtime.
    pub fn diagnostic_sources(&self) -> shared_state::SharedStateRef<Sources> {
        self.shared_state
            .get::<Sources, _>(|| Ok(Sources::new()))
            .unwrap()
    }
}

/// Runtime context.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct Context {
    pub global: RArc<GlobalContext>,
    /// The dynamic scoping interface.
    pub dynamic_scope: DynamicScope,
    /// The error propagation interface.
    pub error_scope: ErrorScope,
    /// The backtrace storage interface.
    pub backtrace: Backtrace,
}

/// A builder for a Context.
#[derive(Default)]
pub struct ContextBuilder {
    logger: Option<LoggerRef>,
    storage_dir: Option<std::path::PathBuf>,
    threads: Option<usize>,
    aggregate_errors: Option<bool>,
    error_scope: Option<ErrorScope>,
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

    /// Set the project storage directory.
    pub fn storage_directory(mut self, dir: std::path::PathBuf) -> Self {
        self.storage_dir = Some(dir);
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

    /// Create a Context.
    pub fn build(self) -> Result<Context, BuilderError> {
        let progress = Progress::default();
        Ok(Context {
            global: RArc::new(GlobalContext {
                log: Log::new(
                    self.logger
                        .unwrap_or_else(|| logger_ref(EmptyLogTarget).into()),
                ),
                env: Environment::new(self.storage_dir.unwrap_or(std::env::temp_dir())),
                shared_state: SharedState::new(),
                task: TaskManager::new(
                    self.threads,
                    self.aggregate_errors.unwrap_or(false),
                    progress.clone(),
                )
                .map_err(BuilderError::TaskManagerError)?,
                traits: Default::default(),
                progress,
            }),
            dynamic_scope: Default::default(),
            error_scope: self.error_scope.unwrap_or_default(),
            backtrace: Default::default(),
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
        Context::global()
            .progress
            .eval_checking_progress(value)
            .await?;
        debug_assert!(value.is_evaluated());
        if value.is_type::<crate::types::Error>() {
            let source = crate::metadata::Source::get_origin(&value);
            if let Some(error) = value.as_mut::<crate::types::Error>() {
                error.modify_diagnostics(|d| {
                    use crate::error::DiagnosticInfo;
                    if d.labels.is_empty() {
                        d.add_primary_label(source.with("while evaluating this value"));
                    }
                });
            }
            let error = value
                .clone()
                .as_type::<crate::types::Error>()
                .unwrap()
                .into_owned();
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

    /// Evaluate the given future with the context error scope set to a no-op function.
    pub async fn ignore_errors<Fut, Ret>(fut: Fut) -> Ret
    where
        Fut: std::future::Future<Output = Ret> + Send,
    {
        Self::fork(|ctx| ctx.error_scope = ErrorScope::new(|_| ()), fut).await
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
            backtrace: self.backtrace.fork(),
        }
    }

    fn join(&self, forked: Self) {
        self.global.join(forked.global);
        self.dynamic_scope.join(forked.dynamic_scope);
        self.error_scope.join(forked.error_scope);
        self.backtrace.join(forked.backtrace);
    }
}
