//! Progress state, used for detecting deadlock.

use crate::abi_stable::{
    external_types::RMutex,
    std_types::{RArc, RVec},
    StableAbi,
};
use crate::metadata::Source;
use crate::Value;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

const DEADLOCK_DURATION: std::time::Duration =
    std::time::Duration::from_secs(if cfg!(debug_assertions) { 10 } else { 2 });
const DEADLOCK_BACKTRACE_LIMIT: usize = 100;

const DEADLOCK_INTERVALS: usize = (DEADLOCK_DURATION.as_millis()
    / super::task::runtime::MAINTENANCE_INTERVAL.as_millis())
    as usize;

#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct Progress {
    inner: RArc<Inner>,
}

#[derive(StableAbi)]
#[repr(C)]
struct DeadlockErrors(RMutex<RVec<crate::Error>>);

impl std::fmt::Debug for DeadlockErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("DeadlockErrors")
            .field("0", &*self.0.lock())
            .finish()
    }
}

impl Default for DeadlockErrors {
    fn default() -> Self {
        DeadlockErrors(RMutex::new(Default::default()))
    }
}

impl DeadlockErrors {
    pub fn push(&self, e: crate::Error) {
        self.0.lock().push(e);
    }

    pub fn take(&self) -> RVec<crate::Error> {
        std::mem::take(&mut *self.0.lock())
    }
}

#[derive(Debug, Default, StableAbi)]
#[repr(C)]
struct Inner {
    made_progress: AtomicBool,
    made_progress_last: AtomicBool,
    attempting_progress: AtomicUsize,
    deadlock_detected: AtomicBool,
    intervals: AtomicUsize,
    deadlock_errors: DeadlockErrors,
}

pub struct AttemptToken<'a> {
    progress: &'a Progress,
}

impl<'a> Drop for AttemptToken<'a> {
    fn drop(&mut self) {
        self.progress
            .inner
            .attempting_progress
            .fetch_sub(1, Ordering::Relaxed);
    }
}

impl Progress {
    /// Evaluate a value once, checking for progress and deadlock.
    ///
    /// For deadlock detection to work correctly, one should call `attempt_progress` and retain the
    /// token while calling this function in a loop.
    ///
    /// Returns whether the value is evaluated.
    pub(crate) async fn eval_once_checking_progress(
        &self,
        value: &mut Value,
    ) -> crate::Result<bool> {
        value.eval_once().await;
        if self.is_deadlocked() {
            // Generate a circular evaluation error if the value is unevaluated.
            if !value.is_evaluated() {
                // Attempt to find an evaluation loop in `value`.
                let mut sources = vec![Source::get(&value)];
                let target = value.referential_id();
                while sources.len() < DEADLOCK_BACKTRACE_LIMIT {
                    value.eval_once().await;
                    if value.referential_id() == target {
                        break;
                    }
                    sources.push(Source::get(&value));
                }
                use crate::error::{Diagnostic, DiagnosticInfo};
                let mut d = Diagnostic::from("circular evaluation detected");
                for (i, s) in sources.into_iter().enumerate() {
                    (&mut d).add_primary_label(s.with(i + 1));
                }

                let e: crate::Error = d.into();
                self.inner.deadlock_errors.push(e.clone());
                return Err(e);
            }

            // Generate a deadlock error if the value is an aborted Error.
            if value
                .as_ref::<crate::types::Error>()
                .map(|e| e.is_aborted())
                .unwrap_or(false)
            {
                let e = crate::error! {
                    labels: [
                        primary(Source::get(&value).with("while evaluating this value"))
                    ],
                    error: "deadlock detected"
                };
                self.inner.deadlock_errors.push(e.clone());
                super::Context::with(|ctx| {
                    for (i, src) in ctx.backtrace.iter().enumerate() {
                        let (src, m) = src.as_ref().take();
                        self.inner.deadlock_errors.push(crate::error! {
                            severity: Note,
                            labels: [primary(src.with(""))],
                            error: if m.is_empty() { format!("backtrace frame {}", i) } else { format!("backtrace frame {} ({})", i, m) }
                        });
                    }
                });

                return Err(e);
            }
        }

        Ok(if value.is_evaluated() {
            self.inner.made_progress.store(true, Ordering::Relaxed);
            true
        } else {
            false
        })
    }

    /// Get a token related to a progress attempt.
    ///
    /// When dropped, the attempt is considered completed.
    pub(crate) fn attempt_progress(&self) -> AttemptToken {
        self.inner
            .attempting_progress
            .fetch_add(1, Ordering::Relaxed);
        AttemptToken { progress: self }
    }

    /// Evaluate a value, checking for progress and deadlock.
    pub(crate) async fn eval_checking_progress(&self, value: &mut Value) -> crate::Result<()> {
        let _token = self.attempt_progress();
        while !self.eval_once_checking_progress(value).await? {}
        debug_assert!(value.is_evaluated());
        Ok(())
    }

    /// Check whether we are in a deadlock state.
    ///
    /// This assumes it is called once per runtime maintenance interval.
    ///
    /// Returns whether deadlock was indicated.
    pub(crate) fn check_for_deadlock(&self, has_blocking_tasks: bool) -> bool {
        let any_attempting = self.inner.attempting_progress.load(Ordering::Relaxed) > 0;
        let made_progress = self.inner.made_progress.swap(false, Ordering::Relaxed);
        self.inner
            .made_progress_last
            .store(made_progress, Ordering::Relaxed);
        if any_attempting && !made_progress && !has_blocking_tasks {
            if self.inner.intervals.fetch_add(1, Ordering::Relaxed) + 1 == DEADLOCK_INTERVALS {
                self.indicate_deadlock();
                return true;
            }
        } else {
            self.inner.intervals.store(0, Ordering::Relaxed);
        }
        false
    }

    /// Indicate a deadlock condition was detected.
    pub fn indicate_deadlock(&self) {
        self.inner.deadlock_detected.store(true, Ordering::Relaxed);
    }

    /// Return whether evaluation is deadlocked.
    pub fn is_deadlocked(&self) -> bool {
        self.inner.deadlock_detected.load(Ordering::Relaxed)
    }

    /// Return whether progress has been made in the most recent maintenance cycle.
    pub fn made_progress(&self) -> bool {
        self.inner.made_progress_last.load(Ordering::Relaxed)
    }

    /// Get any deadlock errors that have occurred since the last call.
    pub fn deadlock_errors(&self) -> crate::error::Diagnostics {
        let mut ret = crate::error::Diagnostics::default();
        for e in self.inner.deadlock_errors.take() {
            ret.insert(&e);
        }
        ret
    }
}
