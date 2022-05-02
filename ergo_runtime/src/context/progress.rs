//! Progress state, used for detecting deadlock.

use crate::abi_stable::{std_types::RArc, StableAbi};
use crate::metadata::Source;
use crate::Value;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

const DEADLOCK_DURATION: std::time::Duration = std::time::Duration::from_secs(1);
const DEADLOCK_BACKTRACE_LIMIT: usize = 100;

const DEADLOCK_INTERVALS: usize = (DEADLOCK_DURATION.as_millis()
    / super::task::runtime::MAINTENANCE_INTERVAL.as_millis())
    as usize;

#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct Progress {
    inner: RArc<Inner>,
}

#[derive(Debug, Default, StableAbi)]
#[repr(C)]
struct Inner {
    made_progress: AtomicBool,
    attempting_progress: AtomicUsize,
    deadlock_detected: AtomicBool,
    intervals: AtomicUsize,
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
    pub async fn eval_once_checking_progress(&self, value: &mut Value) -> crate::Result<bool> {
        value.eval_once().await;
        if value.is_evaluated() {
            self.inner.made_progress.store(true, Ordering::Relaxed);
            Ok(true)
        } else if self.is_deadlocked() {
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
            Err(d.into())
        } else {
            Ok(false)
        }
    }

    /// Get a token related to a progress attempt.
    ///
    /// When dropped, the attempt is considered completed.
    pub fn attempt_progress(&self) -> AttemptToken {
        self.inner
            .attempting_progress
            .fetch_add(1, Ordering::Relaxed);
        AttemptToken { progress: self }
    }

    /// Evaluate a value, checking for progress and deadlock.
    pub async fn eval_checking_progress(&self, value: &mut Value) -> crate::Result<()> {
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
    pub fn check_for_deadlock(&self, has_blocking_tasks: bool) -> bool {
        let any_attempting = self.inner.attempting_progress.load(Ordering::Relaxed) > 0;
        let made_progress = self.inner.made_progress.swap(false, Ordering::Relaxed);
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
}
