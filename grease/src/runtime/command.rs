//! Command/process management.

use std::collections::BTreeMap;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};

use crate::prelude::*;

/// Tracks external command usage.
#[derive(Debug, Default)]
pub struct Commands {
    cmds: Arc<Mutex<BTreeMap<OsString, Option<PathBuf>>>>,
}

#[derive(Debug, Default, Clone)]
#[non_exhaustive]
pub struct UntrackedCommands;

impl fmt::Display for Commands {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (ref k, ref v) in self.cmds.lock().unwrap().iter() {
            let ks = k.to_string_lossy();
            match v {
                None => writeln!(f, "{} -> missing", ks)?,
                Some(ref v) => writeln!(f, "{} -> {}", ks, v.to_string_lossy())?,
            }
        }

        Ok(())
    }
}

/// Resolve a program string to a full path.
fn resolve(program: &OsStr) -> Option<PathBuf> {
    let progpath: &Path = program.as_ref();
    if progpath.is_file() {
        trace!("program exists: {}", progpath.display());
        Some(program.into())
    } else {
        env::var("PATH").ok().and_then(|s| {
            trace!("searching paths: {}", s);
            env::split_paths(&s).find_map(|mut f| {
                f.push(program);
                if f.is_file() {
                    Some(f)
                } else {
                    None
                }
            })
        })
    }
}

impl Commands {
    /// Create a new instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Make a Command suitable for launching an external application.
    ///
    /// The command environment is cleared, so required environment variables should be explicitly
    /// added.
    ///
    /// The dependency on the command is recorded.
    pub fn create<S: AsRef<OsStr>>(&mut self, program: S) -> Command {
        let s = program.as_ref().to_owned();
        let mut guard = self.cmds.lock().unwrap();
        let resolved = guard.entry(s).or_insert_with(|| resolve(program.as_ref()));
        let progpath = resolved
            .as_ref()
            .map(|s| s.as_os_str())
            .unwrap_or(program.as_ref());
        debug!(
            "program '{}' resolved to '{}'",
            (program.as_ref().as_ref() as &Path).display(),
            (progpath.as_ref() as &Path).display()
        );

        let mut cmd = Command::new(progpath);
        cmd.env_clear();
        cmd
    }

    /// Return a command builder that does not track used commands.
    ///
    /// Such a builder can only be used for explicit paths.
    pub fn untracked(&mut self) -> UntrackedCommands {
        UntrackedCommands
    }
}

impl UntrackedCommands {
    /// Make a Command suitable for launching an external application.
    ///
    /// The command environment is cleared, so required environment variables should be explicitly
    /// added.
    pub fn create<S: AsRef<Path>>(&mut self, program: S) -> Command {
        let mut cmd = Command::new(program.as_ref());
        cmd.env_clear();
        cmd
    }
}
