//! Command/process management.

use log::{debug, trace};
use std::collections::BTreeMap;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};

/// Tracks external command usage.
#[derive(Debug, Default)]
pub struct Commands {
    cmds: Arc<Mutex<BTreeMap<OsString, Option<PathBuf>>>>,
}

/// A set of missing commands.
#[derive(Debug, Clone)]
pub struct Missing(Vec<OsString>);

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

    /// Return an account of any missing commands.
    pub fn missing(&self) -> Missing {
        Missing(
            self.cmds
                .lock()
                .unwrap()
                .iter()
                .filter_map(|(k, v)| match v {
                    None => Some(k.clone()),
                    Some(_) => None,
                })
                .collect(),
        )
    }
}

impl Missing {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for Missing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0
            .first()
            .map(|v| write!(f, "{}", v.to_string_lossy()))
            .transpose()?;
        for s in self.0.get(1..).unwrap_or(&[]) {
            write!(f, ", {}", s.to_string_lossy())?;
        }
        Ok(())
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
