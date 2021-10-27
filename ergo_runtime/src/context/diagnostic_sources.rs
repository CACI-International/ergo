//! Sources used in diagnostic messages.

use super::Item;
use crate as ergo_runtime;
use crate::abi_stable::{
    external_types::RMutex,
    path::PathBuf,
    std_types::{RDuration, RHashMap, ROption, RString, RVec},
    StableAbi,
};
use cachemap::CacheMap;
use codespan_reporting::files as csr_files;
use std::hash::Hash;

/// An identifier for an individual diagnostic source.
pub type SourceId = u64;

#[derive(StableAbi)]
#[repr(C)]
enum Source {
    None,
    String {
        name: RString,
        content: RString,
    },
    File {
        path: PathBuf,
        mod_time: RDuration, // since unix epoch
        content: ROption<RString>,
    },
    BinaryFile {
        path: PathBuf,
    },
}

impl Default for Source {
    fn default() -> Self {
        Source::None
    }
}

/// A runtime store of all of the sources in use.
#[derive(StableAbi, crate::type_system::ErgoType)]
#[repr(C)]
pub struct Sources {
    item: Item,
    source_ids: CacheMap<SourceId, RMutex<Source>>,
    file_ids: RMutex<RHashMap<PathBuf, SourceId>>,
    source_line_starts: CacheMap<SourceId, RVec<usize>>,
}

// If duration is missing, it represents a binary file (we don't care about the content).
type StoredData = Vec<(std::path::PathBuf, SourceId, Option<std::time::Duration>)>;

impl std::ops::Drop for Sources {
    fn drop(&mut self) {
        let content = match unsafe { self.item.write_unguarded() } {
            Err(e) => {
                log::error!("could not open diagnostic source info for writing: {}", e);
                return;
            }
            Ok(v) => v,
        };
        let entries: StoredData = std::mem::take(&mut self.source_ids)
            .into_iter()
            .filter_map(|(k, v)| match v.into_inner() {
                Source::File { path, mod_time, .. } => {
                    Some((path.into(), k, Some(mod_time.into())))
                }
                Source::BinaryFile { path } => Some((path.into(), k, None)),
                _ => None,
            })
            .collect();
        if let Err(e) = bincode::serialize_into(content, &entries) {
            log::error!("error while serializing diagnostic source info: {}", e);
        }
    }
}

pub enum SourceName<'a> {
    String(&'a str),
    Path(&'a PathBuf),
}

impl<'a, 'b: 'a> From<&'b str> for SourceName<'a> {
    fn from(v: &'b str) -> Self {
        SourceName::String(v)
    }
}

impl<'a> std::fmt::Display for SourceName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SourceName::String(s) => s.fmt(f),
            SourceName::Path(p) => p.display().fmt(f),
        }
    }
}

fn file_id<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<SourceId> {
    let file = std::fs::File::open(path.as_ref())?;
    let hash = crate::hash::hash_read(file)?;
    let mut hfn = crate::hash::HashFn::default();
    path.as_ref().hash(&mut hfn);
    hash.hash(&mut hfn);
    Ok(hfn.finish())
}

fn binary_file_id<P: AsRef<std::path::Path>>(path: P) -> SourceId {
    let mut hfn = crate::hash::HashFn::default();
    path.as_ref().hash(&mut hfn);
    hfn.finish()
}

impl Sources {
    pub fn new(item: super::Item) -> Self {
        let stored: StoredData = if item.exists() {
            let result = unsafe { item.read_unguarded() }
                .map_err(|e| e.to_string())
                .and_then(|content| bincode::deserialize_from(content).map_err(|e| e.to_string()));
            match result {
                Ok(v) => v,
                Err(e) => {
                    log::warn!("failed to read diagnostic source info ({}), discarding", e);
                    Default::default()
                }
            }
        } else {
            Default::default()
        };
        let mut source_ids = Vec::new();
        let mut file_ids = Vec::new();
        for (path, id, mod_time) in stored {
            file_ids.push((PathBuf::from(path.clone()), id));
            source_ids.push((
                id,
                RMutex::new(match mod_time {
                    Some(mod_time) => Source::File {
                        path: path.into(),
                        mod_time: mod_time.into(),
                        content: ROption::RNone,
                    },
                    None => Source::BinaryFile { path: path.into() },
                }),
            ));
        }
        Sources {
            item,
            source_ids: source_ids.into_iter().collect(),
            file_ids: RMutex::new(file_ids.into_iter().collect()),
            source_line_starts: Default::default(),
        }
    }

    /// Add a file to the set of sources.
    pub fn add_file(&self, path: std::path::PathBuf) -> std::io::Result<SourceId> {
        let path = PathBuf::from(path);
        {
            let mut guard = self.file_ids.lock();
            if let Some(&id) = guard.get(&path) {
                drop(guard);
                if self.get(id).is_some() {
                    return Ok(id);
                }
            } else {
                let id = file_id(path.as_ref())?;
                let mod_time = std::fs::metadata(path.as_ref()).and_then(|meta| meta.modified())?;
                let duration = mod_time
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .unwrap_or_default();
                let content = ROption::RSome(std::fs::read_to_string(path.as_ref())?.into());
                guard.insert(path.clone(), id);
                self.source_ids.cache(id, || {
                    RMutex::new(Source::File {
                        path: path.clone(),
                        mod_time: duration.into(),
                        content,
                    })
                });
            }
        }
        Ok(self
            .file_ids
            .lock()
            .get(&path)
            .copied()
            .expect("file id must have been set"))
    }

    /// Add a binary file to the set of sources.
    pub fn add_binary_file(&self, path: std::path::PathBuf) -> SourceId {
        let path = PathBuf::from(path);
        {
            let mut guard = self.file_ids.lock();
            if let Some(&id) = guard.get(&path) {
                drop(guard);
                if self.get(id).is_some() {
                    return id;
                }
            } else {
                let id = binary_file_id(path.as_ref());
                guard.insert(path.clone(), id);
                self.source_ids.cache(id, || {
                    RMutex::new(Source::BinaryFile { path: path.clone() })
                });
            }
        }
        self.file_ids
            .lock()
            .get(&path)
            .copied()
            .expect("file id must have been set")
    }

    /// Add a string to the set of sources.
    pub fn add_string(&self, name: String, content: String) -> SourceId {
        let mut hfn = crate::hash::HashFn::default();
        name.hash(&mut hfn);
        content.hash(&mut hfn);
        let id = hfn.finish();
        self.source_ids.cache(id, || {
            RMutex::new(Source::String {
                name: name.into(),
                content: content.into(),
            })
        });
        id
    }

    /// Get the name of a source.
    pub fn name(&self, id: SourceId) -> Option<SourceName> {
        self.get(id).map(|source| match source {
            Source::File { path, .. } | Source::BinaryFile { path } => SourceName::Path(path),
            Source::String { name, .. } => SourceName::String(name.as_str()),
            _ => panic!("invalid source state"),
        })
    }

    /// Get the content of a source.
    pub fn content(&self, id: SourceId) -> Option<&str> {
        self.get(id).and_then(|source| match source {
            Source::File {
                content: ROption::RSome(content),
                ..
            }
            | Source::String { content, .. } => Some(content.as_str()),
            Source::BinaryFile { .. } => None,
            _ => panic!("invalid source state"),
        })
    }

    fn get(&self, id: SourceId) -> Option<&Source> {
        unsafe fn return_ref<'a, T: std::ops::Deref<Target = Source> + 'a>(
            guard: T,
        ) -> Option<&'a Source> {
            Some(std::ptr::NonNull::from(&*guard).as_ref())
        }

        let m = self
            .source_ids
            .cache(id, || RMutex::new(Default::default()));
        let mut guard = m.lock();
        match &mut *guard {
            Source::None => None,
            Source::File {
                content,
                path,
                mod_time,
            } if content.is_none() => {
                // Check whether the file content is up-to-date prior to loading
                // This path must result in either changing the entry to Source::None or
                // populating content.
                match std::fs::metadata(path.as_ref()).and_then(|meta| meta.modified()) {
                    Ok(fs_mod_time) => {
                        let fs_mod_time: RDuration = fs_mod_time
                            .duration_since(std::time::SystemTime::UNIX_EPOCH)
                            .unwrap_or_default()
                            .into();
                        if mod_time != &fs_mod_time {
                            // Recalculate id
                            match file_id(path.as_ref()) {
                                Ok(new_id) => {
                                    if new_id != id {
                                        // Id mismatch, remove old entry and insert a new one
                                        let path =
                                            match std::mem::replace(&mut *guard, Source::None) {
                                                Source::File { path, .. } => path,
                                                _ => panic!("invalid source content"),
                                            };
                                        drop(guard);
                                        self.file_ids.lock().insert(path.clone(), new_id);
                                        self.source_ids.cache(new_id, || {
                                            RMutex::new(
                                                match std::fs::read_to_string(path.as_ref()) {
                                                    Ok(content) => Source::File {
                                                        content: ROption::RSome(content.into()),
                                                        path,
                                                        mod_time: fs_mod_time,
                                                    },
                                                    Err(e) => {
                                                        log::error!(
                                                            "error reading source '{}': {}",
                                                            path.display(),
                                                            e
                                                        );
                                                        Source::None
                                                    }
                                                },
                                            )
                                        });
                                        return None;
                                    }
                                }
                                Err(e) => {
                                    log::error!("error reading source '{}': {}", path.display(), e);
                                    *guard = Source::None;
                                    return None;
                                }
                            }
                        }

                        // Populate content and mod time
                        match std::fs::read_to_string(path.as_ref()) {
                            Ok(c) => {
                                *mod_time = fs_mod_time;
                                *content = ROption::RSome(c.into());
                                // Safety: guard is guaranteed to be a Source::File with content
                                // populated, so future get() calls will not alter it.
                                unsafe { return_ref(guard) }
                            }
                            Err(e) => {
                                log::error!("error reading source '{}': {}", path.display(), e);
                                *guard = Source::None;
                                None
                            }
                        }
                    }
                    Err(e) => {
                        log::error!(
                            "error getting metadata for source '{}': {}",
                            path.display(),
                            e
                        );
                        *guard = Source::None;
                        None
                    }
                }
            }
            _ => {
                // Safety: guard is guaranteed to either be a Source::File with content populated
                // or a Source::String, so future get() calls will not alter it.
                unsafe { return_ref(guard) }
            }
        }
    }
}

impl<'a> csr_files::Files<'a> for Sources {
    type FileId = SourceId;
    type Name = SourceName<'a>;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, csr_files::Error> {
        Sources::name(self, id).ok_or(csr_files::Error::FileMissing)
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, csr_files::Error> {
        self.content(id).ok_or(csr_files::Error::FileMissing)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, csr_files::Error> {
        let src = self.source(id)?;
        let line_starts = self
            .source_line_starts
            .cache(id, || csr_files::line_starts(src).collect());
        Ok(line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, csr_files::Error> {
        let src = self.source(id)?;
        let line_starts = self
            .source_line_starts
            .cache(id, || csr_files::line_starts(src).collect());
        match line_starts.get(line_index).copied() {
            None => Err(csr_files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len(),
            }),
            Some(start) => {
                let end = match line_starts.get(line_index + 1).copied() {
                    None => src.len(),
                    Some(s) => s,
                };
                Ok(start..end)
            }
        }
    }
}
