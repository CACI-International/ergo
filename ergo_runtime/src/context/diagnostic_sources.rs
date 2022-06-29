//! Sources used in diagnostic messages.

use crate as ergo_runtime;
use crate::abi_stable::{
    external_types::RMutex,
    path::PathBuf,
    std_types::{RHashMap, ROption, RString, RVec},
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
    String { name: RString, content: RString },
    File { path: PathBuf, content: RString },
    BinaryFile { path: PathBuf },
}

/// A runtime store of all of the sources in use.
#[derive(StableAbi, crate::type_system::ErgoType)]
#[repr(C)]
pub struct Sources {
    source_ids: CacheMap<SourceId, ROption<Source>>,
    file_ids: RMutex<RHashMap<PathBuf, SourceId>>,
    source_line_starts: CacheMap<SourceId, RVec<usize>>,
}

impl std::fmt::Debug for Sources {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Sources")
            .field("file_ids", &*self.file_ids.lock())
            .finish()
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
    pub fn new() -> Self {
        Sources {
            source_ids: Default::default(),
            file_ids: RMutex::new(Default::default()),
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
                let content = std::fs::read_to_string(path.as_ref())?.into();
                guard.insert(path.clone(), id);
                self.source_ids.cache(id, || {
                    ROption::RSome(Source::File {
                        path: path.clone(),
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
                    ROption::RSome(Source::BinaryFile { path: path.clone() })
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
            ROption::RSome(Source::String {
                name: name.into(),
                content: content.into(),
            })
        });
        id
    }

    /// Get the name of a source.
    pub fn name(&self, id: SourceId) -> Option<SourceName> {
        if id == 0 {
            return Some(SourceName::String("<missing>"));
        }
        self.get(id).map(|source| match source {
            Source::File { path, .. } | Source::BinaryFile { path } => SourceName::Path(path),
            Source::String { name, .. } => SourceName::String(name.as_str()),
        })
    }

    /// Get the content of a source.
    pub fn content(&self, id: SourceId) -> Option<&str> {
        if id == 0 {
            return Some("");
        }
        self.get(id).and_then(|source| match source {
            Source::File { content, .. } | Source::String { content, .. } => Some(content.as_str()),
            Source::BinaryFile { .. } => Some(""),
        })
    }

    /// Get the path of the source, if any.
    pub fn path(&self, id: SourceId) -> Option<std::path::PathBuf> {
        self.get(id).and_then(|source| match source {
            Source::File { path, .. } | Source::BinaryFile { path } => Some(path.clone().into()),
            Source::String { .. } => None,
        })
    }

    fn get(&self, id: SourceId) -> Option<&Source> {
        self.source_ids.cache_default(id).as_ref().into()
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
