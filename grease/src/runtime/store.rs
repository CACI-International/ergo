//! Persistent storage.

use std::convert::TryFrom;
use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct Store {
    root_directory: PathBuf,
}

#[derive(Debug, Clone)]
pub struct Item {
    path: PathBuf,
    item: PathBuf,
}

#[derive(Debug)]
pub struct ItemContent {
    file: File,
}

/// An item name.
///
/// Item names may only contain alphanumeric ascii characters.
#[derive(Debug)]
pub struct ItemName(str);

impl ItemName {
    fn new<'a, T: AsRef<str> + ?Sized>(s: &'a T) -> Option<&'a ItemName> {
        let v = s.as_ref();
        if !v.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
            None
        } else {
            Some(unsafe { &*(v as *const str as *const ItemName) })
        }
    }
}

impl<'a> TryFrom<&'a str> for &'a ItemName {
    type Error = &'static str;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        ItemName::new(s).ok_or("invalid item name; must be ascii alphanumeric")
    }
}

impl AsRef<ItemName> for ItemName {
    fn as_ref(&self) -> &ItemName {
        &self
    }
}

impl AsRef<str> for ItemName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<std::ffi::OsStr> for ItemName {
    fn as_ref(&self) -> &std::ffi::OsStr {
        &self.0.as_ref()
    }
}

impl AsRef<Path> for ItemName {
    fn as_ref(&self) -> &Path {
        &self.0.as_ref()
    }
}

impl Store {
    pub(crate) fn new(root_directory: PathBuf) -> Self {
        Store { root_directory }
    }

    /// Get the item with the given identifier.
    pub fn item<P: AsRef<ItemName>>(&self, id: P) -> Item {
        let path = self.root_directory.clone();
        let item = PathBuf::from(id.as_ref());
        Item { path, item }
    }
}

impl Item {
    /// Get the sub-item with the given name.
    pub fn item<P: AsRef<ItemName>>(&self, name: P) -> Item {
        let mut path = self.path.clone();
        let mut sub = self.item.clone().into_os_string();
        sub.push("-");
        path.push(sub);
        let item = PathBuf::from(name.as_ref());
        Item { path, item }
    }

    /// Check whether an item exists.
    pub fn exists(&self) -> bool {
        self.path().exists()
    }

    /// Open an item for writing.
    ///
    /// Any previous content associated with the item will be erased.
    pub fn write(&self) -> io::Result<ItemContent> {
        self.open(OpenOptions::new().write(true).create(true).truncate(true))
    }

    /// Open an item for reading.
    pub fn read(&self) -> io::Result<ItemContent> {
        self.open(OpenOptions::new().read(true).create(true))
    }

    /// Open an item using the provided OpenOptions.
    pub fn open(&self, options: &OpenOptions) -> io::Result<ItemContent> {
        std::fs::create_dir_all(&self.path)?;
        Ok(ItemContent { file: options.open(self.path())? })
    }

    /// Get the path this item uses.
    pub fn path(&self) -> PathBuf {
        std::fs::create_dir_all(&self.path).unwrap();
        let mut path = self.path.clone();
        path.push(&self.item);
        path
    }
}

impl io::Read for ItemContent {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.file.read(buf)
    }
}

impl io::Write for ItemContent {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.file.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.file.flush()
    }
}

impl io::Seek for ItemContent {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.file.seek(pos)
    }
}
