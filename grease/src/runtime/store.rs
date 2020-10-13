//! Persistent storage.

use crate::ffi::OsString as AbiOsString;
use crate::path::PathBuf as AbiPathBuf;
use crate::value::Value;
use abi_stable::{erased_types::DynTrait, std_types::RBox, StableAbi};
use std::convert::TryFrom;
use std::ffi::OsString;
use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, StableAbi)]
#[repr(C)]
pub struct Store {
    root_directory: AbiPathBuf,
}

#[derive(Debug, Clone, StableAbi)]
#[repr(C)]
pub struct Item {
    path: AbiPathBuf,
    item: AbiOsString,
}

#[derive(StableAbi)]
#[sabi(impl_InterfaceType(Debug, Send, Sync, IoRead, IoWrite, IoSeek))]
#[repr(C)]
struct ItemContentInterface;

#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct ItemContent {
    data: DynTrait<'static, RBox<()>, ItemContentInterface>,
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
        Store {
            root_directory: root_directory.into(),
        }
    }

    /// Get the item with the given identifier.
    pub fn item<P: AsRef<ItemName>>(&self, id: P) -> Item {
        let path = self.root_directory.clone();
        let item = AbiOsString::from(OsString::from(id.as_ref()));
        Item { path, item }
    }
}

impl Item {
    /// Get the sub-item with the given name.
    pub fn item<P: AsRef<ItemName>>(&self, name: P) -> Item {
        let mut path: PathBuf = self.path.clone().into_pathbuf();
        let mut sub = self.item.clone().into_os_string();
        sub.push("-");
        path.push(sub);
        let item = OsString::from(name.as_ref());
        Item {
            path: path.into(),
            item: item.into(),
        }
    }

    /// Get the sub-item for the given value.
    pub fn value(&self, v: &Value) -> Item {
        self.value_id(v.id())
    }

    /// Get the sub-item for the given value id.
    pub fn value_id(&self, id: u128) -> Item {
        let mut path: PathBuf = self.path.clone().into_pathbuf();
        let mut sub = self.item.clone().into_os_string();
        sub.push("-v");
        path.push(sub);
        let id = format!("{:x}", id);
        path.push(&id[..2]);
        path.push(&id[2..4]);
        let item = OsString::from(&id[4..]);
        Item {
            path: path.into(),
            item: item.into(),
        }
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
        self.open(OpenOptions::new().read(true).write(true).create(true))
    }

    /// Open an existing item for reading.
    pub fn read_existing(&self) -> io::Result<ItemContent> {
        self.open(OpenOptions::new().read(true))
    }

    /// Open an item using the provided OpenOptions.
    pub fn open(&self, options: &OpenOptions) -> io::Result<ItemContent> {
        std::fs::create_dir_all(self.path.as_ref())?;
        Ok(ItemContent::from(options.open(self.path())?))
    }

    /// Get the path this item uses.
    ///
    /// # TODO
    /// Change this to a grease type representing an existing file.
    pub fn path(&self) -> PathBuf {
        let mut path: PathBuf = self.path.clone().into();
        std::fs::create_dir_all(&path).unwrap();
        path.push(self.item.as_ref());
        path
    }
}

impl From<File> for ItemContent {
    fn from(f: File) -> Self {
        ItemContent {
            data: DynTrait::from_any_value(f, ItemContentInterface),
        }
    }
}

impl io::Read for ItemContent {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.data.read(buf)
    }
}

impl io::Write for ItemContent {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.data.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.data.flush()
    }
}

impl io::Seek for ItemContent {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.data.seek(pos)
    }
}
