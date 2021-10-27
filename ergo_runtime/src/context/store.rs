//! Runtime persistent storage.

use crate::abi_stable::{
    erased_types::DynTrait,
    ffi::OsString as AbiOsString,
    path::PathBuf as AbiPathBuf,
    std_types::{RArc, RBox},
    type_erase::Erased,
    StableAbi,
};
use crate::Value;
use cachemap::CacheMap;
use std::convert::TryFrom;
use std::ffi::OsString;
use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};

mod rwlock {
    use crate::abi_stable::{
        future::BoxFuture, sabi_trait, sabi_trait::prelude::*, std_types::RBox, type_erase::Erased,
        StableAbi,
    };

    #[sabi_trait]
    trait RwLockInterface: Send + Sync + Clone {
        fn read<'a>(&'a self) -> BoxFuture<'a, Erased>;
        fn write<'a>(&'a self) -> BoxFuture<'a, Erased>;
    }

    #[derive(StableAbi, Clone)]
    #[repr(C)]
    pub struct RwLock(RwLockInterface_TO<'static, RBox<()>>);

    impl RwLockInterface for std::sync::Arc<tokio::sync::RwLock<()>> {
        fn read<'a>(&'a self) -> BoxFuture<'a, Erased> {
            BoxFuture::new(async {
                let guard = tokio::sync::RwLock::read_owned(self.clone()).await;
                Erased::new(guard)
            })
        }

        fn write<'a>(&'a self) -> BoxFuture<'a, Erased> {
            BoxFuture::new(async {
                let guard = tokio::sync::RwLock::write_owned(self.clone()).await;
                Erased::new(guard)
            })
        }
    }

    impl RwLock {
        pub fn new() -> Self {
            RwLock(RwLockInterface_TO::from_value(
                std::sync::Arc::new(tokio::sync::RwLock::new(())),
                TU_Opaque,
            ))
        }

        pub async fn read(&self) -> Erased {
            self.0.read().await
        }

        pub async fn write(&self) -> Erased {
            self.0.write().await
        }
    }

    impl Default for RwLock {
        fn default() -> Self {
            RwLock::new()
        }
    }
}

#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Store {
    root_directory: AbiPathBuf,
    paths: RArc<CacheMap<AbiPathBuf, rwlock::RwLock>>,
}

impl std::fmt::Debug for Store {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Store")
            .field("root_directory", &self.root_directory)
            .finish_non_exhaustive()
    }
}

#[derive(Clone, StableAbi)]
#[repr(C)]
pub struct Item {
    path: AbiPathBuf,
    item: AbiOsString,
    paths: RArc<CacheMap<AbiPathBuf, rwlock::RwLock>>,
}

impl std::fmt::Debug for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Item")
            .field("path", &self.path)
            .field("item", &self.item)
            .finish_non_exhaustive()
    }
}

#[derive(StableAbi)]
#[sabi(impl_InterfaceType(Debug, Send, Sync, IoRead, IoWrite, IoSeek))]
#[repr(C)]
struct ItemContentInterface;

#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct ItemContent {
    data: DynTrait<'static, RBox<()>, ItemContentInterface>,
    guard: Erased,
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
            paths: RArc::new(Default::default()),
        }
    }

    /// Get the item with the given identifier.
    pub fn item<P: AsRef<ItemName>>(&self, id: P) -> Item {
        let path = self.root_directory.clone();
        let item = AbiOsString::from(OsString::from(id.as_ref()));
        Item {
            path,
            item,
            paths: self.paths.clone(),
        }
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
            paths: self.paths.clone(),
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
            paths: self.paths.clone(),
        }
    }

    /// Check whether an item exists.
    pub fn exists(&self) -> bool {
        self.path().exists()
    }

    /// Open an item for writing.
    ///
    /// Any previous content associated with the item will be erased.
    pub async fn write(&self) -> io::Result<ItemContent> {
        self.open(
            OpenOptions::new().write(true).create(true).truncate(true),
            true,
        )
        .await
    }

    /// Open an item for writing.
    ///
    /// No guard will be acquired, so callers must ensure no other open calls will occur
    /// concurrently.
    pub unsafe fn write_unguarded(&self) -> io::Result<ItemContent> {
        self.open_unguarded(OpenOptions::new().write(true).create(true).truncate(true))
    }

    /// Open an item for reading.
    pub async fn read(&self) -> io::Result<ItemContent> {
        self.open(
            OpenOptions::new().read(true).write(true).create(true),
            false,
        )
        .await
    }

    /// Open an item for reading.
    ///
    /// No guard will be acquired, so callers must ensure no other open calls will occur
    /// concurrently.
    pub unsafe fn read_unguarded(&self) -> io::Result<ItemContent> {
        self.open_unguarded(OpenOptions::new().read(true).write(true).create(true))
    }

    /// Open an existing item for reading.
    pub async fn read_existing(&self) -> io::Result<ItemContent> {
        self.open(OpenOptions::new().read(true), false).await
    }

    /// Open an existing item for reading.
    ///
    /// No guard will be acquired, so callers must ensure no other open calls will occur
    /// concurrently.
    pub unsafe fn read_existing_unguarded(&self) -> io::Result<ItemContent> {
        self.open_unguarded(OpenOptions::new().read(true))
    }

    /// Open an item using the provided OpenOptions.
    ///
    /// `write` specifies whether exclusive write access should be maintained.
    pub async fn open(&self, options: &OpenOptions, write: bool) -> io::Result<ItemContent> {
        let lock = self.paths.cache_default(self.path().into());
        let guard = if write {
            lock.write().await
        } else {
            lock.read().await
        };
        std::fs::create_dir_all(self.path.as_ref())?;
        Ok(ItemContent::from_file(options.open(self.path())?, guard))
    }

    /// Open an item using the provided OpenOptions.
    ///
    /// No guard will be acquired, so callers must ensure no other open calls will occur
    /// concurrently.
    pub unsafe fn open_unguarded(&self, options: &OpenOptions) -> io::Result<ItemContent> {
        std::fs::create_dir_all(self.path.as_ref())?;
        Ok(ItemContent::from_file(
            options.open(self.path())?,
            Erased::new(()),
        ))
    }

    /// Get the path this item uses.
    ///
    /// # TODO
    /// Change this to an ergo type representing an existing file (as opposed to an arbitrary path
    /// which may not exist)?
    pub fn path(&self) -> PathBuf {
        let mut path: PathBuf = self.path.clone().into();
        std::fs::create_dir_all(&path).unwrap();
        path.push(self.item.as_ref());
        path
    }
}

impl ItemContent {
    fn from_file(f: File, guard: Erased) -> Self {
        ItemContent {
            data: DynTrait::from_any_value(f, ItemContentInterface),
            guard,
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
