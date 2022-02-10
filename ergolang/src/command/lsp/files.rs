use lspower::lsp::Url;
use ropey::Rope;
use std::collections::HashMap;
use std::io::Read;
use tokio::sync::{RwLock, RwLockMappedWriteGuard, RwLockReadGuard, RwLockWriteGuard};

#[derive(Debug, Default)]
pub struct Files {
    content: RwLock<HashMap<Url, Rope>>,
}

fn read_file(url: &Url, guard: &mut RwLockWriteGuard<HashMap<Url, Rope>>) {
    let s = url
        .to_file_path()
        .ok()
        .and_then(|path| std::fs::File::open(path).and_then(Rope::from_reader).ok())
        .unwrap_or_default();
    guard.insert(url.clone(), s);
}

impl Files {
    pub async fn content(&self, url: &Url) -> RwLockReadGuard<'_, Rope> {
        let mut guard = self.content.read().await;
        if !guard.contains_key(url) {
            drop(guard);
            let mut wguard = self.content.write().await;
            read_file(url, &mut wguard);
            guard = wguard.downgrade();
        }
        RwLockReadGuard::map(guard, |m| m.get(url).unwrap())
    }

    pub async fn content_mut(&self, url: &Url) -> RwLockMappedWriteGuard<'_, Rope> {
        let mut guard = self.content.write().await;
        if !guard.contains_key(url) {
            read_file(url, &mut guard);
        }
        RwLockWriteGuard::map(guard, |m| m.get_mut(url).unwrap())
    }
}
