use lspower::lsp::Url;
use std::collections::HashMap;
use std::io::Read;
use tokio::sync::{RwLock, RwLockMappedWriteGuard, RwLockReadGuard, RwLockWriteGuard};

#[derive(Debug, Default)]
pub struct Files {
    content: RwLock<HashMap<Url, String>>,
}

fn read_file(url: &Url, guard: &mut RwLockWriteGuard<HashMap<Url, String>>) {
    if let Ok(path) = url.to_file_path() {
        let mut s = String::new();
        if let Ok(mut f) = std::fs::File::open(path) {
            if f.read_to_string(&mut s).is_err() {
                s = String::new();
            }
        }
        guard.insert(url.clone(), s);
    }
}

impl Files {
    pub async fn content(&self, url: &Url) -> RwLockReadGuard<'_, str> {
        let mut guard = self.content.read().await;
        if !guard.contains_key(url) {
            drop(guard);
            let mut wguard = self.content.write().await;
            read_file(url, &mut wguard);
            guard = wguard.downgrade();
        }
        RwLockReadGuard::map(guard, |m| m.get(url).unwrap().as_str())
    }

    pub async fn content_mut(&self, url: &Url) -> RwLockMappedWriteGuard<'_, String> {
        let mut guard = self.content.write().await;
        if !guard.contains_key(url) {
            read_file(url, &mut guard);
        }
        RwLockWriteGuard::map(guard, |m| m.get_mut(url).unwrap())
    }
}
