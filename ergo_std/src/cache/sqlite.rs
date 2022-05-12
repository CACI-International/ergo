//! A sqlite-backed cache.

use super::memory::MemCache;
use super::ErrorHandling;
use crate::sqlite::{self, Connection};
use ergo_runtime::{
    abi_stable::{
        future::BoxFuture,
        std_types::{RArc, RIoError, RResult, RSlice, RSliceMut},
        type_erase::ErasedTrivial,
        u128::U128,
    },
    error::DiagnosticInfo,
    traits, Context, Result, Value,
};
use futures::{future::FutureExt, lock::Mutex};
use std::mem::ManuallyDrop;
use std::path::Path;
use std::sync::Arc;

const CACHE_WRITE_FREQUENCY: std::time::Duration = std::time::Duration::from_secs(1);

pub struct SqliteCache {
    db: ManuallyDrop<ThreadShared<Db, ()>>,
    /// Root values that are cached.
    cached: MemCache,
    /// All stored values.
    stored: MemCache,
}

struct Db {
    connection: Mutex<Connection>,
    /// To be written to the database in a single transaction (as an optimization).
    pending_writes: Mutex<pending::Writes>,
    /// Used when dropping.
    finished: std::sync::atomic::AtomicBool,
    log: ergo_runtime::context::Log,
}

mod thread_shared {
    use std::thread::JoinHandle;

    pub struct ThreadShared<T, R> {
        handle: JoinHandle<R>,
        data: Box<T>,
    }

    impl<T: Sync + 'static, R> ThreadShared<T, R> {
        pub fn new<F>(value: T, f: F) -> Self
        where
            F: FnOnce(&T) -> R + Send + 'static,
            R: Send + 'static,
        {
            struct SafePtr<T>(std::ptr::NonNull<T>);
            unsafe impl<T> Send for SafePtr<T> {}

            impl<T> SafePtr<T> {
                pub unsafe fn as_ref(&self) -> &T {
                    self.0.as_ref()
                }
            }

            let data = Box::new(value);
            let ptr = SafePtr(data.as_ref().into());
            let handle = std::thread::spawn(move || f(unsafe { ptr.as_ref() }));
            ThreadShared { handle, data }
        }

        pub fn thread(&self) -> &std::thread::Thread {
            self.handle.thread()
        }

        pub fn join(self) -> (T, R) {
            match self.handle.join() {
                Ok(v) => (*self.data, v),
                Err(e) => std::panic::resume_unwind(e),
            }
        }
    }

    impl<T, R> std::ops::Deref for ThreadShared<T, R> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &*self.data
        }
    }

    impl<T, R> AsRef<T> for ThreadShared<T, R> {
        fn as_ref(&self) -> &T {
            self.data.as_ref()
        }
    }
}

use thread_shared::ThreadShared;

mod schema {
    macro_rules! load {
        ( $name:ident ) => {
            #[allow(non_upper_case_globals)]
            pub const $name: &'static str =
                std::include_str!(std::concat!("schema/", std::stringify!($name), ".sql"));
        };
    }
    load!(init);
    load!(read_entry);
    load!(read_value);
    load!(write_entry);
    load!(write_reference);
    load!(write_value);
    load!(delete_unused_values);
    load!(delete_unused_paths);
}

struct List<T> {
    inner: std::collections::LinkedList<Vec<T>>,
}

impl<T> Default for List<T> {
    fn default() -> Self {
        List {
            inner: Default::default(),
        }
    }
}

impl<T> List<T> {
    const SIZE_LIMIT: usize = 20;

    pub fn push(&mut self, value: T) {
        match self.inner.back_mut() {
            Some(v) if v.len() < Self::SIZE_LIMIT => {
                v.push(value);
                return;
            }
            _ => (),
        }

        // Must grow the list to add the value.
        self.inner.push_back(Vec::with_capacity(Self::SIZE_LIMIT));
        self.inner.back_mut().unwrap().push(value);
    }

    pub fn append(&mut self, mut other: Self) {
        self.inner.append(&mut other.inner);
    }

    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.inner.into_iter().flatten()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

mod pending {
    use super::List;

    pub struct Value {
        pub id: u128,
        pub evaluated_id: u128,
        pub tp: Vec<u8>,
        pub data: Vec<u8>,
    }

    pub struct ValueReference {
        pub from: u128,
        pub to: u128,
    }

    pub struct CacheEntry {
        pub key: u128,
        pub value: u128,
        pub expiration_time: Option<i64>,
    }

    #[derive(Default)]
    pub struct Writes {
        pub(super) values: List<Value>,
        pub(super) references: List<ValueReference>,
        pub(super) entries: List<CacheEntry>,
    }

    impl Writes {
        pub fn append(
            &mut self,
            Writes {
                values,
                references,
                entries,
            }: Self,
        ) {
            self.values.append(values);
            self.references.append(references);
            self.entries.append(entries);
        }

        pub fn is_empty(&self) -> bool {
            self.values.is_empty() && self.references.is_empty() && self.entries.is_empty()
        }
    }
}

struct SqliteCacheReader<'a, R> {
    cache: &'a SqliteCache,
    reader: R,
}

impl<'a, R: std::io::Read + Send + Sync + 'a> traits::GetDataInterface
    for SqliteCacheReader<'a, R>
{
    fn read(&mut self, buf: RSliceMut<'_, u8>) -> RResult<usize, RIoError> {
        self.reader.read(buf.into()).map_err(|e| e.into()).into()
    }

    fn read_value(&self, id: &U128) -> BoxFuture<'_, ergo_runtime::RResult<Value>> {
        BoxFuture::new(self.cache.read_value((*id).into()).map(|r| r.into()))
    }

    fn has_value(&self, id: &U128) -> BoxFuture<'_, ergo_runtime::RResult<bool>> {
        BoxFuture::new(self.cache.has_value((*id).into()).map(|r| r.into()))
    }

    fn may_block(&self) -> bool {
        false
    }
}

struct SqliteCacheWriter<'a, W> {
    cache: &'a SqliteCache,
    error_handling: ErrorHandling,
    pending_writes: Arc<Mutex<pending::Writes>>,
    from_id: Option<u128>,
    writer: W,
}

impl<'a, W: std::io::Write + Send + Sync + 'a> traits::PutDataInterface
    for SqliteCacheWriter<'a, W>
{
    fn write(&mut self, buf: RSlice<'_, u8>) -> RResult<usize, RIoError> {
        self.writer.write(buf.into()).map_err(|e| e.into()).into()
    }

    fn flush(&mut self) -> RResult<(), RIoError> {
        self.writer.flush().map_err(|e| e.into()).into()
    }

    fn write_value(&self, v: Value) -> BoxFuture<'_, ergo_runtime::RResult<()>> {
        BoxFuture::new(self.write_value(v).map(|r| r.into()))
    }

    fn may_block(&self) -> bool {
        false
    }
}

impl<'a> SqliteCacheWriter<'a, ()> {
    pub fn new(cache: &'a SqliteCache, error_handling: ErrorHandling) -> Self {
        SqliteCacheWriter {
            cache,
            error_handling,
            pending_writes: Default::default(),
            from_id: None,
            writer: (),
        }
    }
}

impl<'a, W> SqliteCacheWriter<'a, W> {
    fn for_child<T>(&self, id: u128, writer: T) -> SqliteCacheWriter<'a, T> {
        SqliteCacheWriter {
            cache: &self.cache,
            error_handling: self.error_handling,
            pending_writes: self.pending_writes.clone(),
            from_id: Some(id),
            writer,
        }
    }

    pub async fn write_value(&self, mut v: Value) -> Result<()> {
        let source = ergo_runtime::metadata::Source::get(&v);
        let id = v.id().await;
        if let Err(e) = Context::eval(&mut v).await {
            if self.error_handling.child_should_error() {
                return Err(e);
            }
        }
        self.cache.stored.get_basic_entry(id, async move {
            ergo_runtime::error_info! {
                labels: [
                    primary(source.with("while writing this value"))
                ],
                async {
                    let t = Context::get_trait::<traits::Stored>(&v)
                        .ok_or_else(|| format!("no stored trait for {}", traits::type_name(&v)))?;

                    let evaluated_id = v.id().await;
                    let mut tp_bytes = Vec::new();
                    let mut data_bytes = Vec::new();
                    let tp: ErasedTrivial = v.ergo_type().unwrap().clone().into();
                    tp.serialize(&mut tp_bytes)?;

                    {
                        let mut writer = self.for_child(id, &mut data_bytes);
                        let mut put_data = traits::PutData::new(&mut writer);
                        t.put(v.clone(), &mut put_data).await.into_result()?;
                    }

                    {
                        let mut pending_writes = self.pending_writes.lock().await;
                        pending_writes.values.push(pending::Value {
                            id, evaluated_id, tp: tp_bytes, data: data_bytes
                        });
                        if let Some(from) = &self.from_id {
                            pending_writes.references.push(pending::ValueReference {
                                from: *from,
                                to: id
                            });
                        }
                    }

                    Result::Ok(v)
                }
            }
        }).await?;
        Ok(())
    }
}

fn write_pending_loop(db: &Db) -> () {
    use ergo_runtime::async_executor::MiniExecutor;
    let executor = MiniExecutor::default();

    macro_rules! run {
        ( $fut:expr ) => {
            if let Err(e) = executor.block_on($fut) {
                db.log.error(e);
            }
        };
    }

    while !db.finished.load(std::sync::atomic::Ordering::Relaxed) {
        run!(db.write_pending());
        std::thread::park_timeout(CACHE_WRITE_FREQUENCY);
    }
    run!(db.write_pending());
    run!(db.cleanup());
}

impl Db {
    pub async fn write_pending(&self) -> Result<()> {
        let writes = std::mem::take(&mut *self.pending_writes.lock().await);
        if writes.is_empty() {
            return Ok(());
        }
        let conn = self.connection.lock().await;

        ergo_runtime::error_info! {{
            let transaction = sqlite::Transaction::new(&conn)?;
            for pending::Value { id, evaluated_id, tp, data } in writes.values.into_iter() {
                let mut stmt = conn.prepare(schema::write_value)?;
                stmt.bind(1, sqlite::U128(id))?;
                stmt.bind(3, sqlite::U128(evaluated_id))?;
                stmt.bind(5, tp.as_slice())?;
                stmt.bind(6, data.as_slice())?;
                while stmt.next()? != sqlite::State::Done {}
            }
            for pending::ValueReference { from, to } in writes.references.into_iter() {
                let mut stmt = conn.prepare(schema::write_reference)?;
                stmt.bind(1, sqlite::U128(from))?;
                stmt.bind(3, sqlite::U128(to))?;
                while stmt.next()? != sqlite::State::Done {}
            }
            for pending::CacheEntry { key, value, expiration_time } in writes.entries.into_iter() {
                let mut stmt = conn.prepare(schema::write_entry)?;
                stmt.bind(1, sqlite::U128(key))?;
                stmt.bind(3, sqlite::U128(value))?;
                stmt.bind(5, expiration_time)?;
                while stmt.next()? != sqlite::State::Done {}
            }
            transaction.commit()
        }}
    }

    pub async fn cleanup(&self) -> Result<()> {
        let conn = self.connection.lock().await;

        let mut paths = Vec::new();
        ergo_runtime::error_info! {{
            let transaction = sqlite::Transaction::new(&conn)?;
            loop {
                conn.execute(schema::delete_unused_values)?;
                if conn.change_count() == 0 {
                    break;
                }
            }
            let mut stmt = conn.prepare(schema::delete_unused_paths)?;
            while let sqlite::State::Row = stmt.next()? {
                let path: String = stmt.read(0)?;
                paths.push(std::path::PathBuf::from(path));
            }
            transaction.commit()
        }}?;

        paths
            .into_iter()
            .map(|p| {
                if p.is_dir() {
                    std::fs::remove_dir_all(&p)
                        .add_note(format_args!("removing dir {}", p.display()))
                } else if p.is_file() || p.is_symlink() {
                    std::fs::remove_file(&p).add_note(format_args!("removing file {}", p.display()))
                } else {
                    // TODO should this produce an error instead?
                    Ok(())
                }
                .map_err(|e| e.into())
            })
            .collect()
    }
}

impl SqliteCache {
    pub fn open<P: AsRef<Path>>(path: P) -> sqlite::Result<Self> {
        let connection = Connection::open(path)?;
        connection.execute(schema::init)?;
        let log = Context::global().log.sublog("cache"); //TODO should there be an associated name?
        Ok(SqliteCache {
            db: ManuallyDrop::new(ThreadShared::new(
                Db {
                    connection: Mutex::new(connection),
                    pending_writes: Default::default(),
                    finished: Default::default(),
                    log,
                },
                write_pending_loop,
            )),
            cached: Default::default(),
            stored: Default::default(),
        })
    }

    async fn read_value(&self, id: u128) -> Result<Value> {
        self.stored.get_basic_entry(id, async move {
            ergo_runtime::error_info! {
                notes: [
                    format!("while reading value with id {:032x}", id)
                ],
                async {
                    let result = {
                        let conn = self.db.connection.lock().await;
                        let mut stmt = conn.prepare(schema::read_value)?;
                        stmt.bind(1, sqlite::U128(id))?;
                        if let sqlite::State::Row = stmt.next()? {
                            let sqlite::U128(eval_id) = stmt.read(0)?;
                            let tp: Vec<u8> = stmt.read(2)?;
                            let data: Vec<u8> = stmt.read(3)?;
                            Some((eval_id, tp, data))
                        } else {
                            None
                        }
                    };

                    if let Some((id, tp, data)) = result {
                        use ergo_runtime::type_system::Type;
                        let tp: Type = ErasedTrivial::deserialize(&mut std::io::Cursor::new(tp))?.into();
                        if let Some(s) = Context::get_trait_for_type::<traits::Stored>(&tp) {
                            let mut cursor = std::io::Cursor::new(data);
                            let mut reader = SqliteCacheReader {
                                cache: self,
                                reader: &mut cursor
                            };
                            let mut get_data = traits::GetData::new(&mut reader);
                            let data = s.get(&mut get_data).await.into_result()?;
                            Result::Ok(unsafe { Value::new(RArc::new(tp), RArc::new(data), id) })
                        } else {
                            Err(format!(
                                "no stored trait for {}",
                                traits::type_name_for(&tp)
                            )).into_diagnostic()?
                        }
                    } else {
                        Err("expected database entry but it was missing").into_diagnostic()?
                    }
                }
            }
        }).await
    }

    async fn has_value(&self, id: u128) -> Result<bool> {
        if let Some(result) = self.stored.has_entry(id).await {
            return Ok(result);
        }
        ergo_runtime::error_info! {
            notes: [
                format!("while reading value with id {:032x}", id)
            ],
            async {
                let conn = self.db.connection.lock().await;
                let mut stmt = conn.prepare(schema::read_value)?;
                stmt.bind(1, sqlite::U128(id))?;
                Result::Ok(if let sqlite::State::Row = stmt.next()? {
                    true
                } else {
                    false
                })
            }
        }
    }

    async fn get_or_insert(
        &self,
        key: u128,
        value: Value,
        error_handling: ErrorHandling,
    ) -> Result<Value> {
        ergo_runtime::error_info! {
            async {
                let id = value.id().await;

                let stored_id = {
                    let conn = self.db.connection.lock().await;
                    let mut stmt = conn.prepare(schema::read_entry)?;
                    stmt.bind(1, sqlite::U128(key))?;
                    if let sqlite::State::Row = stmt.next()? {
                        let sqlite::U128(stored_id) = stmt.read(0)?;
                        Some(stored_id)
                    } else {
                        None
                    }
                };

                let mut stored_value = loop {
                    // Try to read serialized data if the value was stored and the stored id matches
                    if let Some(stored_id) = stored_id {
                        if stored_id == id {
                            match self.read_value(id).await {
                                Ok(v) => {
                                    self.db.log.debug(format_args!("successfully read cached value for {:032x}", id));
                                    break v;
                                }
                                Err(err) => {
                                    self.db.log.debug(format_args!(
                                        "failed to read cached value for {:032x}, (re)caching: {}",
                                        id, err
                                    ));
                                }
                            }
                        }
                        else {
                            self.db.log.debug(format_args!(
                                "stored id for key {:032x} was {:032x}, but new id is {:032x}, recaching",
                                key, stored_id, id
                            ));
                        }
                    }

                    // Reading serialized data failed, write the value.
                    //
                    // Make a copy of the value that we will immediately evaluate, because no
                    // matter what, from this point on it will be evaluated (either to serialize
                    // the result, to return early on error, or in the worst case to return a
                    // result when all else fails).
                    let mut value = value.clone();
                    // Copy the value before evaluating to retain the identity for write_value.
                    let orig_value = value.clone();
                    if let Err(e) = Context::eval(&mut value).await {
                        if error_handling.top_level_should_error() {
                            Err(e)?;
                        }
                    }
                    let writer = SqliteCacheWriter::new(self, error_handling);
                    if let Err(e) = writer.write_value(orig_value).await {
                        self.db.log.warn(format!("failed to cache value for {:032x}: {}", id, e));
                        break value;
                    }

                    self.db.log.debug(format!("wrote cache value for {:032x}", id));

                    let writes = {
                        let mut pending_writes = writer.pending_writes.lock().await;
                        pending_writes.entries.push(pending::CacheEntry {
                            key, value: id, expiration_time: None
                        });
                        std::mem::take(&mut *pending_writes)
                    };
                    self.db.pending_writes.lock().await.append(writes);

                    // Read the stored value to ensure the returned value has a consistent identity
                    // whether we have a cache hit or miss.
                    break match self.read_value(id).await {
                        Ok(val) => val,
                        Err(err) => {
                            self.db.log.warn(format!(
                                "failed to read just-written value for {:032x}: {}; falling back to original value",
                                id, err
                            ));
                            value
                        }
                    };
                };
                stored_value.copy_metadata(&value);

                Result::Ok(stored_value)
            }
        }
    }

    async fn cache_value(
        &self,
        key: u128,
        value: Value,
        error_handling: ErrorHandling,
    ) -> Result<Value> {
        self.cached
            .get_entry(key, value, |v| self.get_or_insert(key, v, error_handling))
            .await
    }
}

impl Drop for SqliteCache {
    fn drop(&mut self) {
        let db = unsafe { ManuallyDrop::take(&mut self.db) };
        db.finished
            .store(true, std::sync::atomic::Ordering::Relaxed);
        db.thread().unpark();
        db.join();
    }
}

impl super::CacheInterface for SqliteCache {
    fn cache_value(
        &self,
        key: U128,
        value: Value,
        error_handling: ErrorHandling,
    ) -> BoxFuture<'_, super::RResult<Value>> {
        BoxFuture::new(
            self.cache_value(key.into(), value, error_handling)
                .map(|r| r.into()),
        )
    }
}
