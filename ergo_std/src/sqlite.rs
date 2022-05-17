//! Sqlite interface and additional utilities.

pub use sqlite::*;

/// A Bindable/Readable u128.
///
/// *Note that this will bind/read to _two_ parameters starting at the given index!*
pub struct U128(pub u128);

impl U128 {
    fn split(self) -> [i64; 2] {
        // Safety: this is exactly what we intend to do; we don't care about the values
        // themselves, we just want to split the bits into two i64s.
        unsafe { std::mem::transmute(self.0) }
    }

    fn join(vals: [i64; 2]) -> Self {
        // Safety: this is exactly what we intend to do; we don't care about the values
        // themselves, we just want to join the bits into a u128.
        U128(unsafe { std::mem::transmute(vals) })
    }
}

impl Bindable for U128 {
    fn bind(self, stmt: &mut Statement<'_>, index: usize) -> Result<()> {
        let parts = self.split();
        stmt.bind(index, &Value::Integer(parts[0]))?;
        stmt.bind(index + 1, &Value::Integer(parts[1]))?;
        Ok(())
    }
}

impl Readable for U128 {
    fn read(stmt: &Statement<'_>, index: usize) -> Result<Self> {
        Ok(Self::join([stmt.read(index)?, stmt.read(index + 1)?]))
    }
}

/// A Bindable/Readable std::time::Duration
///
/// *Note that this will bind/read to _two_ INTEGER parameters starting at the given index!*
pub struct Duration(pub std::time::Duration);

fn promote_error<E: std::fmt::Display>(e: E) -> Error {
    Error {
        code: None,
        message: Some(e.to_string()),
    }
}

fn promote_result<T, E: std::fmt::Display>(r: std::result::Result<T, E>) -> Result<T> {
    r.map_err(promote_error)
}

impl Bindable for Duration {
    fn bind(self, stmt: &mut Statement<'_>, index: usize) -> Result<()> {
        let secs = promote_result(self.0.as_secs().try_into())?;
        let nsecs = promote_result(self.0.subsec_nanos().try_into())?;
        stmt.bind(index, &Value::Integer(secs))?;
        stmt.bind(index + 1, &Value::Integer(nsecs))?;
        Ok(())
    }
}

impl Readable for Duration {
    fn read(stmt: &Statement<'_>, index: usize) -> Result<Self> {
        let secs: i64 = stmt.read(index)?;
        let nsecs: i64 = stmt.read(index + 1)?;
        let secs = promote_result(secs.try_into())?;
        let nsecs = promote_result(nsecs.try_into())?;
        Ok(Duration(std::time::Duration::new(secs, nsecs)))
    }
}

/// A Bindable/Readable std::path::PathBuf.
pub struct Path(pub std::path::PathBuf);

impl Bindable for Path {
    fn bind(self, stmt: &mut Statement<'_>, index: usize) -> Result<()> {
        stmt.bind(
            index,
            promote_result(self.0.to_str().ok_or("cannot convert path to string"))?,
        )
    }
}

impl Readable for Path {
    fn read(stmt: &Statement<'_>, index: usize) -> Result<Self> {
        let path: String = stmt.read(index)?;
        Ok(Path(path.into()))
    }
}

/// A Bindable/Readable bool.
pub struct Bool(pub bool);

impl Bindable for Bool {
    fn bind(self, stmt: &mut Statement<'_>, index: usize) -> Result<()> {
        stmt.bind(index, &Value::Integer(self.0 as _))
    }
}

impl Readable for Bool {
    fn read(stmt: &Statement<'_>, index: usize) -> Result<Self> {
        let val: i64 = stmt.read(index)?;
        Ok(Bool(val != 0))
    }
}

/// Encapsulate a transaction.
///
/// When the type is dropped, if `commit()` hasn't been called, a rollback is performed.
pub struct Transaction<'a> {
    conn: &'a Connection,
    committed: bool,
}

impl<'a> Transaction<'a> {
    pub fn new(conn: &'a Connection) -> Result<Self> {
        conn.execute("BEGIN IMMEDIATE TRANSACTION")?;
        Ok(Transaction {
            conn,
            committed: false,
        })
    }

    pub fn commit(mut self) -> Result<()> {
        self.conn.execute("COMMIT")?;
        self.committed = true;
        Ok(())
    }
}

impl<'a> std::ops::Deref for Transaction<'a> {
    type Target = Connection;

    fn deref(&self) -> &Self::Target {
        &self.conn
    }
}

impl<'a> AsRef<Connection> for Transaction<'a> {
    fn as_ref(&self) -> &Connection {
        &self.conn
    }
}

impl<'a> Drop for Transaction<'a> {
    fn drop(&mut self) {
        if !self.committed {
            drop(self.conn.execute("ROLLBACK"));
        }
    }
}
