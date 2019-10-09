//! Procedure types and creation.

use crate::prelude::*;
use crate::uuid::UniqueType;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/**
 * A trait for types which are used in the procedure type system.
 *
 * Types must have an associated type id and be serializable.
 */
pub trait DataType<'de>: UniqueType + Serialize + Deserialize<'de> {}

//TODO implement derive(DataType)

lazy_static! {
    static ref BOB_NS: Uuid = Uuid::new_v5(&*crate::uuid::TYPE_NS, b"bob");
}

/// Create a base type UUID from the given string identifier.
fn ns_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*BOB_NS, name)
}

impl UniqueType for i64 {
    fn type_id() -> Uuid {
        ns_uuid(b"i64")
    }
}

impl DataType<'_> for i64 {}

impl UniqueType for f64 {
    fn type_id() -> Uuid {
        ns_uuid(b"f64")
    }
}

impl DataType<'_> for f64 {}

impl UniqueType for String {
    fn type_id() -> Uuid {
        ns_uuid(b"string")
    }
}

impl DataType<'_> for String {}

/**
 * A trait for file types.
 *
 * This is used to mark a (typically infallible) type as representing a particular file type.
 */
pub trait FileType: UniqueType {}

lazy_static! {
    pub static ref FILE_NS: Uuid = ns_uuid(b"file");
}

pub fn file_ns_uuid(name: &[u8]) -> Uuid {
    Uuid::new_v5(&*FILE_NS, name)
}

mod filetype {
    pub enum Data {}

    impl super::UniqueType for Data {
        fn type_id() -> super::Uuid {
            super::file_ns_uuid(b"data")
        }
    }

    impl super::FileType for Data {}
}

/// A file, with the given file type.
pub struct File<'a, T: FileType>(&'a std::path::Path, std::marker::PhantomData<T>);

impl<'a, T: FileType> File<'a, T> {
    pub fn new(path: &'a std::path::Path) -> Self {
        File(path, std::marker::PhantomData)
    }
}

impl<'a, T: FileType> UniqueType for File<'a, T> {
    fn type_id() -> Uuid {
        T::type_id()
    }
}

//impl<'a,T: FileType> DataType for File<'a,T> {}

/// A directory.
pub struct Directory<'a>(&'a std::path::Path);

impl<'a> Directory<'a> {
    pub fn new(path: &'a std::path::Path) -> Self {
        Directory(path)
    }
}

impl<'a> UniqueType for Directory<'a> {
    fn type_id() -> Uuid {
        ns_uuid(b"directory")
    }
}

//impl<'a> DataType for Directory<'a> {}

pub enum TypeConstraint {
    Any,
    Type(Uuid, Vec<Box<TypeConstraint>>),
    Union(Box<TypeConstraint>, Box<TypeConstraint>),
    Difference(Box<TypeConstraint>, Box<TypeConstraint>),
}

impl TypeConstraint {
    pub fn any() -> Box<TypeConstraint> {
        Box::new(TypeConstraint::Any)
    }

    pub fn t<'de, T>(args: Vec<Box<TypeConstraint>>) -> Box<TypeConstraint>
    where
        T: DataType<'de>,
    {
        Box::new(TypeConstraint::Type(T::type_id(), args))
    }

    pub fn union(a: Box<TypeConstraint>, b: Box<TypeConstraint>) -> Box<TypeConstraint> {
        Box::new(TypeConstraint::Union(a, b))
    }

    pub fn diff(a: Box<TypeConstraint>, b: Box<TypeConstraint>) -> Box<TypeConstraint> {
        Box::new(TypeConstraint::Difference(a, b))
    }
}

/*
impl<A> DataType for (A,) {}
impl<A,B> DataType for (A,B) {}
impl<A,B,C> DataType for (A,B,C) {}
impl<A,B,C,D> DataType for (A,B,C,D) {}

impl<T> DataType for Vec<T> {}

impl<T> DataType for Option<T> {}
*/

#[derive(Debug, Default)]
pub struct ProcedureBuilder {
    inputs: Vec<Uuid>,
    outputs: Vec<Uuid>,
}

impl ProcedureBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn input<'de, T>(&mut self) -> &mut Self
    where
        T: DataType<'de>,
    {
        self.inputs.push(T::type_id());
        self
    }

    pub fn output<'de, T>(&mut self) -> &mut Self
    where
        T: DataType<'de>,
    {
        self.outputs.push(T::type_id());
        self
    }
}
