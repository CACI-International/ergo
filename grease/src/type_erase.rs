//! Store arbitrary types, erasing their type.
//!
//! It is an unsafe operation to retrieve the typed value; you must ensure that the type is correct
//! by other means before retrieval.
//!
//! The Erased and ErasedTrivial types are ABI stable, though that doesn't mean the types they
//! store are also ABI stable.

use abi_stable::{std_types::RArc, StableAbi};
use std::alloc::{alloc, Layout};
use std::mem::drop;

/// A 24-byte buffer which erases types.
///
/// This buffer does _not_ manage dropping values.
///
/// For any of the methods of this type, if the type has incompatible size or alignment, the method
/// will panic.
#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C, align(8))]
struct Buffer([u8; 24]);

impl Buffer {
    /// Create a buffer from the given value.
    pub fn new<T>(v: T) -> Self {
        Self::check_type::<T>();
        let mut ret = Buffer(unsafe { std::mem::MaybeUninit::uninit().assume_init() });
        let ptr = &mut ret.0 as *mut [u8; 24] as *mut T;
        unsafe {
            ptr.write(v);
        }
        ret
    }

    /// Convert the buffer into the given type.
    ///
    /// Unsafe because callers must not use the Buffer after this call.
    pub unsafe fn as_value<T>(&mut self) -> T {
        Self::check_type::<T>();
        let ptr = &mut self.0 as *mut [u8; 24] as *mut T;
        ptr.read()
    }

    /// Get a reference of type T.
    ///
    /// Unsafe because callers must ensure the Buffer holds the type T.
    pub unsafe fn as_ref<T>(&self) -> &T {
        Self::check_type::<T>();
        let ptr = &self.0 as *const [u8; 24] as *const T;
        ptr.as_ref().unwrap()
    }

    fn check_type<T>() {
        debug_assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Self>());
        debug_assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Self>());
    }
}

/// A type which stores a value with the type erased.
#[derive(StableAbi)]
#[repr(C)]
pub struct Erased {
    data: ErasedTrivial,
    drop: extern "C" fn(&mut ErasedTrivial),
}

// Erased will always be Send, and should be Sync if the value it stores is Sync. So we require
// all stored values to be Sync for safety.
unsafe impl Send for Erased {}
unsafe impl Sync for Erased {}

extern "C" fn no_drop(_v: &mut ErasedTrivial) {}

extern "C" fn drop_type<T>(v: &mut ErasedTrivial) {
    drop(unsafe { std::mem::take(v).to_owned::<T>() });
}

pub trait Eraseable: Send + Sync + 'static {}

impl<T: Send + Sync + ?Sized + 'static> Eraseable for T {}

impl Erased {
    /// Create a new Erased from the given value.
    pub fn new<T: Eraseable>(v: T) -> Self {
        Erased {
            // Safe to trivialize since ErasedTrivial is only a veneer over trait safety.
            data: ErasedTrivial::new(unsafe { trivialize(v) }),
            drop: if std::mem::needs_drop::<T>() {
                drop_type::<T>
            } else {
                no_drop
            },
        }
    }

    /// Create a new Erased from the given boxed value.
    ///
    /// When retrieving the value later, one should use `as_ref::<T>()`/`to_owned::<T>()`.
    pub fn from_boxed<T: Eraseable>(v: Box<T>) -> Self {
        Erased {
            // Safe to trivialize since ErasedTrivial is only a veneer over trait safety.
            data: ErasedTrivial::from_boxed(unsafe { v.trivialize() }),
            drop: if std::mem::needs_drop::<T>() {
                drop_type::<T>
            } else {
                no_drop
            },
        }
    }

    /// Get a &T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_ref<T>(&self) -> &T {
        self.data.as_ref()
    }

    /// Get a T.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn to_owned<T>(mut self) -> T {
        self.drop = no_drop;
        std::mem::take(&mut self.data).to_owned()
    }
}

impl Default for Erased {
    /// The default Erased value should never be converted to a type.
    fn default() -> Self {
        Erased {
            data: Default::default(),
            drop: no_drop,
        }
    }
}

impl Drop for Erased {
    fn drop(&mut self) {
        (self.drop)(&mut self.data)
    }
}

impl std::fmt::Debug for Erased {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Erased").field("data", &self.data).finish()
    }
}

/// Trivial type trait.
///
/// Trivial types do not need a drop, can be compared for equality and hashed, etc.
/// That is, trivial data represents the data semantically within a sequence of bytes.
///
/// Note this differs from Copy because Copy implies copy semantics, which may not necessarily be
/// present/desirable for all trivial types. Though it is true that all Copy types are Trivial (and
/// such a blanket implementation exists).
pub trait Trivial {}

impl<T: Copy> Trivial for T {}

mod triv {
    #[repr(C)]
    pub struct Trivialized<T>(T);

    impl<T> super::Trivial for Trivialized<T> {}

    pub trait Trivialize {
        type Output;
        unsafe fn trivialize(self) -> Self::Output;
    }

    impl<T> Trivialize for Box<T> {
        type Output = Box<Trivialized<T>>;

        unsafe fn trivialize(self) -> Self::Output {
            std::mem::transmute(self)
        }
    }

    impl<T> Trivialize for Box<[T]> {
        type Output = Box<[Trivialized<T>]>;

        unsafe fn trivialize(self) -> Self::Output {
            std::mem::transmute(self)
        }
    }

    /// Unsafe because users must ensure that T is valid as a Trivial value in context.
    pub unsafe fn trivialize<T>(v: T) -> Trivialized<T> {
        Trivialized(v)
    }
}

use triv::{trivialize, Trivialize};

/// An Erased type for Trivial types.
///
/// Unlike Erased, this type implements Clone, PartialEq, Eq, and Hash.
#[derive(Debug, StableAbi)]
#[repr(C)]
pub struct ErasedTrivial {
    data: Buffer,
    size: u32,
    is_box_and_align: u8, // first bit indicates box, remaining indicate alignment
}

// ErasedTrivial will always be Send and Sync by definition of Trivial, though we are requiring
// Eraseable too, which also needs Send and Sync.
unsafe impl Send for ErasedTrivial {}
unsafe impl Sync for ErasedTrivial {}

impl ErasedTrivial {
    const IS_BOX_MASK_AND_MAX_ALIGN: u8 = 1 << (std::mem::size_of::<u8>() * 8 - 1);

    /// Create a new ErasedTrivial from the given value.
    pub fn new<T: Eraseable + Trivial>(v: T) -> Self {
        if std::mem::size_of_val(&v) <= std::mem::size_of::<Buffer>()
            && std::mem::align_of_val(&v) <= std::mem::align_of::<Buffer>()
        {
            Self::from_type(
                v,
                false,
                std::mem::size_of::<T>(),
                std::mem::align_of::<T>(),
            )
        } else {
            // Box<T: Sized> is defined as having the same size as usize/pointer, so the size
            // assertions should be true on any platform.
            Self::from_boxed(Box::new(v))
        }
    }

    /// Create a new ErasedTrivial from the given boxed value.
    ///
    /// When retrieving the value later, one should use `as_ref::<T>()`/`to_owned::<T>()`.
    pub fn from_boxed<T: Eraseable + Trivial>(v: Box<T>) -> Self {
        Self::from_type(v, true, std::mem::size_of::<T>(), std::mem::align_of::<T>())
    }

    /// Create a new ErasedTrivial from the given boxed slice.
    ///
    /// When retrieving the value later, one should use `as_slice::<T>()`/`to_boxed_slice::<T>()`.
    pub fn from_slice<T: Eraseable + Trivial>(v: Box<[T]>) -> Self {
        let size = std::mem::size_of_val(&*v);
        if size == 0 {
            Self::default()
        } else {
            let align = std::mem::align_of_val(&*v);
            Self::from_type(v, true, size, align)
        }
    }

    /// Create a new ErasedTrivial from the given boxed str.
    ///
    /// When retrieving the value later, one should use `as_str()`/`to_boxed_str()`.
    pub fn from_str(v: Box<str>) -> Self {
        Self::from_slice(<Box<[u8]>>::from(v))
    }

    /// Create an ErasedTrivial from the given type, assuming the size and alignment of the type will fit
    /// in the Buffer. Panics if these constraints are not satisfied (in debug builds).
    fn from_type<T: Eraseable>(v: T, boxed: bool, size: usize, align: usize) -> Self {
        debug_assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Buffer>());
        debug_assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Buffer>());
        assert!(size <= u32::MAX as usize);
        assert!(align < Self::IS_BOX_MASK_AND_MAX_ALIGN as usize);
        ErasedTrivial {
            data: Buffer::new(v),
            size: size as u32,
            is_box_and_align: (if boxed {
                Self::IS_BOX_MASK_AND_MAX_ALIGN
            } else {
                0
            }) + (align as u8),
        }
    }

    /// Convert this ErasedTrivial into raw components.
    ///
    /// Returns a boxed slice and the required alignment of the bytes.
    pub fn into_raw(mut self) -> (Box<[u8]>, usize) {
        let ptr = if self.is_box() {
            unsafe { self.data.as_value::<*mut u8>() }
        } else {
            unsafe {
                let ptr = alloc(Layout::from_size_align_unchecked(self.size(), self.align()));
                ptr.copy_from_nonoverlapping(self.data.0.as_ptr(), self.size());
                ptr
            }
        };

        let ret = (
            unsafe { Box::from_raw(std::slice::from_raw_parts_mut(ptr, self.size()) as *mut [u8]) },
            self.align(),
        );
        self.clear();
        ret
    }

    /// Create an ErasedTrivial from raw components.
    ///
    /// # Safety
    /// This is unsafe because the pointer's alignment and the size must match that of the type it
    /// represents. This should generally only be called with arguments returned by a call to
    /// `into_raw`.
    pub unsafe fn from_raw(bytes: Box<[u8]>, align: usize) -> Self {
        let size = bytes.len();
        if size <= std::mem::size_of::<Buffer>() && align <= std::mem::align_of::<Buffer>() {
            let mut b = Buffer::default();
            b.0[..size].copy_from_slice(&bytes);
            ErasedTrivial {
                data: b,
                size: size as u32,
                is_box_and_align: align as u8,
            }
        } else {
            Self::from_type(bytes, true, size, align)
        }
    }

    /// Get a T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_ref<T>(&self) -> &T {
        if self.is_box() {
            let b: &Box<T> = self.data.as_ref();
            b.as_ref()
        } else {
            self.data.as_ref()
        }
    }

    /// Get a T slice.
    ///
    /// Unsafe because callers must ensure the data is a [T].
    pub unsafe fn as_slice<T>(&self) -> &[T] {
        let ptr = if self.is_box() {
            *self.data.as_ref::<*const T>()
        } else {
            self.data.as_ref::<T>() as *const T
        };
        std::slice::from_raw_parts(ptr, self.size() / std::mem::size_of::<T>())
    }

    /// Get a str.
    ///
    /// Unsafe because callers must ensure the data is a str.
    pub unsafe fn as_str(&self) -> &str {
        std::str::from_utf8_unchecked(self.as_slice::<u8>())
    }

    /// Get a T.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn to_owned<T>(mut self) -> T {
        if self.is_box() {
            let v = self.data.as_value::<Box<T>>();
            self.clear();
            *v
        } else {
            let v = self.data.as_value::<T>();
            self.clear();
            v
        }
    }

    /// Get a boxed T slice.
    ///
    /// Unsafe because callers must ensure the data is a [T].
    pub unsafe fn to_boxed_slice<T>(mut self) -> Box<[T]> {
        let ptr = if self.is_box() {
            self.data.as_value::<*mut T>()
        } else {
            let ptr = alloc(Layout::from_size_align_unchecked(self.size(), self.align()));
            ptr.copy_from_nonoverlapping(self.data.0.as_ptr(), self.size());
            ptr as *mut T
        };

        let ret = Box::from_raw(std::slice::from_raw_parts_mut(
            ptr,
            self.size() / std::mem::size_of::<T>(),
        ) as *mut [T]);
        self.clear();
        ret
    }

    /// Get a boxed str.
    ///
    /// Unsafe because callers must ensure the data is a str.
    pub unsafe fn to_boxed_str(self) -> Box<str> {
        String::from_utf8_unchecked(self.to_boxed_slice::<u8>().to_vec()).into_boxed_str()
    }

    /// Serialize into a vector.
    ///
    /// The value can be recreated with `deserialize`.
    ///
    /// TODO: use serde?
    pub fn serialize<W: std::io::Write>(self, w: &mut W) -> std::io::Result<()> {
        let (bytes, align) = self.into_raw();
        let size = bytes.len();

        debug_assert!(align <= u8::MAX as usize);
        w.write_all(&[align as u8])?;
        debug_assert!(size <= u32::MAX as usize);
        w.write_all(&(size as u32).to_be_bytes())?;
        w.write_all(bytes.as_ref())
    }

    /// Deserialize an ErasedTrivial value from a slice.
    ///
    /// Panics if there are not as many bytes as expected.
    ///
    /// The slice is updated to the location where deserialization ended.
    ///
    /// This will always work if the value was written with `serialize`.
    pub fn deserialize<R: std::io::Read>(r: &mut R) -> std::io::Result<Self> {
        use std::convert::TryInto;
        let mut align_size = [0; 5];
        r.read_exact(&mut align_size)?;
        let align = align_size[0] as usize;
        let size = u32::from_be_bytes(align_size[1..5].try_into().expect("incorrect array length"))
            as usize;
        let ptr = unsafe { alloc(Layout::from_size_align_unchecked(size, align)) };

        let mut boxed =
            unsafe { Box::from_raw(std::slice::from_raw_parts_mut(ptr, size) as *mut [u8]) };
        r.read_exact(boxed.as_mut())?;
        Ok(unsafe { Self::from_raw(boxed, align) })
    }

    /// Return whether the underlying buffer is a Box or not.
    fn is_box(&self) -> bool {
        self.is_box_and_align >= Self::IS_BOX_MASK_AND_MAX_ALIGN
    }

    /// Return the size of the stored type.
    fn size(&self) -> usize {
        self.size as usize
    }

    fn align(&self) -> usize {
        (self.is_box_and_align & !Self::IS_BOX_MASK_AND_MAX_ALIGN) as usize
    }

    fn clear(&mut self) {
        self.size = 0;
        self.is_box_and_align = 0;
    }

    /// Return the underlying data bytes as a slice.
    fn as_bytes(&self) -> &[u8] {
        if self.is_box() {
            unsafe {
                let ptr = *self.data.as_ref::<*const u8>();
                std::slice::from_raw_parts(ptr, self.size())
            }
        } else {
            &self.data.0[..self.size()]
        }
    }
}

impl Drop for ErasedTrivial {
    fn drop(&mut self) {
        if self.is_box() && self.size() > 0 {
            unsafe {
                let ptr = *self.data.as_ref::<*mut u8>();
                std::alloc::dealloc(
                    ptr,
                    Layout::from_size_align_unchecked(self.size(), self.align()),
                )
            };
        }
    }
}

impl Default for ErasedTrivial {
    fn default() -> Self {
        ErasedTrivial {
            data: unsafe { std::mem::MaybeUninit::uninit().assume_init() },
            size: 0,
            is_box_and_align: 0,
        }
    }
}

impl Clone for ErasedTrivial {
    fn clone(&self) -> Self {
        let data = if self.is_box() {
            unsafe {
                let from = *self.data.as_ref::<*const u8>();
                let mem = alloc(Layout::from_size_align_unchecked(self.size(), self.align()));
                mem.copy_from_nonoverlapping(from, self.size());
                Buffer::new(mem)
            }
        } else {
            self.data.clone()
        };
        ErasedTrivial { data, ..*self }
    }
}

impl PartialEq for ErasedTrivial {
    fn eq(&self, other: &Self) -> bool {
        self.align() == other.align() && self.as_bytes() == other.as_bytes()
    }
}

impl Eq for ErasedTrivial {}

impl PartialOrd for ErasedTrivial {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.align()
            .partial_cmp(&other.align())
            .or_else(|| self.as_bytes().partial_cmp(other.as_bytes()))
    }
}

impl Ord for ErasedTrivial {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.align()
            .cmp(&other.align())
            .then_with(|| self.as_bytes().cmp(other.as_bytes()))
    }
}

impl std::hash::Hash for ErasedTrivial {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        h.write(self.as_bytes());
    }
}

/// A reference to an Erased that can be dereferenced to T.
pub struct Ref<T, Ptr>(Ptr, std::marker::PhantomData<*const T>);

unsafe impl<T, Ptr: Send> Send for Ref<T, Ptr> {}
unsafe impl<T, Ptr: Sync> Sync for Ref<T, Ptr> {}

impl<T, Ptr: std::ops::Deref<Target = Erased>> Ref<T, Ptr> {
    /// Create a new ref from an erased value.
    ///
    /// Unsafe because callers must ensure that the Erased stores T.
    pub unsafe fn new(inner: Ptr) -> Self {
        Ref(inner, Default::default())
    }
}

impl<T, Ptr: std::fmt::Debug> std::fmt::Debug for Ref<T, Ptr> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Ref")
            .field("pointer", &format_args!("{:?}", self.0))
            .finish()
    }
}

impl<T: std::fmt::Display + Sync, Ptr: std::ops::Deref<Target = Erased>> std::fmt::Display
    for Ref<T, Ptr>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_ref(), f)
    }
}

impl<T: Clone + Sync> Ref<T, RArc<Erased>> {
    /// Get an owned version of the value.
    ///
    /// This may clone or simply move the value, depending on whether the value is needed
    /// elsewhere.
    pub fn owned(self) -> T {
        match RArc::try_unwrap(self.0) {
            Ok(v) => unsafe { v.to_owned() },
            Err(r) => unsafe {
                let v: &T = (*r).as_ref();
                v.clone()
            },
        }
    }
}

impl<T: Sync, Ptr: std::ops::Deref<Target = Erased>> std::ops::Deref for Ref<T, Ptr> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { (*self.0).as_ref() }
    }
}

impl<T: Sync, Ptr: std::ops::Deref<Target = Erased>> AsRef<T> for Ref<T, Ptr> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Clone, Copy, Debug, Default, PartialEq)]
    struct S {
        a: i32,
        b: (),
        c: [usize; 4],
    }

    struct Dropper(Box<dyn FnMut()>);
    unsafe impl Send for Dropper {}
    unsafe impl Sync for Dropper {}

    impl Drop for Dropper {
        fn drop(&mut self) {
            (self.0)()
        }
    }

    #[test]
    fn erased() {
        {
            let k: usize = 142;
            let erased = Erased::new(k);
            assert_eq!(*unsafe { erased.as_ref::<usize>() }, 142);
        }

        {
            let s = "Hello, world, how are you!?";
            let st = String::from(s);
            let erased = Erased::new(st);
            assert_eq!(unsafe { erased.as_ref::<String>() }, &s);
            assert_eq!(unsafe { erased.to_owned::<String>() }, s);
        }

        {
            let mut dropped = false;
            let dropped_ref: &'static mut bool = unsafe { std::mem::transmute(&mut dropped) };
            let erased = Erased::new(Dropper(Box::new(move || *dropped_ref = true)));
            drop(erased);
            assert!(dropped);
        }
    }

    #[test]
    fn erased_trivial() {
        let s = S {
            a: 42,
            b: (),
            c: [0, 1, 2, 3],
        };
        let erased = ErasedTrivial::new(s);
        let copied = erased.clone();
        assert_eq!(unsafe { erased.as_ref::<S>() }, &s);
        assert_eq!(unsafe { copied.as_ref::<S>() }, &s);
        assert_eq!(erased, copied);
    }

    #[test]
    fn erased_trivial_slice() {
        let v: Vec<u8> = vec![1, 3, 5, 7];
        let o: Vec<u8> = vec![1, 3, 5, 9];
        let erased = ErasedTrivial::from_slice(v.into_boxed_slice());
        let copied = erased.clone();
        assert_eq!(unsafe { erased.as_slice::<u8>() }, &[1, 3, 5, 7]);
        assert_eq!(erased, copied);
        assert_eq!(&*unsafe { erased.to_boxed_slice::<u8>() }, &[1, 3, 5, 7]);
        assert_ne!(copied, ErasedTrivial::from_slice(o.into_boxed_slice()));
    }

    #[test]
    fn erased_trivial_serialize() {
        let a: Vec<u8> = vec![1, 3, 5, 7];
        let b: Vec<u8> = vec![2, 4, 6, 8];
        let mut buf: Vec<u8> = Vec::new();
        let erased_a = ErasedTrivial::from_slice(a.clone().into_boxed_slice());
        let erased_b = ErasedTrivial::from_slice(b.clone().into_boxed_slice());
        erased_a.serialize(&mut buf).unwrap();
        erased_b.serialize(&mut buf).unwrap();

        let buf_serialized = ErasedTrivial::from_slice(buf.into_boxed_slice());
        let mut buf2 = Vec::new();
        buf_serialized.serialize(&mut buf2).unwrap();

        let recovered_buf = ErasedTrivial::deserialize(&mut &buf2[..]).unwrap();
        let mut bytes = unsafe { recovered_buf.as_slice::<u8>() };

        let recovered_a = ErasedTrivial::deserialize(&mut bytes).unwrap();
        let recovered_b = ErasedTrivial::deserialize(&mut bytes).unwrap();
        assert_eq!(unsafe { recovered_a.as_slice::<u8>() }, a.as_slice());
        assert_eq!(unsafe { recovered_b.as_slice::<u8>() }, b.as_slice());
    }
}
