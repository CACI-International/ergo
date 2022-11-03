//! Store arbitrary types, erasing their type.
//!
//! It is an unsafe operation to retrieve the typed value; you must ensure that the type is correct
//! by other means before retrieval.
//!
//! The Erased and ErasedTrivial types are ABI stable, though that doesn't mean the types they
//! store are also ABI stable.

use abi_stable::{marker_type, std_types::RArc, StableAbi};
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
        // Zero-initialize so that any padding bytes are consistent.
        let mut ret = Buffer([0; 24]);
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

    /// Get a mutable reference of type T.
    ///
    /// Unsafe because callers must ensure the Buffer holds the type T.
    pub unsafe fn as_mut<T>(&mut self) -> &mut T {
        Self::check_type::<T>();
        let ptr = &mut self.0 as *mut [u8; 24] as *mut T;
        ptr.as_mut().unwrap()
    }

    fn check_type<T>() {
        debug_assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Self>());
        debug_assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Self>());
    }
}

pub type Erased = ErasedT<'static, marker_type::SyncSend>;

/// A type which stores a value with the type erased.
#[derive(StableAbi)]
#[repr(C)]
pub struct ErasedT<'a, SyncSend> {
    data: ErasedTrivial,
    drop: extern "C" fn(&mut ErasedTrivial),
    _sync_send: SyncSend,
    _lifetime: std::marker::PhantomData<&'a ()>,
}

extern "C" fn no_drop(_v: &mut ErasedTrivial) {}

extern "C" fn drop_type<T>(v: &mut ErasedTrivial) {
    drop(unsafe { std::mem::take(v).to_owned::<T>() });
}

pub trait EraseableT<'a, SyncSend>: 'a {}

impl<'a, T: Send + Sync + ?Sized + 'a> EraseableT<'a, marker_type::SyncSend> for T {}
impl<'a, T: Send + ?Sized + 'a> EraseableT<'a, marker_type::UnsyncSend> for T {}
impl<'a, T: Sync + ?Sized + 'a> EraseableT<'a, marker_type::SyncUnsend> for T {}
impl<'a, T: ?Sized + 'a> EraseableT<'a, marker_type::UnsyncUnsend> for T {}

pub trait Eraseable: EraseableT<'static, marker_type::SyncSend> + Send + Sync {}

impl<T: EraseableT<'static, marker_type::SyncSend> + Send + Sync> Eraseable for T {}

pub trait SyncSendMarker: 'static {
    fn marker() -> Self;
}

impl SyncSendMarker for marker_type::SyncSend {
    fn marker() -> Self {
        marker_type::SyncSend
    }
}

impl SyncSendMarker for marker_type::UnsyncSend {
    fn marker() -> Self {
        Self::NEW
    }
}

impl SyncSendMarker for marker_type::SyncUnsend {
    fn marker() -> Self {
        Self::NEW
    }
}

impl SyncSendMarker for marker_type::UnsyncUnsend {
    fn marker() -> Self {
        Self::NEW
    }
}

impl<'a, SyncSend: SyncSendMarker> ErasedT<'a, SyncSend> {
    /// Create a new Erased from the given value.
    pub fn new<T: EraseableT<'a, SyncSend>>(v: T) -> Self {
        ErasedT {
            // Safe to trivialize since ErasedTrivial is only a veneer over trait safety.
            data: ErasedTrivial::new(unsafe { trivialize(v) }),
            drop: if std::mem::needs_drop::<T>() {
                drop_type::<T>
            } else {
                no_drop
            },
            _sync_send: SyncSend::marker(),
            _lifetime: std::marker::PhantomData,
        }
    }

    /// Create a new Erased from the given boxed value.
    ///
    /// When retrieving the value later, one should use `as_ref::<T>()`/`to_owned::<T>()`.
    pub fn from_boxed<T: EraseableT<'a, SyncSend>>(v: Box<T>) -> Self {
        ErasedT {
            // Safe to trivialize since ErasedTrivial is only a veneer over trait safety.
            data: ErasedTrivial::from_boxed(unsafe { v.trivialize() }),
            drop: if std::mem::needs_drop::<T>() {
                drop_type::<T>
            } else {
                no_drop
            },
            _sync_send: SyncSend::marker(),
            _lifetime: std::marker::PhantomData,
        }
    }

    /// Get a pointer to the stored value.
    pub fn as_ptr(&self) -> *const () {
        self.data.as_ptr()
    }

    /// Get a &T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_ref<T>(&self) -> &T {
        self.data.as_ref()
    }

    /// Get a &mut T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_mut<T>(&mut self) -> &mut T {
        self.data.as_mut()
    }

    /// Get a T.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn to_owned<T>(mut self) -> T {
        self.drop = no_drop;
        std::mem::take(&mut self.data).to_owned()
    }
}

impl<'a, SyncSend: SyncSendMarker> Default for ErasedT<'a, SyncSend> {
    /// The default Erased value should never be converted to a type.
    fn default() -> Self {
        ErasedT {
            data: Default::default(),
            drop: no_drop,
            _sync_send: SyncSend::marker(),
            _lifetime: std::marker::PhantomData,
        }
    }
}

impl<'a, SyncSend> Drop for ErasedT<'a, SyncSend> {
    fn drop(&mut self) {
        (self.drop)(&mut self.data)
    }
}

impl<'a, SyncSend> std::fmt::Debug for ErasedT<'a, SyncSend> {
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
/// present/desirable for all trivial types. Though it is true that all Copy types can be trivial.
///
/// # Safety
/// Trivial types _must_ have consistent padding bytes, if any. Otherwise the guarantees of
/// equality/ordering/hashing for ErasedTrivial do not hold.
pub unsafe trait Trivial: Sized {
    fn new_zeroed() -> Self {
        let mut v = std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            v.as_mut_ptr().write_bytes(0, 1);
            v.assume_init()
        }
    }
}

unsafe impl Trivial for () {}
unsafe impl Trivial for bool {}
unsafe impl Trivial for char {}
unsafe impl Trivial for u8 {}
unsafe impl Trivial for i8 {}
unsafe impl Trivial for u16 {}
unsafe impl Trivial for i16 {}
unsafe impl Trivial for u32 {}
unsafe impl Trivial for i32 {}
unsafe impl Trivial for u64 {}
unsafe impl Trivial for i64 {}
unsafe impl Trivial for u128 {}
unsafe impl Trivial for i128 {}
unsafe impl Trivial for usize {}
unsafe impl Trivial for isize {}

mod triv {
    #[repr(C)]
    pub struct Trivialized<T>(T);

    unsafe impl<T> super::Trivial for Trivialized<T> {}

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

use triv::Trivialize;
pub use triv::{trivialize, Trivialized};

struct Boxed {
    pub ptr: *mut u8,
    dealloc: unsafe extern "C" fn(*mut u8, usize, usize),
}

unsafe extern "C" fn dealloc_fn(ptr: *mut u8, size: usize, align: usize) {
    std::alloc::dealloc(ptr, Layout::from_size_align_unchecked(size, align));
}

impl Boxed {
    pub fn new(ptr: *mut u8) -> Self {
        Boxed {
            ptr,
            dealloc: dealloc_fn,
        }
    }

    pub fn from_box<T>(b: Box<T>) -> Self {
        Self::new(Box::into_raw(b) as *mut u8)
    }

    pub fn from_slice<T>(s: Box<[T]>) -> Self {
        let ret = Self::new(s.as_ptr() as *mut u8);
        std::mem::forget(s);
        ret
    }

    pub unsafe fn as_ref<T>(&self) -> &T {
        (self.ptr as *const T).as_ref().unwrap()
    }

    pub unsafe fn as_mut<T>(&mut self) -> &mut T {
        (self.ptr as *mut T).as_mut().unwrap()
    }

    pub unsafe fn to_owned<T>(self) -> T {
        let ret = (self.ptr as *mut T).read();
        self.drop(std::mem::size_of::<T>(), std::mem::align_of::<T>());
        ret
    }

    pub unsafe fn drop(self, size: usize, align: usize) {
        (self.dealloc)(self.ptr, size, align)
    }
}

/// An Erased type for Trivial types.
///
/// Unlike Erased, this type implements Clone, PartialEq, Eq, and Hash.
#[derive(Debug, Default, StableAbi)]
#[repr(C)]
pub struct ErasedTrivial {
    data: Buffer,
    size: u32,
    is_box_and_align: u8, // first bit indicates box, remaining indicate alignment
}

// ErasedTrivial will always be Send and Sync by definition of Trivial.
unsafe impl Send for ErasedTrivial {}
unsafe impl Sync for ErasedTrivial {}

impl ErasedTrivial {
    const IS_BOX_MASK_AND_MAX_ALIGN: u8 = 1 << (std::mem::size_of::<u8>() * 8 - 1);

    /// Create a new ErasedTrivial from the given value.
    pub fn new<T: Trivial>(v: T) -> Self {
        let size = std::mem::size_of_val(&v);
        let align = std::mem::align_of_val(&v);
        if size <= std::mem::size_of::<Buffer>() && align <= std::mem::align_of::<Buffer>() {
            Self::from_type(v, false, size, align)
        } else {
            // Box<T: Sized> is defined as having the same size as usize/pointer, so the size
            // assertions should be true on any platform.
            Self::from_boxed(Box::new(v))
        }
    }

    /// Create a new ErasedTrivial from the given boxed value.
    ///
    /// When retrieving the value later, one should use `as_ref::<T>()`/`to_owned::<T>()`.
    pub fn from_boxed<T: Trivial>(v: Box<T>) -> Self {
        Self::from_type(
            Boxed::from_box(v),
            true,
            std::mem::size_of::<T>(),
            std::mem::align_of::<T>(),
        )
    }

    /// Create a new ErasedTrivial from the given boxed slice.
    ///
    /// When retrieving the value later, one should use `as_slice::<T>()`/`to_boxed_slice::<T>()`.
    pub fn from_slice<T: Trivial>(v: Box<[T]>) -> Self {
        let size = std::mem::size_of_val(&*v);
        if size == 0 {
            Self::default()
        } else {
            let align = std::mem::align_of_val(&*v);
            Self::from_type(Boxed::from_slice(v), true, size, align)
        }
    }

    /// Create a new ErasedTrivial from the given boxed str.
    ///
    /// When retrieving the value later, one should use `as_str()`/`to_boxed_str()`.
    pub fn from_str(v: Box<str>) -> Self {
        Self::from_slice(<Box<[u8]>>::from(v))
    }

    /// Get a pointer to the stored value.
    pub fn as_ptr(&self) -> *const () {
        if self.is_box() {
            unsafe { self.data.as_ref::<Boxed>().as_ref::<()>() as *const () }
        } else {
            unsafe { self.data.as_ref::<()>() as *const () }
        }
    }

    /// Create an ErasedTrivial from the given type, assuming the size and alignment of the type will fit
    /// in the Buffer. Panics if these constraints are not satisfied (in debug builds).
    fn from_type<T>(v: T, boxed: bool, size: usize, align: usize) -> Self {
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

    /// Get a T reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_ref<T>(&self) -> &T {
        debug_assert!(self.size() == std::mem::size_of::<T>());
        debug_assert!(self.align() == std::mem::align_of::<T>());

        if self.is_box() {
            self.data.as_ref::<Boxed>().as_ref()
        } else {
            self.data.as_ref()
        }
    }

    /// Get a T mutable reference.
    ///
    /// Unsafe because callers must ensure the data is a T.
    pub unsafe fn as_mut<T>(&mut self) -> &mut T {
        debug_assert!(self.size() == std::mem::size_of::<T>());
        debug_assert!(self.align() == std::mem::align_of::<T>());

        if self.is_box() {
            self.data.as_mut::<Boxed>().as_mut()
        } else {
            self.data.as_mut()
        }
    }

    /// Get a T slice.
    ///
    /// Unsafe because callers must ensure the data is a [T].
    pub unsafe fn as_slice<T>(&self) -> &[T] {
        debug_assert!(self.size() % std::mem::size_of::<T>() == 0);
        debug_assert!(self.align() == std::mem::align_of::<T>());

        let ptr = if self.is_box() {
            let p: *mut u8 = self.data.as_ref::<Boxed>().ptr;
            p as *const T
        } else {
            self.data.as_ref::<T>() as *const T
        };
        std::slice::from_raw_parts(ptr, self.size() / Layout::new::<T>().pad_to_align().size())
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
        debug_assert!(self.size() == std::mem::size_of::<T>());
        debug_assert!(self.align() == std::mem::align_of::<T>());

        if self.is_box() {
            let v = self.data.as_value::<Boxed>();
            self.clear();
            v.to_owned()
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
        debug_assert!(self.size() % std::mem::size_of::<T>() == 0);
        debug_assert!(self.align() == std::mem::align_of::<T>());

        let ptr = alloc(Layout::from_size_align_unchecked(self.size(), self.align()));
        ptr.copy_from_nonoverlapping(
            if self.is_box() {
                self.data.as_value::<Boxed>().ptr
            } else {
                self.data.0.as_ptr()
            },
            self.size(),
        );
        let ptr = ptr as *mut T;

        Box::from_raw(std::slice::from_raw_parts_mut(
            ptr,
            self.size() / Layout::new::<T>().pad_to_align().size(),
        ) as *mut [T])
    }

    /// Get a boxed str.
    ///
    /// Unsafe because callers must ensure the data is a str.
    pub unsafe fn to_boxed_str(self) -> Box<str> {
        String::from_utf8_unchecked(self.to_boxed_slice::<u8>().to_vec()).into_boxed_str()
    }

    /// Serialize into a `Write`.
    ///
    /// The value can be recreated with `deserialize`.
    ///
    /// TODO: use serde?
    pub fn serialize<W: std::io::Write>(&self, w: &mut W) -> std::io::Result<()> {
        let bytes = self.as_bytes();
        let align = self.align();
        let size = bytes.len();

        debug_assert!(align <= u8::MAX as usize);
        w.write_all(&[align as u8])?;
        debug_assert!(size <= u32::MAX as usize);
        w.write_all(&(size as u32).to_be_bytes())?;
        w.write_all(bytes.as_ref())
    }

    /// Deserialize an ErasedTrivial value from a `Read`.
    ///
    /// Panics if there are not as many bytes as expected.
    ///
    /// This will always work if the value was written with `serialize`.
    pub fn deserialize<R: std::io::Read>(r: &mut R) -> std::io::Result<Self> {
        let mut align_size = [0; 5];
        r.read_exact(&mut align_size)?;
        let align = align_size[0] as usize;
        let size = u32::from_be_bytes(align_size[1..5].try_into().expect("incorrect array length"))
            as usize;

        Ok(
            if size <= std::mem::size_of::<Buffer>() && align <= std::mem::align_of::<Buffer>() {
                let mut b = Buffer::default();
                r.read_exact(&mut b.0[..size])?;
                ErasedTrivial {
                    data: b,
                    size: size as u32,
                    is_box_and_align: align as u8,
                }
            } else {
                let ptr = unsafe { alloc(Layout::from_size_align_unchecked(size, align)) };
                r.read_exact(unsafe { std::slice::from_raw_parts_mut(ptr, size) })?;
                Self::from_type(Boxed::new(ptr), true, size, align)
            },
        )
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
                let ptr = self.data.as_ref::<Boxed>().ptr;
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
                let boxed = self.data.as_value::<Boxed>();
                boxed.drop(self.size(), self.align());
            };
        }
    }
}

impl Clone for ErasedTrivial {
    fn clone(&self) -> Self {
        let data = if self.is_box() {
            unsafe {
                let from = self.data.as_ref::<Boxed>().ptr;
                let ptr = alloc(Layout::from_size_align_unchecked(self.size(), self.align()));
                ptr.copy_from_nonoverlapping(from, self.size());
                Buffer::new(Boxed::new(ptr))
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

impl<'a, SyncSend, T, Ptr: std::ops::Deref<Target = ErasedT<'a, SyncSend>>> Ref<T, Ptr> {
    /// Create a new ref from an erased value.
    ///
    /// Unsafe because callers must ensure that the Erased stores T.
    pub unsafe fn new(inner: Ptr) -> Self {
        Ref(inner, Default::default())
    }
}

impl<T, Ptr: Clone> Clone for Ref<T, Ptr> {
    fn clone(&self) -> Self {
        Ref(self.0.clone(), Default::default())
    }
}

impl<T, Ptr: std::fmt::Debug> std::fmt::Debug for Ref<T, Ptr> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Ref")
            .field("pointer", &format_args!("{:?}", self.0))
            .finish()
    }
}

impl<
        'a,
        SyncSend: SyncSendMarker,
        T: std::fmt::Display,
        Ptr: std::ops::Deref<Target = ErasedT<'a, SyncSend>>,
    > std::fmt::Display for Ref<T, Ptr>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(unsafe { (*self.0).as_ref::<T>() }, f)
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

impl<SyncSend: SyncSendMarker, T, Ptr: std::ops::Deref<Target = ErasedT<'static, SyncSend>>>
    std::ops::Deref for Ref<T, Ptr>
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { (*self.0).as_ref() }
    }
}

impl<SyncSend: SyncSendMarker, T, Ptr: std::ops::Deref<Target = ErasedT<'static, SyncSend>>>
    AsRef<T> for Ref<T, Ptr>
{
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

    impl S {
        pub fn new(a: i32, c: [usize; 4]) -> Self {
            let mut v = Self::new_zeroed();
            v.a = a;
            v.c = c;
            v
        }
    }

    unsafe impl Trivial for S {}

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
    fn erased_ref() {
        let s = S::new(42, [0, 1, 2, 3]);
        let erased = RArc::new(Erased::new(s));
        let r = unsafe { Ref::<S, _>::new(erased) };
        assert_eq!(r.as_ref(), &s);
        let cloned = r.clone();
        assert_eq!(r.as_ref(), cloned.as_ref());
        let s2 = cloned.owned();
        assert_eq!(s, s2);
        let s3 = r.owned();
        assert_eq!(s, s3);
    }

    #[test]
    fn erased_trivial() {
        let s = S::new(42, [0, 1, 2, 3]);
        let erased = ErasedTrivial::new(s);
        assert_eq!(erased.size(), std::mem::size_of::<S>());
        assert_eq!(erased.align(), std::mem::align_of::<S>());
        assert_eq!(unsafe { erased.as_ref::<S>() }, &s);
        let copied = erased.clone();
        assert_eq!(erased.align(), copied.align());
        assert_eq!(erased.size(), copied.size());
        assert_eq!(erased, copied);
        assert_eq!(unsafe { copied.as_ref::<S>() }, &s);
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
