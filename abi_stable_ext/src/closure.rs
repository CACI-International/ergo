//! ABI-stable closures.

use crate::type_erase::{Eraseable, EraseableT, Erased, ErasedT, ErasedTrivial, SyncSendMarker};
use abi_stable::{std_types, StableAbi};

/// A function pointer.
///
/// The user _must_ ensure `F` is some function pointer type. Otherwise the properties of this
/// struct do not hold.
#[derive(Clone, StableAbi)]
#[repr(C)]
#[sabi(unsafe_unconstrained(F))]
pub struct FnPtr<F>(
    ErasedTrivial,
    #[sabi(unsafe_opaque_field)] std::marker::PhantomData<F>,
);

impl<F: Eraseable + Copy> FnPtr<F> {
    /// # Safety
    /// `F` must be a function pointer type.
    pub unsafe fn new(f: F) -> Self {
        FnPtr(
            ErasedTrivial::new(crate::type_erase::trivialize(f)),
            Default::default(),
        )
    }

    pub fn as_fn(&self) -> &F {
        unsafe { self.0.as_ref::<F>() }
    }
}

unsafe impl<F> Send for FnPtr<F> {}
unsafe impl<F> Sync for FnPtr<F> {}

pub type Args1<A> = std_types::Tuple1<A>;
pub type Args2<A, B> = std_types::Tuple2<A, B>;
pub type Args3<A, B, C> = std_types::Tuple3<A, B, C>;
pub type Args4<A, B, C, D> = std_types::Tuple4<A, B, C, D>;

#[derive(StableAbi)]
#[repr(C)]
pub struct ClosureOnceT<'a, SyncSend, Args, Ret> {
    f: extern "C" fn(ErasedT<'a, SyncSend>, Args) -> Ret,
    data: ErasedT<'a, SyncSend>,
}

pub type ClosureOnce<Args, Ret> =
    ClosureOnceT<'static, abi_stable::marker_type::SyncSend, Args, Ret>;

impl<'a, SyncSend, Args, Ret> std::fmt::Debug for ClosureOnceT<'a, SyncSend, Args, Ret> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("ClosureOnce")
            .field("f", &(&self.f as *const _))
            .field("data", &self.data)
            .finish()
    }
}

impl<'a, SyncSend: SyncSendMarker, Ret> ClosureOnceT<'a, SyncSend, (), Ret> {
    pub fn new<F: FnOnce() -> Ret + EraseableT<'a, SyncSend>>(f: F) -> Self {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn func<
            'k,
            SyncSend: SyncSendMarker,
            F: FnOnce() -> Ret + EraseableT<'k, SyncSend>,
            Ret,
        >(
            data: ErasedT<'k, SyncSend>,
            _: (),
        ) -> Ret {
            (unsafe { data.to_owned::<F>() })()
        }

        ClosureOnceT {
            f: func::<SyncSend, F, Ret>,
            data: ErasedT::new(f),
        }
    }

    pub fn call(self) -> Ret {
        (self.f)(self.data, ())
    }
}

impl<'a, SyncSend: SyncSendMarker, F, Ret> From<F> for ClosureOnceT<'a, SyncSend, (), Ret>
where
    F: FnOnce() -> Ret + EraseableT<'a, SyncSend>,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

#[derive(StableAbi)]
#[repr(C)]
pub struct Closure<Args, Ret> {
    f: extern "C" fn(&Erased, Args) -> Ret,
    data: Erased,
}

impl<Args, Ret> std::fmt::Debug for Closure<Args, Ret> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("f", &(&self.f as *const _))
            .field("data", &self.data)
            .finish()
    }
}

impl<Ret> Closure<(), Ret> {
    pub fn new<F: Fn() -> Ret + Eraseable>(f: F) -> Self {
        #[allow(improper_ctypes_definitions)]
        extern "C" fn func<F: Fn() -> Ret, Ret>(data: &Erased, _: ()) -> Ret {
            (unsafe { data.as_ref::<F>() })()
        }

        Closure {
            f: func::<F, Ret>,
            data: Erased::new(f),
        }
    }

    pub fn call(&self) -> Ret {
        (self.f)(&self.data, ())
    }
}

impl<F, Ret> From<F> for Closure<(), Ret>
where
    F: Fn() -> Ret + Eraseable,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<A, Ret> Closure<std_types::Tuple1<A>, Ret> {
    pub fn new<F: Fn(A) -> Ret + Eraseable>(f: F) -> Self {
        extern "C" fn func<F: Fn(A) -> Ret, A, Ret>(
            data: &Erased,
            args: std_types::Tuple1<A>,
        ) -> Ret {
            (unsafe { data.as_ref::<F>() })(args.0)
        }

        Closure {
            f: func::<F, A, Ret>,
            data: Erased::new(f),
        }
    }

    pub fn call(&self, a: A) -> Ret {
        (self.f)(&self.data, (a,).into())
    }
}

impl<F, A, Ret> From<F> for Closure<std_types::Tuple1<A>, Ret>
where
    F: Fn(A) -> Ret + Eraseable,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<A, B, Ret> Closure<std_types::Tuple2<A, B>, Ret> {
    pub fn new<F: Fn(A, B) -> Ret + Eraseable>(f: F) -> Self {
        extern "C" fn func<F: Fn(A, B) -> Ret, A, B, Ret>(
            data: &Erased,
            args: std_types::Tuple2<A, B>,
        ) -> Ret {
            (unsafe { data.as_ref::<F>() })(args.0, args.1)
        }

        Closure {
            f: func::<F, A, B, Ret>,
            data: Erased::new(f),
        }
    }

    pub fn call(&self, a: A, b: B) -> Ret {
        (self.f)(&self.data, (a, b).into())
    }
}

impl<F, A, B, Ret> From<F> for Closure<std_types::Tuple2<A, B>, Ret>
where
    F: Fn(A, B) -> Ret + Eraseable,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<A, B, C, Ret> Closure<std_types::Tuple3<A, B, C>, Ret> {
    pub fn new<F: Fn(A, B, C) -> Ret + Eraseable>(f: F) -> Self {
        extern "C" fn func<F: Fn(A, B, C) -> Ret, A, B, C, Ret>(
            data: &Erased,
            args: std_types::Tuple3<A, B, C>,
        ) -> Ret {
            (unsafe { data.as_ref::<F>() })(args.0, args.1, args.2)
        }

        Closure {
            f: func::<F, A, B, C, Ret>,
            data: Erased::new(f),
        }
    }

    pub fn call(&self, a: A, b: B, c: C) -> Ret {
        (self.f)(&self.data, (a, b, c).into())
    }
}

impl<F, A, B, C, Ret> From<F> for Closure<std_types::Tuple3<A, B, C>, Ret>
where
    F: Fn(A, B, C) -> Ret + Eraseable,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<A, B, C, D, Ret> Closure<std_types::Tuple4<A, B, C, D>, Ret> {
    pub fn new<F: Fn(A, B, C, D) -> Ret + Eraseable>(f: F) -> Self {
        extern "C" fn func<F: Fn(A, B, C, D) -> Ret, A, B, C, D, Ret>(
            data: &Erased,
            args: std_types::Tuple4<A, B, C, D>,
        ) -> Ret {
            (unsafe { data.as_ref::<F>() })(args.0, args.1, args.2, args.3)
        }

        Closure {
            f: func::<F, A, B, C, D, Ret>,
            data: Erased::new(f),
        }
    }

    pub fn call(&self, a: A, b: B, c: C, d: D) -> Ret {
        (self.f)(&self.data, (a, b, c, d).into())
    }
}

impl<F, A, B, C, D, Ret> From<F> for Closure<std_types::Tuple4<A, B, C, D>, Ret>
where
    F: Fn(A, B, C, D) -> Ret + Eraseable,
{
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

#[cfg(test)]
mod test {
    use super::{Closure, FnPtr};

    #[test]
    fn no_args() {
        let c = Closure::from(|| 10);
        assert_eq!(c.call(), 10);
    }

    #[test]
    fn one_arg() {
        let c = Closure::from(|a| a + 2);
        assert_eq!(c.call(40), 42);
    }

    #[test]
    fn two_args() {
        let c = Closure::from(|a, b| a * b);
        assert_eq!(c.call(40, 2), 80);
    }

    #[test]
    fn three_args() {
        let c = Closure::from(|a, b, c| a * b + c);
        assert_eq!(c.call(40, 2, 14), 94);
    }

    #[test]
    fn four_args() {
        let c = Closure::from(|a, b, c, d| if d { a * b } else { c });
        assert_eq!(c.call(40, 2, 14, true), 80);
        assert_eq!(c.call(40, 2, 14, false), 14);
    }

    #[test]
    fn fn_ptr() {
        extern "C" fn my_func(v: u8) -> u8 {
            v + 2
        }

        let ptr: FnPtr<extern "C" fn(u8) -> u8> = unsafe { FnPtr::new(my_func) };
        assert_eq!((ptr.as_fn())(40), 42);
    }
}
