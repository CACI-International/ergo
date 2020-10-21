//! Functionality for building graphs of computations from configuration.
//!
//! This crate offers a veneer over building _plans_, where a plan is a directed acyclic graph of
//! values and dependencies. Values have runtime type information rather than compile time types.
//!
//! The main feature the crate offers is a runtime context, which contains common functionality for
//! plans to utilize. Functionality includes logging, progress reporting/recording, external command
//! execution, and scheduling of tasks on a thread pool.

pub mod bst;
pub mod closure;
pub mod error;
pub mod ffi;
pub mod future;
pub mod hash;
pub mod path;
pub mod runtime;
pub mod traits;
pub mod type_erase;
pub mod types;
pub mod u128;
pub mod uuid;
pub mod value;

pub use self::type_erase::{Eraseable, Erased, ErasedTrivial, Trivial};
pub use self::uuid::Uuid;
pub use error::{Error, Result};
pub use futures::future::{FutureExt, TryFutureExt};
pub use value::{TypedValue, Value};

/// Create a literal item name.
///
/// Item names must contain only ascii alphanumeric characters.
pub use grease_macro::item_name;

/// Make a new value.
///
/// The expected syntax is:
///
/// `make_value!(( `(` _clones_ `)` ) ( `[` _dependencies_ `]` ) _body_ `)`
///
/// Where _clones_ are either comma-separated single identifiers of bindings in scope, or
/// _ident_ `=` _expr_ bindings.
///
/// All _clones_ will be exposed as the given identifiers in _body_, and they will be clones of
/// the given expression (or the identifier if no expression provided). They will also all be
/// tracked as dependencies of the value. If there are no clones, the surrounding parentheses may
/// be omitted.
///
/// Additional dependency expressions may be provided in _dependencies_, as comma-separated
/// expressions. These are used as dependencies but not exposed in _body_. If there are no
/// additional dependencies, the surrounding brackets may be omitted.
///
/// The items in _clones_ and _dependencies_ may be preceded by a `^`, indicating that the
/// expression (by reference) implements `IntoDependencies` and those dependencies should be
/// propagated.
///
/// All captures in _body_ are moved into the block. _body_ is used to produce the result of
/// the value, and is in an async context (so `.await` is valid).
pub use grease_macro::make_value;

/// An attribute macro to convert a typical trait definition into a grease trait.
///
/// The trait definition must only contain methods, which all must be async. If self is used, it
/// must be a borrowing reference.
///
/// ```
/// # #[macro_use] extern crate grease;
/// #[grease_trait]
/// pub trait Something {
///     async fn my_something(&self) -> bool;
/// }
/// ```
pub use grease_macro::grease_trait;

/// An expression macro to creat a grease trait implementation.
///
/// The macro will produce an implemented struct. Within implementations, one may access the
/// keyword `CONTEXT`, which will be a reference to the `grease::runtime::Context`. One may also
/// use the try operator (`?`) to return a grease error.
///
/// ```
/// # #[macro_use] extern crate grease;
/// # #[grease_trait]
/// # pub trait Something {
/// #     async fn my_something(&self) -> bool;
/// # }
/// fn my_trait_impl() -> SomethingImpl {
///     grease_trait_impl!{
///         impl Something for () {
///             async fn my_something(&self) -> bool {
///                 CONTEXT.log.debug("hello");
///                 if false {
///                     Err("on no!")?
///                 }
///                 true
///             }
///         }
///     }
/// }
/// ```
pub use grease_macro::grease_trait_impl;

/// A macro which creates a `traits` grease registration function.
///
/// Any trait impl items in the block will be registered (as if made into implementations with
/// `grease_trait_impl!`), and there will be a `traits: &mut grease::runtime::Traits` in scope.
///
/// ```
/// # #[macro_use] extern crate grease;
/// use abi_stable::std_types::ROption;
/// use grease::{
///     closure::FnPtr,
///     future::BoxFuture,
///     runtime::{Context,Traits},
///     types::{GreaseType,Type}
/// };
///
/// #[grease_trait]
/// pub trait ToBool {
///     async fn to_bool(&self) -> bool;
/// }
///
/// grease_traits_fn!{
///     {
///         extern "C" fn always_true<'a>(_ctx: &'a Context, _v: &'a grease::Erased)
///             -> BoxFuture<'a, grease::error::RResult<bool>>
///         {
///             BoxFuture::new(async move { grease::error::RResult::ROk(true) })
///         }
///         fn to_bool(traits: &Traits, tp: &Type) -> ROption<ToBoolImpl> {
///             ROption::RSome(ToBoolImpl {
///                 to_bool: FnPtr::from_fn(always_true as *const _)
///             })
///         }
///         traits.add_generator_by_trait_for_trait::<ToBool>(to_bool);
///     }
///     
///     impl ToBool for () {
///         async fn to_bool(&self) -> bool {
///             false
///         }
///     }
/// }
/// ```
pub use grease_macro::grease_traits_fn;

/// Re-export of `abi_stable::StableAbi` for attributes.
pub use abi_stable::StableAbi;
