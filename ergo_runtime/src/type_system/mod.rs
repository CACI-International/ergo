//! The ergo runtime type system.
//!
//! The type system allows for types with arbitrary extra data composing the type, and type traits
//! that also allow arbitrary extra data to define them. Type traits mirror rust traits, and can be
//! registered in the runtime at any time. Specialization exists, in that trait implementations
//! that are registered later will always be matched before earlier registrations, when applicable.

#[path = "type.rs"]
mod ergo_type;

#[path = "trait.rs"]
mod ergo_trait;

/// An attribute macro to convert a typical trait definition into an ergo trait.
///
/// The trait definition must only contain methods, which may optionally be async. If self is used,
/// it must be a borrowing reference.
///
/// ```
/// # use ergo_runtime::type_system::ergo_trait;
/// #[ergo_trait]
/// pub trait Something {
///     async fn my_something(&self) -> bool;
/// }
/// ```
pub use ergo_runtime_macro::ergo_trait;

/// An expression macro to create an ergo trait implementation.
///
/// The macro will produce an implemented struct. Within implementations, one may access the
/// keyword `CONTEXT`, which will be a reference to the `ergo_runtime::Context`.
///
/// ```
/// # use ergo_runtime::type_system::{ergo_trait, ergo_trait_impl};
/// # #[ergo_trait]
/// # pub trait Something {
/// #     async fn my_something(&self) -> bool;
/// # }
/// fn my_trait_impl() -> SomethingImpl {
///     ergo_trait_impl!{
///         impl Something for bool {
///             async fn my_something(&self) -> bool {
///                 CONTEXT.log.debug("hello");
///                 *self
///             }
///         }
///     }
/// }
/// ```
pub use ergo_runtime_macro::ergo_trait_impl;

/// A macro which creates an `ergo_traits` registration function.
///
/// Any trait impl items in the block will be registered (as if made into implementations with
/// `ergo_trait_impl!`), and there will be a `traits: &mut ergo_runtime::context::Traits` in scope.
///
/// ```
/// use ergo_runtime::abi_stable::{
///     std_types::ROption,
///     closure::FnPtr,
///     future::BoxFuture,
///     type_erase::Erased,
/// };
/// use ergo_runtime::{
///     Value,
///     type_system::{ErgoType, Type, ergo_trait, ergo_traits_fn},
///     Context
/// };
///
/// #[ergo_trait]
/// pub trait ToBool {
///     async fn to_bool(&self) -> bool;
/// }
///
/// ergo_traits_fn!{
///     {
///         extern "C" fn always_true<'a>(_data: &'a Erased, _ctx: &'a Context, _val: &'a Value, _tp: &'a Type, _v: &'a Erased)
///             -> BoxFuture<'a, bool>
///         {
///             BoxFuture::new(async move { true })
///         }
///         traits.add_generator_by_trait_for_trait::<ToBool>(|_traits,_type| {
///             ROption::RSome(ToBoolImpl {
///                 to_bool: unsafe { FnPtr::new(always_true) },
///                 ergo_trait_data: Default::default()
///             })
///         });
///     }
///     
///     impl ToBool for ergo_runtime::types::Unit {
///         async fn to_bool(&self) -> bool {
///             false
///         }
///     }
/// }
/// ```
pub use ergo_runtime_macro::ergo_traits_fn;

/// Derive an ErgoType implementation based on the rust type's fully-qualified namespace.
pub use ergo_runtime_macro::ErgoType;

pub use ergo_trait::*;
pub use ergo_type::*;
