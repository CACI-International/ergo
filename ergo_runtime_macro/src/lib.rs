//! Proc-macros for the `ergo_runtime` crate.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

mod derive_ergo_type;
mod ergo_fn;
mod ergo_trait;
mod lazy_block;
mod lazy_value;
mod match_value;
mod unbound_value;

/// Derive the ErgoType trait.
#[proc_macro_derive(ErgoType)]
pub fn derive_ergo_type(input: TokenStream) -> TokenStream {
    derive_ergo_type::derive_ergo_type_impl(input)
}

/// Define an ergo trait.
#[proc_macro_attribute]
pub fn ergo_trait(_args: TokenStream, item: TokenStream) -> TokenStream {
    ergo_trait::ergo_trait(item)
}

/// Implement an ergo trait.
#[proc_macro]
pub fn ergo_trait_impl(item: TokenStream) -> TokenStream {
    ergo_trait::ergo_trait_impl(item)
}

/// Implement multiple ergo traits and add them to the traits context.
#[proc_macro]
pub fn ergo_traits_fn(ts: TokenStream) -> TokenStream {
    ergo_trait::ergo_traits_fn(ts)
}

/// A `match` expression for Values.
#[proc_macro]
pub fn match_value(ts: TokenStream) -> TokenStream {
    match_value::match_value(ts)
}

/// Create a lazy Value.
///
/// The body of the macro contains the body of the (async) function to be evaluated when evaluating
/// the Value. The body may contain the following inner attributes:
/// * `#![contains(a,b,c)]` - a list of identifiers of values contained within this value (and
/// captured by the function).
/// * `#![depends(a,b,c)]` - arguments to pass to the `depends!` macro to create an identity from dependencies.
/// * `#![id(id)]` - the exact id to use for the value (overriding the above `depends` attribute).
/// * `#![eval_for_id]` - mark the identity as eval_for_id.
/// * `#![exclude_contains_from_id]` - if specified, the value's identity will only be derived
/// from `depends`/`id`, rather than being derived from that and `contains`.
///
/// In addition, any inner documentation will be applied to the resulting value.
///
/// The body return type is ergo_runtime::Result<Value> (so you can use `?`), however the
/// function body block should evaluate to a Value.
#[proc_macro]
pub fn lazy_value(ts: TokenStream) -> TokenStream {
    lazy_value::lazy_value(ts)
}

/// Create an Unbound Value.
///
/// This macro works exactly like `lazy_value` does, with the additional feature that you may refer
/// to `ARG` in the body (which will be the argument being bound).
#[proc_macro]
pub fn unbound_value(ts: TokenStream) -> TokenStream {
    unbound_value::unbound_value(ts)
}

/// Create an ergo Unbound for Args from a rust function declaration.
///
/// Evaluates to a function with no arguments which, when called, returns the Value.
///
/// See `ergo_fn_value` for a detailed description of the macro.
#[proc_macro_attribute]
pub fn ergo_fn(_args: TokenStream, item: TokenStream) -> TokenStream {
    ergo_fn::ergo_fn(item)
}

/// Create an ergo Unbound for Args from a rust function declaration.
///
/// Evaluates to the Value.
///
/// The function declaration must be `async`. Visibility is preserved. The function may have the
/// following special attributes:
/// * `#[eval_for_id]` - indicate that the function should have the `eval_for_id` bit set.
/// * `#[depends(a,b,c)]` - arguments to pass to the `depends!` macro to create the function
/// identity. The identity will always include a generate identifier from the function name and
/// module scope.
///
/// Any other attributes will be preserved, and doc comments will be used as documentation for the
/// produced Value.
///
/// Function arguments may be:
/// * `name: Type` where `Type` is a type implementing `ErgoType`, in which
/// case `name` will be bound to a `TypedValue<Type>`,
/// * `name: _` in which case `name` will be bound to a `Value`,
/// * `(name): ...` in which case `name` is a keyed argument (and any underscores in `name` are
/// replaced with dashes for the key)
/// * `...: [...]` in which case the argument is optional.
///
/// The function return type is ergo_runtime::Result<Value> (so you can use `?`), however the
/// function body block should evaluate to a Value.
#[proc_macro]
pub fn ergo_fn_value(ts: TokenStream) -> TokenStream {
    ergo_fn::ergo_fn_value(ts)
}

/// Mark a function as the plugin entrypoint.
///
/// The function must take no arguments and return a `Result<Value>`.
///
/// This entrypoint will set up thread local storage, a logger (to be used with the `log` crate),
/// and a static global `ERGO_PLUGIN_SOURCE: Source<()>`.
#[proc_macro_attribute]
pub fn plugin_entry(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let f = parse_macro_input!(item as ItemFn);

    let fn_name = f.sig.ident.clone();

    {
        quote! {
            #[ergo_runtime::abi_stable::sabi_extern_fn]
            #[no_mangle]
            pub fn _ergo_plugin(__plugin_ctx: ergo_runtime::plugin::Context)
                -> ergo_runtime::error::RResult<ergo_runtime::Value>
            {
                unsafe {
                    _ERGO_PLUGIN_SOURCE = __plugin_ctx.initialize();
                }
                #fn_name().into()
            }

            static mut _ERGO_PLUGIN_SOURCE: ergo_runtime::Source<()> = ergo_runtime::Source::new(0);

            fn plugin_source() -> ergo_runtime::Source<()> {
                unsafe { _ERGO_PLUGIN_SOURCE }
            }

            #f
        }
    }
    .into()
}
