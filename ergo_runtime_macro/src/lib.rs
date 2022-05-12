//! Proc-macros for the `ergo_runtime` crate.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

mod derive_ergo_type;
mod ergo_fn;
mod ergo_trait;
mod match_value;

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

/// Create an ergo Unbound for Args from a rust function declaration.
///
/// Evaluates to a function with no arguments which, when called, returns the ergo Value.
#[proc_macro_attribute]
pub fn ergo_fn(_args: TokenStream, item: TokenStream) -> TokenStream {
    ergo_fn::ergo_fn(item)
}

/// Create an ergo Unbound for Args from a rust function declaration.
///
/// Evaluates to the ergo Value.
#[proc_macro]
pub fn ergo_fn_value(ts: TokenStream) -> TokenStream {
    ergo_fn::ergo_fn_value(ts)
}

/// Mark a function as the plugin entrypoint.
///
/// The function must take a single `&Context` argument and return a `Result<Value>`.
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
                unsafe { __plugin_ctx.initialize_tls(); }
                #fn_name().into()
            }

            #f
        }
    }
    .into()
}
