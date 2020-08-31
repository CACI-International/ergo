//! Proc-macros for the `ergo_runtime` crate.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn};

/// Mark a function as the plugin entrypoint.
///
/// The function must take a single `&mut Context<Runtime>` argument and return a
/// `ergo_runtime::EvalResult`.
#[proc_macro_attribute]
pub fn plugin_entry(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let f = parse_macro_input!(item as ItemFn);

    let fn_name = f.sig.ident.clone();

    {quote! {
        #[abi_stable::sabi_extern_fn]
        #[no_mangle]
        pub fn _so_plugin(ctx: &mut ::grease::runtime::Context<::ergo_runtime::Runtime>)
            -> abi_stable::std_types::RResult<::ergo_runtime::source::Source<::grease::value::Value>, ::grease::value::Error>
        {
            #fn_name(ctx).into()
        }

        #f
    }}.into()
}
