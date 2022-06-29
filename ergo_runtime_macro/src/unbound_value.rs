//! Create unbound values.

use crate::lazy_block::LazyBlock;
use proc_macro::TokenStream;
use quote::quote;

pub fn unbound_value(ts: TokenStream) -> TokenStream {
    let LazyBlock {
        id,
        use_captures,
        contains,
        doc,
        body,
    } = syn::parse_macro_input!(ts);
    let (capture, create_capture, bind_capture) = contains.into_components();
    let doc = if doc.is_empty() {
        None
    } else {
        Some(quote! { ergo_runtime::metadata::Doc::set_string(&mut v, #doc); })
    };
    quote! {
        {
            let __unbound_value_id = #id;
            #create_capture
            let __unbound_value_f = move |#capture, mut ARG: Value| async move {
                ergo_runtime::Value::from(async move {
                    #bind_capture
                    let r = {#body};
                    // In case `body` only contains a `return`, allow unreachable code.
                    #[allow(unreachable_code)]
                    ergo_runtime::Result::<ergo_runtime::Value>::Ok(r)
                }.await)
            };
            let mut v = ergo_runtime::Value::new(ergo_runtime::types::Unbound::new(
                ergo_runtime::types::unbound::UnboundFn::<_, _, #use_captures>::new(__unbound_value_id, #capture, __unbound_value_f)
            ));
            #doc
            v
        }
    }.into()
}
