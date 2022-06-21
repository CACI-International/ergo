//! Convenience macro to create lazy values.

use proc_macro::TokenStream;
use quote::quote;

#[derive(Default)]
struct LazyValue {
    contains: Vec<syn::Ident>,
    depends: proc_macro2::TokenStream,
    id: proc_macro2::TokenStream,
    overwrite_depends: bool,
    eval_for_id: bool,
    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for LazyValue {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut ret = LazyValue::default();
        for attr in syn::Attribute::parse_inner(input)? {
            if attr.path.is_ident("contains") {
                ret.contains.extend(attr.parse_args_with(
                    syn::punctuated::Punctuated::<syn::Ident, syn::token::Comma>::parse_terminated,
                )?);
            } else if attr.path.is_ident("depends") {
                ret.depends = attr.parse_args()?;
            } else if attr.path.is_ident("id") {
                ret.id = attr.parse_args()?;
                ret.overwrite_depends = true;
            } else if attr.path.is_ident("exclude_contains_from_id") {
                ret.overwrite_depends = true;
            } else if attr.path.is_ident("eval_for_id") {
                ret.eval_for_id = true;
            } else {
                return Err(syn::Error::new_spanned(attr, "unsupported inner attribute"));
            }
        }
        ret.body = input.parse()?;
        Ok(ret)
    }
}

pub fn lazy_value(ts: TokenStream) -> TokenStream {
    let LazyValue {
        contains,
        depends,
        id,
        overwrite_depends,
        eval_for_id,
        body,
    } = syn::parse_macro_input!(ts);
    let use_captures = if overwrite_depends {
        quote! { false }
    } else {
        quote! { true }
    };
    let id = if id.is_empty() {
        if depends.is_empty() {
            quote! { 0 }
        } else {
            quote! { ergo_runtime::depends![#depends] }
        }
    } else {
        id
    };
    let id = if eval_for_id {
        quote! { ergo_runtime::value::IdInfo::new(#id).eval_for_id(true) }
    } else {
        id
    };
    quote! {
        {
            let __lazy_value_id = #id;
            let (__lazy_value_captures, __lazy_value_witness) = ergo_runtime::value::lazy::CapturesWitness::witness((#(#contains,)*));
            let __lazy_value_f = move |__lazy_value_captures| async move {
                ergo_runtime::Value::from(async move {
                    let (#(#contains,)*) = __lazy_value_witness.check(__lazy_value_captures);
                    ergo_runtime::Result::<ergo_runtime::Value>::Ok({#body})
                }.await)
            };
            ergo_runtime::Value::new(ergo_runtime::value::lazy::LazyValueFn::<_, _, #use_captures>::new(__lazy_value_id, __lazy_value_captures, __lazy_value_f))
        }
    }.into()
}
