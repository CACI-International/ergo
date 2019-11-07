extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::{quote, quote_spanned};
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Expr, Ident, LitStr,
};

#[proc_macro_hack]
pub fn item_name(ts: TokenStream) -> TokenStream {
    let input = parse_macro_input!(ts as LitStr);

    let s = input.value();
    if !s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
        quote_spanned! {
            input.span() => {
                compile_error!("literals may only have ascii alphanumeric and underscore characters");
            }
        }
    } else {
        quote! {
            unsafe { &*(#s as *const str as *const ::grease::ItemName) }
        }
    }
    .into()
}

struct Binding {
    name: Ident,
    value: Expr,
}

impl Parse for Binding {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        let value: Expr = if input.peek(syn::token::Eq) {
            input.parse::<syn::token::Eq>()?;
            input.parse()?
        } else {
            parse_quote! { #name }
        };
        Ok(Binding { name, value })
    }
}

struct MakeValue {
    tp: Expr,
    bindings: Punctuated<Binding, syn::token::Comma>,
    deps: Punctuated<Expr, syn::token::Comma>,
    body: Expr,
}

impl Parse for MakeValue {
    fn parse(input: ParseStream) -> Result<Self> {
        let tp: Expr = input.parse()?;
        input.parse::<syn::token::Comma>()?;
        let bindings = {
            if input.peek(syn::token::Paren) {
                let group;
                parenthesized!(group in input);
                group.parse_terminated(Binding::parse)?
            } else {
                Default::default()
            }
        };
        let deps = {
            if input.peek(syn::token::Bracket) {
                let group;
                bracketed!(group in input);
                group.parse_terminated(Expr::parse)?
            } else {
                Default::default()
            }
        };
        let body: Expr = input.parse()?;
        Ok(MakeValue {
            tp,
            bindings,
            deps,
            body,
        })
    }
}

#[proc_macro_hack]
pub fn make_value(ts: TokenStream) -> TokenStream {
    let input = parse_macro_input!(ts as MakeValue);

    let bindings = input.bindings.iter().map(|v| {
        let name = &v.name;
        let value = &v.value;
        quote! { let #name = #value.clone(); }
    });

    let deps = {
        let depslist = input
        .bindings
        .iter()
        .map(|v| {
            let name = &v.name;
            quote! { #name }
        })
        .chain(input.deps.iter().map(|v| quote! { #v }));
        quote! { depends![#(#depslist),*] }
    };

    let tp = input.tp;
    let body = input.body;

    let expanded = quote! {
        {
            #( #bindings )*
            let make_value__deps = #deps;
            ::grease::Value::new(#tp, async move { #body }, make_value__deps)
        }
    };

    TokenStream::from(expanded)
}
