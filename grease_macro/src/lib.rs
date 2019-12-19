extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::{quote, quote_spanned};
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    DeriveInput, Expr, Ident, LitStr,
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

struct Dep<T> {
    nested_deps: bool,
    value: T,
}

impl<T: Parse> Parse for Dep<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        let nested_deps = input.peek(syn::token::Caret);
        if nested_deps {
            input.parse::<syn::token::Caret>()?;
        }
        let value: T = input.parse()?;
        Ok(Dep { nested_deps, value })
    }
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
    bindings: Punctuated<Dep<Binding>, syn::token::Comma>,
    deps: Punctuated<Dep<Expr>, syn::token::Comma>,
    body: Expr,
}

impl Parse for MakeValue {
    fn parse(input: ParseStream) -> Result<Self> {
        let bindings = if input.peek(syn::token::Paren) {
            let group;
            parenthesized!(group in input);
            group.parse_terminated(Dep::parse)?
        } else {
            Default::default()
        };
        let deps = if input.peek(syn::token::Bracket) {
            let group;
            bracketed!(group in input);
            group.parse_terminated(Dep::parse)?
        } else {
            Default::default()
        };
        let body: Expr = input.parse()?;
        Ok(MakeValue {
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
        let name = &v.value.name;
        let value = &v.value.value;
        quote! { let #name = #value.clone(); }
    });

    let deps = {
        let (nested_deps, value_deps): (Vec<_>, Vec<_>) = input
            .bindings
            .iter()
            .map(|v| {
                let name = &v.value.name;
                (v.nested_deps, quote! { #name })
            })
            .chain(input.deps.iter().map(|v| {
                let val = &v.value;
                (v.nested_deps, quote! { #val })
            }))
            .partition(|(n, _)| *n);

        let nested_deps = nested_deps.into_iter().map(|(_, v)| v);
        let value_deps = value_deps.into_iter().map(|(_, v)| v);
        quote! { {
            let value_deps = ::grease::depends![#(#value_deps),*];
            ::grease::depends![join value_deps #(, &#nested_deps)*]
        } }
    };

    let body = input.body;

    let expanded = quote! {
        {
            #( #bindings )*
            let make_value__deps = #deps;
            ::grease::TypedValue::new(async move { #body }, make_value__deps)
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(GetValueType)]
pub fn derive_get_value_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let params = input.generics.params;
    let param_reqs = params.clone().into_iter().map(|mut p| {
        if let syn::GenericParam::Type(ref mut tp) = &mut p {
            tp.bounds.push(parse_quote! {
                    ::grease::GetValueType
            });
        }
        p
    });
    let param_args = params.clone().into_iter().map(|e| match e {
        syn::GenericParam::Type(tp) => {
            let ident = tp.ident;
            quote!(#ident)
        }
        syn::GenericParam::Lifetime(lt) => {
            let lifetime = lt.lifetime;
            quote!(#lifetime)
        }
        syn::GenericParam::Const(c) => {
            let ident = c.ident;
            quote!(#ident)
        }
    });
    let set_data = params.into_iter().filter_map(|e| match e {
        syn::GenericParam::Type(tp) => {
            let ident = tp.ident;
            Some(quote! {
                {
                    let tp = #ident::value_type();
                    data.extend(tp.id.as_bytes());
                    data.extend(tp.data);
                }
            })
        }
        _ => None,
    });
    let namestr = syn::LitStr::new(&format!("{}", name), name.span());

    let expanded = quote! {
        impl<#(#param_reqs),*> ::grease::GetValueType for #name<#(#param_args),*> {
            fn value_type() -> ::grease::ValueType {
                let mut data = Vec::new();
                #(#set_data)*
                ::grease::ValueType::with_data(::grease::type_uuid(concat![module_path!(), "::", #namestr].as_bytes()), data)
            }
        }
    };

    TokenStream::from(expanded)
}
