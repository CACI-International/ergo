use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput, GenericParam, LitStr};

/// Derive the ErgoType trait.
pub fn derive_ergo_type_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let params = input.generics.params;
    let param_reqs = params.clone().into_iter().map(|mut p| {
        if let GenericParam::Type(ref mut tp) = &mut p {
            tp.bounds.push(parse_quote! {
                ergo_runtime::type_system::ErgoType
            });
        }
        p
    });
    let param_args = params.clone().into_iter().map(|e| match e {
        GenericParam::Type(tp) => {
            let ident = tp.ident;
            quote!(#ident)
        }
        GenericParam::Lifetime(lt) => {
            let lifetime = lt.lifetime;
            quote!(#lifetime)
        }
        GenericParam::Const(c) => {
            let ident = c.ident;
            quote!(#ident)
        }
    });
    let set_data = params.into_iter().filter_map(|e| match e {
        GenericParam::Type(tp) => {
            let ident = tp.ident;
            Some(quote! {
                {
                    type_params.0.push(#ident::ergo_type());
                }
            })
        }
        _ => None,
    });
    let namestr = LitStr::new(&format!("{}", name), name.span());

    let expanded = quote! {
        impl<#(#param_reqs),*> ergo_runtime::type_system::ErgoType for #name<#(#param_args),*> {
            fn ergo_type() -> ergo_runtime::type_system::Type {
                let mut type_params = ergo_runtime::type_system::TypeParameters::default();
                #(#set_data)*
                ergo_runtime::type_system::Type::with_data(
                    ergo_runtime::nsid!(
                        type,
                        concat![module_path!(), "::", #namestr].as_bytes()
                    ),
                    type_params.into()
                )
            }
        }
    };

    TokenStream::from(expanded)
}
