//! Convenience function attributes to create Unbound values from rust functions.

use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse::Parser, parse_macro_input, parse_quote, spanned::Spanned, Error, ItemFn};

pub fn ergo_fn(item: TokenStream) -> TokenStream {
    let parser = ErgoFnLike(quote! { ergo_runtime::types::Args });
    parse_macro_input!(item with parser)
        .into_token_stream()
        .into()
}

pub fn ergo_fn_value(item: TokenStream) -> TokenStream {
    let parser = ErgoFnLike(quote! { ergo_runtime::types::Args });
    parse_macro_input!(item with parser)
        .block
        .into_token_stream()
        .into()
}

pub fn ergo_pat(item: TokenStream) -> TokenStream {
    let parser = ErgoFnLike(quote! { ergo_runtime::types::PatternArgs });
    parse_macro_input!(item with parser)
        .into_token_stream()
        .into()
}

pub fn ergo_pat_value(item: TokenStream) -> TokenStream {
    let parser = ErgoFnLike(quote! { ergo_runtime::types::PatternArgs });
    parse_macro_input!(item with parser)
        .block
        .into_token_stream()
        .into()
}

struct ErgoFnLike(proc_macro2::TokenStream);

impl Parser for ErgoFnLike {
    type Output = ItemFn;

    fn parse2(self, tokens: proc_macro2::TokenStream) -> syn::Result<Self::Output> {
        let mut f = (|s: syn::parse::ParseStream| s.parse::<ItemFn>()).parse2(tokens)?;
        let inner_block = f.block.clone();
        if f.sig.asyncness.take().is_none() {
            return Err(Error::new_spanned(f, "function must be async"));
        }

        let doc = {
            let ind = f.attrs.iter().position(|a| match a.parse_meta() {
                Ok(syn::Meta::NameValue(nv)) => nv.path.is_ident("doc"),
                _ => false,
            });
            ind.map(|i| {
                if let Ok(syn::Meta::NameValue(nv)) = f.attrs.swap_remove(i).parse_meta() {
                    nv.lit
                } else {
                    panic!("`position` incorrect");
                }
            })
        };

        let clones = {
            let ind = f.attrs.iter().position(|a| match a.parse_meta() {
                Ok(syn::Meta::List(l)) => l.path.is_ident("cloning"),
                _ => false,
            });
            ind.map(|i| {
                if let Ok(syn::Meta::List(l)) = f.attrs.swap_remove(i).parse_meta() {
                    l.nested
                        .into_iter()
                        .filter_map(|nm| match nm {
                            syn::NestedMeta::Meta(syn::Meta::Path(p)) => Some(p),
                            _ => None,
                        })
                        .collect::<Vec<_>>()
                } else {
                    panic!("`position` incorrect");
                }
            })
            .unwrap_or_default()
        };

        let id = f.sig.ident.clone();
        let inputs = std::mem::take(&mut f.sig.inputs);
        let has_rest = f.sig.variadic.take().is_some();

        enum Arg {
            Positional(syn::Pat),
            Keyed(syn::PatIdent),
        }

        let mut args = vec![];

        for i in inputs {
            match i {
                syn::FnArg::Typed(pt) => {
                    let (optional, ty) = match *pt.ty {
                        // If the type is inferred, it will be a Value
                        syn::Type::Infer(_) => (false, None),
                        syn::Type::Path(path) => {
                            if path.qself.is_some() {
                                return Err(Error::new_spanned(
                                    path,
                                    "qualified paths not supported",
                                ));
                            }
                            (false, Some(path.path))
                        }
                        // If the type is a slice type, this indicates its presence optional.
                        syn::Type::Slice(s) => match *s.elem {
                            syn::Type::Infer(_) => (true, None),
                            syn::Type::Path(path) => {
                                if path.qself.is_some() {
                                    return Err(Error::new_spanned(
                                        path,
                                        "qualified paths not supported",
                                    ));
                                }
                                (true, Some(path.path))
                            }
                            o => return Err(Error::new_spanned(o, "unsupported type")),
                        },
                        o => return Err(Error::new_spanned(o, "unsupported type")),
                    };

                    match *pt.pat {
                        syn::Pat::Wild(_) | syn::Pat::Ident(_) => {
                            args.push((Arg::Positional(*pt.pat), optional, ty))
                        }
                        // If the pattern is a tuple with one element, this indicates it is a arg keyed
                        // on the identifier's string.
                        syn::Pat::Tuple(mut t) => {
                            if t.elems.len() != 1 {
                                return Err(Error::new_spanned(t, "tuple patterns must have exactly one element (for a keyed argument)"));
                            }
                            let arg = t.elems.pop().unwrap().into_value();
                            match arg {
                                syn::Pat::Ident(i) => args.push((Arg::Keyed(i), optional, ty)),
                                o => {
                                    return Err(Error::new_spanned(
                                        o,
                                        "only ident patterns allowed as keyed arguments",
                                    ))
                                }
                            }
                        }
                        o => return Err(Error::new_spanned(o, "invalid argument pattern")),
                    }
                }
                o @ syn::FnArg::Receiver(_) => {
                    return Err(Error::new_spanned(
                        o,
                        "`self` parameter is only allowed in associated functions",
                    ))
                }
            }
        }

        let args = args.into_iter().map(|(arg, optional, ty)| {
            let (bind, get_arg) = match arg {
                Arg::Positional(p) => (p, quote! { __ergo_fn_args.next() }),
                Arg::Keyed(i) => {
                    let key = syn::LitStr::new(&i.ident.to_string(), i.span());
                    (i.into(), quote! { __ergo_fn_args.kw(#key) })
                }
            };
            let v_as_type = match ty {
                None => quote! { v },
                Some(ref ty) => quote! {
                    ergo_runtime::try_result!(CONTEXT.eval_as::<#ty>(v).await)
                }
            };
            let typed_val = if optional {
                quote! {
                    match #get_arg {
                        Some(v) => Some(#v_as_type),
                        None => None
                    }
                }
            } else {
                quote! {
                    match #get_arg {
                        Some(v) => #v_as_type,
                        None => ergo_runtime::try_result!(Err(
                                std::concat!("not enough arguments, expected ", std::stringify!(#ty), " ", std::stringify!(#bind))
                        ))
                    }
                }
            };
            quote! {
                let #bind = #typed_val;
            }
        });

        let rest_or_check = if has_rest {
            quote! {
                let REST = __ergo_fn_args;
            }
        } else {
            quote! {
                ergo_runtime::try_result!(__ergo_fn_args.unused_arguments());
                drop(__ergo_fn_args);
            }
        };

        let clones = clones.into_iter().map(|c| {
            quote_spanned! { c.span()=>
                let #c = #c.clone();
            }
        });

        let which = self.0;

        f.block = Box::new(parse_quote! {
            {
                ergo_runtime::types::Unbound::new(move |CONTEXT, __ergo_fn_arg| {
                    #(#clones)*
                    ergo_runtime::future::FutureExt::boxed(async move {
                        let (ARGS_SOURCE, __ergo_fn_args) = ergo_runtime::try_result!(CONTEXT.eval_as::<#which>(__ergo_fn_arg).await).take();
                        let mut __ergo_fn_args = __ergo_fn_args.to_owned().args;
                        #(#args)*
                        #rest_or_check
                        #inner_block
                    })
                },
                ergo_runtime::depends![ergo_runtime::nsid!(function, std::concat!(std::module_path!(),"::",std::stringify!(#id)).as_bytes())],
                #doc
                ).into()
            }
        });

        Ok(f)
    }
}
