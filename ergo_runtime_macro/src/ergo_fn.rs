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
            let mut strip_leading_ws = None;
            let mut docstr = String::new();
            for attr in f.attrs.iter() {
                match attr.parse_meta() {
                    Ok(syn::Meta::NameValue(nv)) if nv.path.is_ident("doc") => {
                        if let syn::Lit::Str(s) = nv.lit {
                            let s = s.value();
                            match (strip_leading_ws, s.find(|c: char| !c.is_whitespace())) {
                                (None, None) => (),
                                (None, Some(offset)) => {
                                    strip_leading_ws = Some(offset);
                                    docstr.push_str(unsafe { s.get_unchecked(offset..) });
                                }
                                (Some(_), None) => docstr.push('\n'),
                                (Some(a), Some(b)) => {
                                    docstr.push('\n');
                                    docstr.push_str(unsafe {
                                        s.get_unchecked(std::cmp::min(a, b)..)
                                    });
                                }
                            }
                        }
                    }
                    _ => (),
                }
            }
            docstr.trim().to_owned()
        };

        let forced = {
            match f.attrs.iter().position(|a| match a.parse_meta() {
                Ok(syn::Meta::Path(l)) if l.is_ident("forced") => true,
                _ => false,
            }) {
                Some(ind) => {
                    f.attrs.swap_remove(ind);
                    true
                }
                None => false,
            }
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
                    let key = syn::LitStr::new(&i.ident.to_string().replace('_', "-"), i.span());
                    (i.into(), quote! { __ergo_fn_args.kw(#key) })
                }
            };
            let v_as_type = match ty {
                None => quote! { v },
                Some(ref ty) => quote! {
                    ergo_runtime::try_result!(ergo_runtime::Context::eval_as::<#ty>(v).await)
                },
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
                        None => ergo_runtime::try_result!(Err(ergo_runtime::error!{
                            labels: [ primary(ARGS_SOURCE.with("in this function call")) ],
                            error: std::concat!("missing argument: ", std::stringify!(#bind))
                        }))
                    }
                }
            };
            quote! {
                let #bind = #typed_val;
            }
        });

        let rest_or_check = if has_rest {
            quote! {
                let mut REST = __ergo_fn_args;
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

        let deps = {
            let deps = quote! { ergo_runtime::depends![const fn_id] };
            if forced {
                quote! { ergo_runtime::value::EvalForId::set(#deps) }
            } else {
                deps
            }
        };

        f.block = Box::new(parse_quote! {
            {
                let fn_id = ergo_runtime::nsid!(function, std::concat!(std::module_path!(),"::",std::stringify!(#id)).as_bytes());
                ergo_runtime::types::Unbound::new(move |__ergo_fn_arg| {
                    #(#clones)*
                    ergo_runtime::future::FutureExt::boxed(async move {
                        let __ergo_fn_args = ergo_runtime::try_result!(ergo_runtime::Context::eval_as::<#which>(__ergo_fn_arg).await);
                        let ARGS_SOURCE = ergo_runtime::metadata::Source::get(&__ergo_fn_args);
                        let CALL_DEPENDS: ergo_runtime::dependency::Dependencies = ergo_runtime::depends![fn_id, __ergo_fn_args];
                        let mut __ergo_fn_args = __ergo_fn_args.to_owned().args;
                        #(#args)*
                        #rest_or_check
                        ergo_runtime::error_info!(
                            labels: [
                                secondary(ARGS_SOURCE.with("in this call"))
                            ],
                            async {
                                ergo_runtime::Result::<Value>::Ok(#inner_block)
                            }
                        ).into()
                    })
                },
                #deps,
                #doc
                ).into()
            }
        });

        Ok(f)
    }
}
