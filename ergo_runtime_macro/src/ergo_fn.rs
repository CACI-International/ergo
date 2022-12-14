//! Convenience function attributes to create Unbound values from rust functions.

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
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

        let eval_for_id = {
            match f.attrs.iter().position(|a| match a.parse_meta() {
                Ok(syn::Meta::Path(l)) if l.is_ident("eval_for_id") => true,
                _ => false,
            }) {
                Some(ind) => {
                    f.attrs.swap_remove(ind);
                    true
                }
                None => false,
            }
        };

        let depends = {
            f.attrs
                .iter()
                .position(|a| a.path.is_ident("depends"))
                .map(|i| {
                    f.attrs
                        .swap_remove(i)
                        .parse_args::<proc_macro2::TokenStream>()
                })
                .transpose()?
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
                    ergo_runtime::Context::eval_as::<#ty>(v).await?
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
                        None => return Err(ergo_runtime::error!{
                            labels: [ primary(ARGS_SOURCE.with("in this function call")) ],
                            error: std::concat!("missing argument: ", std::stringify!(#bind))
                        })
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
                __ergo_fn_args.unused_arguments()?;
                drop(__ergo_fn_args);
            }
        };

        let which = self.0;

        let deps = match depends {
            None => quote! { ergo_runtime::depends![const __ergo_fn_id] },
            Some(ds) => quote! { ergo_runtime::depends![dyn __ergo_fn_id, #ds] },
        };

        let fn_id = quote! {
            let __ergo_fn_id = ergo_runtime::nsid!(function, const std::concat!(std::module_path!(),"::",std::stringify!(#id)).as_bytes());
        };

        let declare_deps = quote! {
            let __ergo_fn_deps = #deps;
            let __ergo_fn_deps_inner = __ergo_fn_deps.clone();
        };

        let deps = if eval_for_id {
            quote! { ergo_runtime::value::IdInfo::new(__ergo_fn_deps).eval_for_id(true) }
        } else {
            quote! { __ergo_fn_deps }
        };

        let imp = quote! {
            ergo_runtime::types::unbound_value! {
                #![doc = #doc]
                #![id(#deps)]
                let __ergo_fn_args = ergo_runtime::Context::eval_as::<#which>(ARG).await?;
                let ARGS_SOURCE = ergo_runtime::metadata::Source::get(&__ergo_fn_args);
                let CALL_DEPENDS: ergo_runtime::dependency::Dependencies = __ergo_fn_deps_inner + ergo_runtime::depends![dyn __ergo_fn_args];
                let mut __ergo_fn_args = __ergo_fn_args.into_owned().args;
                #(#args)*
                #rest_or_check
                return ergo_runtime::error_info!(
                    labels: [
                        secondary(ARGS_SOURCE.with("in this call"))
                    ],
                    async {
                        ergo_runtime::Result::<ergo_runtime::Value>::Ok(#inner_block)
                    }
                );
            }
        };

        f.block = Box::new(parse_quote! {
            {
                #fn_id
                #declare_deps
                #imp
            }
        });

        Ok(f)
    }
}
