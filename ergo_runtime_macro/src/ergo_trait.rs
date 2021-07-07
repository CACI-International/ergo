use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use std::collections::HashSet;
use syn::{parse::Result, parse_macro_input, parse_quote, spanned::Spanned, ItemStruct, ItemTrait};

pub fn ergo_trait(item: TokenStream) -> TokenStream {
    ergo_trait_definition(parse_macro_input!(item))
}

pub fn ergo_trait_impl(item: TokenStream) -> TokenStream {
    match do_ergo_trait_impl(parse_macro_input!(item)) {
        Ok(v) => v.2,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

fn impl_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}Impl", name), name.span())
}

#[derive(Debug)]
enum ReceiverType {
    Owned(proc_macro2::Span),
    Reference(proc_macro2::Span),
    None,
}

impl ReceiverType {
    pub fn present(&self) -> bool {
        match self {
            ReceiverType::None => false,
            _ => true,
        }
    }
}

fn ergo_trait_definition(trt: ItemTrait) -> TokenStream {
    let mut errs = proc_macro2::TokenStream::new();

    let mut funcs = Vec::new();

    let generic_types: HashSet<_> = trt
        .generics
        .type_params()
        .map(|t| t.ident.to_string())
        .collect();

    for item in trt.items {
        use syn::TraitItem::*;
        match item {
            Const(_) => errs.extend(
                syn::Error::new(item.span(), "constants not supported in ergo traits")
                    .to_compile_error(),
            ),
            Type(_) => errs.extend(
                syn::Error::new(item.span(), "types not supported in ergo traits")
                    .to_compile_error(),
            ),
            Macro(_) => errs.extend(
                syn::Error::new(item.span(), "macros not supported in ergo traits")
                    .to_compile_error(),
            ),
            Verbatim(_) => errs.extend(
                syn::Error::new(item.span(), "unrecognized ergo trait item").to_compile_error(),
            ),
            Method(m) => {
                let mut args = Vec::new();
                let is_async = m.sig.asyncness.is_some();
                let mut inputs = m.sig.inputs.into_iter().peekable();
                let rcvr = if let Some(syn::FnArg::Receiver(r)) = inputs.peek() {
                    if r.reference.is_none() {
                        ReceiverType::Owned(r.span())
                    } else if r.mutability.is_none() {
                        ReceiverType::Reference(r.span())
                    } else {
                        errs.extend(
                            syn::Error::new(r.span(), "self must be a borrowed reference")
                                .to_compile_error(),
                        );
                        ReceiverType::None
                    }
                } else {
                    ReceiverType::None
                };
                if rcvr.present() {
                    inputs.next().unwrap();
                }
                for i in inputs {
                    match i {
                        syn::FnArg::Typed(pt) => {
                            args.push((*pt.pat, *pt.ty));
                        }
                        _ => errs.extend(
                            syn::Error::new(i.span(), "unexpected receiver").to_compile_error(),
                        ),
                    }
                }
                let ret = match m.sig.output {
                    syn::ReturnType::Default => parse_quote! { () },
                    syn::ReturnType::Type(_, tp) => *tp,
                };

                funcs.push((is_async, m.sig.ident, rcvr, args, ret));
            }
            _ => (),
        }
    }

    let ref_ident = trt.ident;

    // Construct fields for impl struct
    let fields = funcs.iter().map(|(is_async, ident, rcvr, args, ret)| {
        let mut fn_args = Vec::new();
        match rcvr {
            ReceiverType::Reference(span) => {
                fn_args.push(quote_spanned! { *span=> &'a ergo_runtime::value::Value });
                fn_args.push(quote_spanned! { *span=> &'a ergo_runtime::type_system::Type });
                fn_args.push(quote_spanned! { *span=> &'a ergo_runtime::abi_stable::type_erase::Erased });
            }
            ReceiverType::Owned(span) => {
                fn_args.push(quote_spanned! { *span=> ergo_runtime::value::Value });
            }
            ReceiverType::None => ()
        }
        for (_, t) in args {
            let mut ty = t.clone();
            if let syn::Type::Reference(tr) = &mut ty {
                tr.lifetime = Some(parse_quote! { 'a });
            }
            fn_args.push(quote_spanned! { ty.span() => #ty });
        }

        let ret = if generic_types.contains(&quote! {#ret}.to_string()) {
            parse_quote! { ergo_runtime::value::Value }
        } else {
            ret.clone()
        };

        let fn_ret = if *is_async {
            parse_quote! { ergo_runtime::abi_stable::future::BoxFuture<'a, #ret> }
        } else {
            ret
        };

        syn::Field {
            attrs: Default::default(),
            vis: parse_quote! { pub },
            ident: Some(ident.clone()),
            colon_token: Some(Default::default()),
            ty: parse_quote! {
                    ergo_runtime::abi_stable::closure::FnPtr<
                        for<'a> extern "C" fn(&'a ergo_runtime::abi_stable::type_erase::Erased, &'a ergo_runtime::Context, #(#fn_args),*)
                            -> #fn_ret
                    >
            },
        }
    });

    // Add ergo trait data field
    let fields = fields.chain(std::iter::once({
        syn::Field {
            attrs: Default::default(),
            vis: parse_quote! { pub },
            ident: parse_quote! { ergo_trait_data },
            colon_token: Some(Default::default()),
            ty: parse_quote! { ergo_runtime::abi_stable::type_erase::Erased },
        }
    }));

    let fields = fields.chain(generic_types.iter().enumerate().map(|(n, t)| {
        let name = syn::Ident::new(&format!("_phantom{}", n), t.span());
        use std::str::FromStr;
        let t = proc_macro2::TokenStream::from_str(&t).unwrap();
        syn::Field {
            attrs: Default::default(),
            vis: parse_quote! { pub },
            ident: Some(name),
            colon_token: Some(Default::default()),
            ty: parse_quote! { std::marker::PhantomData<extern "C" fn() -> #t> },
        }
    }));

    let fields = syn::FieldsNamed {
        brace_token: trt.brace_token,
        named: fields.collect(),
    };

    let impl_struct_name = impl_name(&ref_ident);

    let mut impl_attrs = trt.attrs.clone();
    impl_attrs.push(parse_quote! { #[derive(ergo_runtime::abi_stable::StableAbi)] });
    impl_attrs.push(parse_quote! { #[repr(C)] });

    let impl_struct = ItemStruct {
        attrs: impl_attrs,
        vis: trt.vis.clone(),
        struct_token: syn::token::Struct {
            span: trt.trait_token.span,
        },
        ident: impl_struct_name.clone(),
        generics: trt.generics.clone(),
        fields: syn::Fields::Named(fields),
        semi_token: None,
    };

    let ergo_trait = impl_ergo_trait(ref_ident.clone(), trt.generics.clone());

    let ref_generics = trt.generics.clone();
    let (impl_generics, type_generics, where_clause) = ref_generics.split_for_impl();

    let mut ref_attrs = vec![];
    ref_attrs.push(parse_quote! { #[derive(Clone)] });

    let ref_struct = ItemStruct {
        attrs: ref_attrs,
        vis: trt.vis,
        struct_token: syn::token::Struct {
            span: trt.trait_token.span,
        },
        ident: ref_ident.clone(),
        generics: ref_generics.clone(),
        fields: syn::Fields::Named(parse_quote! {
            {
                inner: ergo_runtime::type_system::Ref<#impl_struct_name #type_generics>,
            }
        }),
        semi_token: None,
    };

    let ref_struct_impl_methods = funcs.iter().map(|(is_async, ident, rcvr, args, ret)| {
        let v = match rcvr {
            ReceiverType::Reference(span) | ReceiverType::Owned(span) =>
                    quote_spanned! { *span=> ergo_trait_self_value: ergo_runtime::value::Value, },
            _ => Default::default()
        };
        let arg_names = args.iter().map(|(name, _)| name);
        let args = args.iter().map(|(name, ty)| quote! { #name: #ty });
        let call_v = match rcvr {
            ReceiverType::Reference(span) => {
                quote_spanned! { *span=> &ergo_trait_self_value, ergo_trait_self_value.ergo_type().unwrap(), ergo_trait_self_value.data().unwrap(), }
            }
            ReceiverType::Owned(span) => {
                quote_spanned! { *span=> ergo_trait_self_value, }
            }
            ReceiverType::None => Default::default()
        };
        let async_tok = if *is_async {
            quote! { async }
        } else {
            Default::default()
        };
        let await_tok = if *is_async {
            quote! { .await }
        } else {
            Default::default()
        };
        quote_spanned! { ident.span()=>
            pub #async_tok fn #ident(&self, ctx: &ergo_runtime::Context, #v #(#args),*) -> #ret {
                let inner = &self.inner;
                (inner.#ident.as_fn())(&inner.ergo_trait_data, ctx, #call_v #(#arg_names),*)#await_tok
            }
        }
    });

    let ref_struct_impl = quote! {
        impl #impl_generics #ref_ident #type_generics
        #where_clause
        {
            #(#ref_struct_impl_methods)*
        }
    };

    TokenStream::from(quote! {
        #errs
        #impl_struct
        #ref_struct
        #ref_struct_impl
        #ergo_trait
    })
}

fn impl_ergo_trait(name: syn::Ident, mut generics: syn::Generics) -> proc_macro2::TokenStream {
    for p in generics.params.iter_mut() {
        if let syn::GenericParam::Type(ref mut tp) = p {
            tp.bounds.push(parse_quote! {
                ergo_runtime::type_system::ErgoType
            });
        }
    }
    let set_data = generics.params.iter().filter_map(|e| match e {
        syn::GenericParam::Type(tp) => {
            let ident = tp.ident.clone();
            Some(quote! {
                {
                    type_params.0.push(#ident::ergo_type());
                }
            })
        }
        _ => None,
    });
    let namestr = syn::LitStr::new(&format!("{}", name), name.span());

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let imp_name = impl_name(&name);

    let expanded = quote! {
        impl #impl_generics ergo_runtime::type_system::ErgoTrait for #name #type_generics
        #where_clause
        {
            type Impl = #imp_name #type_generics;

            fn ergo_trait() -> ergo_runtime::type_system::Trait {
                let mut type_params = ergo_runtime::type_system::TypeParameters::default();
                #(#set_data)*
                ergo_runtime::type_system::Trait::with_data(
                    ergo_runtime::nsid!(trait, concat![module_path!(), "::", #namestr].as_bytes()),
                    type_params.into(),
                )
            }

            fn create(inner: ergo_runtime::type_system::Ref<Self::Impl>) -> Self {
                #name { inner }
            }
        }
    };

    expanded
}

fn do_ergo_trait_impl(
    imp: syn::ItemImpl,
) -> Result<(syn::Type, syn::Type, proc_macro2::TokenStream)> {
    let mut errs = proc_macro2::TokenStream::new();

    let span = imp.span();

    let (trait_name, struct_name, args) = match imp.trait_ {
        None => return Err(syn::Error::new(imp.span(), "no trait for impl")),
        Some((_, mut v, _)) => {
            let orig = v.clone();
            let mut args = syn::PathArguments::None;
            if let Some(seg) = v.segments.last_mut() {
                seg.ident = impl_name(&seg.ident);
                args = seg.arguments.clone();
                seg.arguments = syn::PathArguments::None;
            }
            (orig, v, args)
        }
    };

    let ty = *imp.self_ty;

    let mut funcs = Vec::new();

    let generic_args: HashSet<_> = match args {
        syn::PathArguments::AngleBracketed(args) => args
            .args
            .into_iter()
            .filter_map(|t| {
                if let syn::GenericArgument::Type(t) = t {
                    Some(t)
                } else {
                    None
                }
            })
            .collect(),
        _ => Default::default(),
    };

    for item in imp.items {
        use syn::ImplItem::*;
        match item {
            Const(_) => errs.extend(
                syn::Error::new(item.span(), "constants not supported in ergo traits")
                    .to_compile_error(),
            ),
            Type(_) => errs.extend(
                syn::Error::new(item.span(), "types not supported in ergo traits")
                    .to_compile_error(),
            ),
            Macro(_) => errs.extend(
                syn::Error::new(item.span(), "macros not supported in ergo traits")
                    .to_compile_error(),
            ),
            Verbatim(_) => errs.extend(
                syn::Error::new(item.span(), "unrecognized ergo trait item").to_compile_error(),
            ),
            Method(m) => {
                let mut args = Vec::new();
                let is_async = m.sig.asyncness.is_some();
                let mut inputs = m.sig.inputs.into_iter().peekable();
                let rcvr = if let Some(syn::FnArg::Receiver(r)) = inputs.peek() {
                    if r.reference.is_none() {
                        ReceiverType::Owned(r.span())
                    } else if r.mutability.is_none() {
                        ReceiverType::Reference(r.span())
                    } else {
                        errs.extend(
                            syn::Error::new(r.span(), "self must be a borrowed reference")
                                .to_compile_error(),
                        );
                        ReceiverType::None
                    }
                } else {
                    ReceiverType::None
                };
                if rcvr.present() {
                    inputs.next().unwrap();
                }
                for i in inputs {
                    match i {
                        syn::FnArg::Typed(pt) => {
                            args.push((*pt.pat, *pt.ty));
                        }
                        _ => errs.extend(
                            syn::Error::new(i.span(), "unexpected receiver").to_compile_error(),
                        ),
                    }
                }
                let ret = match m.sig.output {
                    syn::ReturnType::Default => parse_quote! { () },
                    syn::ReturnType::Type(_, tp) => *tp,
                };

                funcs.push((is_async, m.sig.ident, rcvr, args, ret, m.block));
            }
            _ => (),
        }
    }

    let mut generics = imp.generics.clone();
    let (_, type_generics, _) = imp.generics.split_for_impl();
    let type_generics = type_generics.as_turbofish();
    generics.params.insert(0, parse_quote! { 'a });

    let (impl_generics, _, where_clause) = generics.split_for_impl();

    let untyped = if let syn::Type::Infer(_) = &ty {
        true
    } else {
        false
    };

    let impl_ty = ty.clone();
    let func_defs = funcs.iter().map(|(is_async, name, rcvr, args, ret, block)| {
        let mut fn_args = Vec::new();
        let mut self_bind = None;
        let mut block_tokens = block.into_token_stream();

        fn replace_self(ts: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
            use proc_macro2::TokenTree::*;
            ts.into_iter()
                .map(|tt| match tt {
                    Group(g) => Group(proc_macro2::Group::new(
                        g.delimiter(),
                        replace_self(g.stream()),
                    )),
                    Ident(id) => {
                        if id == "self" {
                            Ident(proc_macro2::Ident::new("ergo_trait_self", id.span()))
                        } else {
                            Ident(id)
                        }
                    }
                    o => o,
                })
                .collect()
        }

        match rcvr {
            ReceiverType::Reference(span) => {
                fn_args.insert(0, quote_spanned! { *span=> #[allow(non_snake_case,unused)] SELF_VALUE: &'a ergo_runtime::value::Value });
                fn_args.insert(
                    1,
                    quote_spanned! { *span=> #[allow(non_snake_case,unused)] SELF_TYPE: &'a ergo_runtime::type_system::Type }
                );
                fn_args.insert(
                    2,
                    quote_spanned! { *span=> ergo_trait_erased: &'a ergo_runtime::abi_stable::type_erase::Erased },
                );
                self_bind = Some(if untyped {
                    quote_spanned! { *span=> #[allow(unused)] let ergo_trait_self = ergo_trait_erased; }
                } else {
                    quote_spanned! { *span=> #[allow(unused)] let ergo_trait_self = unsafe { ergo_trait_erased.as_ref::<#ty>() }; }
                });
                block_tokens = replace_self(block_tokens);
            }
            ReceiverType::Owned(span) => {
                fn_args.insert(
                    0,
                    quote_spanned! { *span=> ergo_trait_erased: ergo_runtime::value::Value },
                );
                self_bind = Some(if untyped {
                    quote_spanned! { *span=> let ergo_trait_self = ergo_trait_erased; }
                } else {
                    quote_spanned! { *span=> let ergo_trait_self = ergo_trait_erased.as_type::<#ty>().unwrap(); }
                });
                block_tokens = replace_self(block_tokens);
            }
            ReceiverType::None => ()
        }

        for (n, t) in args {
            let mut ty = t.clone();
            if let syn::Type::Reference(tr) = &mut ty {
                tr.lifetime = Some(parse_quote! { 'a });
            }
            fn_args.push(quote! { #n: #ty });
        }

        let ret = ret.clone();

        let fn_ret = if *is_async {
            parse_quote! { ergo_runtime::abi_stable::future::BoxFuture<'a, #ret> }
        } else {
            ret
        };

        let fn_body = if *is_async {
            quote_spanned! { block_tokens.span()=>
                ergo_runtime::abi_stable::future::BoxFuture::new(async move {
                    #self_bind
                    #block_tokens
                })
            }
        } else {
            quote! {
                #self_bind
                #block_tokens
            }
        };

        quote_spanned! { name.span() =>
            #[allow(improper_ctypes_definitions)]
            extern "C" fn #name #impl_generics(#[allow(non_snake_case,unused)] TRAIT_DATA: &'a ergo_runtime::abi_stable::type_erase::Erased,
                #[allow(non_snake_case,unused)] CONTEXT: &'a ergo_runtime::Context, #(#fn_args),*)
                -> #fn_ret
            #where_clause
            {
                #fn_body
            }
        }
    });

    let func_sets = funcs.iter().map(|(_, name, _, _, _, _)| {
        quote_spanned! { name.span() =>
            #name: unsafe { ergo_runtime::abi_stable::closure::FnPtr::new(#name #type_generics) }
        }
    });

    let val_set = func_sets.chain(std::iter::once(
        parse_quote! { ergo_trait_data: Default::default() },
    ));

    let field_sets = val_set.chain(generic_args.iter().enumerate().map(|(n, t)| {
        let name = syn::Ident::new(&format!("_phantom{}", n), t.span());
        parse_quote! {
            #name: Default::default()
        }
    }));

    Ok((
        parse_quote! { #trait_name },
        impl_ty,
        quote_spanned! { span =>
            {
                #errs

                #(#func_defs)*

                #struct_name {
                    #(#field_sets),*
                }
            }
        },
    ))
}

pub fn ergo_traits_fn(ts: TokenStream) -> TokenStream {
    fn parse(s: syn::parse::ParseStream) -> Result<TokenStream> {
        if s.is_empty() {
            return Ok(TokenStream::new());
        }
        use syn::parse::discouraged::Speculative;
        let o = s.fork();
        let mut ts = match o.parse::<syn::ItemImpl>() {
            Ok(imp) => {
                s.advance_to(&o);
                let (trt, ty, imp) = do_ergo_trait_impl(imp)?;
                quote! { traits.add_impl_for_type::<#ty, #trt>(#imp); }
            }
            Err(_) => s.parse::<proc_macro2::TokenTree>().unwrap().into(),
        };
        ts.extend(proc_macro2::TokenStream::from(parse(s)?));
        Ok(ts.into())
    }

    let ts = proc_macro2::TokenStream::from(parse_macro_input!(ts with parse));

    TokenStream::from(quote! {
        pub fn ergo_traits(traits: &ergo_runtime::context::Traits) {
            #ts
        }
    })
}
