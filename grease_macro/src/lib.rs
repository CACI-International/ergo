extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    DeriveInput, Expr, Ident, ItemStruct, ItemTrait, LitStr,
};

#[proc_macro]
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
            unsafe { &*(#s as *const str as *const ::grease::runtime::ItemName) }
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

#[proc_macro]
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
            ::grease::depends![#(#value_deps),* #(, ^#nested_deps)*]
        } }
    };

    let body = input.body;

    let expanded = quote! {
        {
            #( #bindings )*
            let make_value__deps = #deps;
            ::grease::value::TypedValue::new(async move { #body }, make_value__deps)
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(GreaseType)]
pub fn derive_grease_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let params = input.generics.params;
    let param_reqs = params.clone().into_iter().map(|mut p| {
        if let syn::GenericParam::Type(ref mut tp) = &mut p {
            tp.bounds.push(parse_quote! {
                    ::grease::types::GreaseType
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
                    type_params.0.push(#ident::grease_type());
                }
            })
        }
        _ => None,
    });
    let namestr = syn::LitStr::new(&format!("{}", name), name.span());

    let expanded = quote! {
        impl<#(#param_reqs),*> ::grease::types::GreaseType for #name<#(#param_args),*> {
            fn grease_type() -> ::grease::types::Type {
                let mut type_params = ::grease::types::TypeParameters::default();
                #(#set_data)*
                ::grease::types::Type::with_data(::grease::types::grease_type_uuid(concat![module_path!(), "::", #namestr].as_bytes()), type_params.into())
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn grease_trait(_args: TokenStream, item: TokenStream) -> TokenStream {
    if let Ok(trt) = syn::parse::<ItemTrait>(item.clone()) {
        grease_trait_definition(trt)
    } else if let Ok(imp) = syn::parse::<syn::ItemImpl>(item) {
        match grease_trait_impl(imp) {
            Ok(v) => v.2,
            Err(e) => e.to_compile_error(),
        }
        .into()
    } else {
        TokenStream::from(
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "failed to parse as trait definition or implemenation",
            )
            .to_compile_error(),
        )
    }
}

fn impl_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}Impl", name), name.span())
}

fn grease_trait_definition(trt: ItemTrait) -> TokenStream {
    let mut errs = proc_macro2::TokenStream::new();

    let mut funcs = Vec::new();

    for item in trt.items {
        use syn::TraitItem::*;
        match item {
            Const(_) => errs.extend(
                syn::Error::new(item.span(), "constants not supported in grease traits")
                    .to_compile_error(),
            ),
            Type(_) => errs.extend(
                syn::Error::new(item.span(), "types not supported in grease traits")
                    .to_compile_error(),
            ),
            Macro(_) => errs.extend(
                syn::Error::new(item.span(), "macros not supported in grease traits")
                    .to_compile_error(),
            ),
            Verbatim(_) => errs.extend(
                syn::Error::new(item.span(), "unrecognized grease trait item").to_compile_error(),
            ),
            Method(m) => {
                let mut args = Vec::new();
                if m.sig.asyncness.is_none() {
                    errs.extend(
                        syn::Error::new(m.sig.span(), "grease trait functions must be async")
                            .to_compile_error(),
                    );
                }
                let mut inputs = m.sig.inputs.into_iter().peekable();
                let mut has_receiver = false;
                if let Some(syn::FnArg::Receiver(r)) = inputs.peek() {
                    if r.reference.is_none() || r.mutability.is_some() {
                        errs.extend(
                            syn::Error::new(r.span(), "self must be a borrowed reference")
                                .to_compile_error(),
                        );
                    }
                    has_receiver = true;
                }
                if has_receiver {
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

                funcs.push((m.sig.ident, has_receiver, args, ret));
            }
            _ => (),
        }
    }

    let ref_ident = trt.ident;

    // Construct fields for impl struct
    let fields = funcs.iter().map(|(ident, rcvr, args, ret)| {
        let mut fn_args = Vec::new();
        if *rcvr {
            fn_args.push(quote! { &'a ::grease::type_erase::Erased });
        }
        for (_, t) in args {
            let mut ty = t.clone();
            if let syn::Type::Reference(tr) = &mut ty {
                tr.lifetime = Some(parse_quote! { 'a });
            }
            fn_args.push(quote! { #ty });
        }

        let ret = ret.clone();

        syn::Field {
            attrs: Default::default(),
            vis: parse_quote! { pub },
            ident: Some(ident.clone()),
            colon_token: Some(Default::default()),
            ty: parse_quote! {
                ::grease::future::BoxSharedFuture<::grease::error::RResult<
                    grease::closure::FnPtr<
                        for<'a> extern "C" fn(&'a ::grease::runtime::Context, #(#fn_args),*)
                            -> ::grease::future::BoxFuture<'a, ::grease::error::RResult<#ret>>
                    >>>
            },
        }
    });

    let fields = syn::FieldsNamed {
        brace_token: trt.brace_token,
        named: fields.collect(),
    };

    let impl_struct_name = impl_name(&ref_ident);

    let mut impl_attrs = trt.attrs.clone();
    impl_attrs.push(parse_quote! { #[derive(StableAbi)] });
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

    let grease_trait = impl_grease_trait(ref_ident.clone(), trt.generics.clone());

    let ref_generics = trt.generics.clone();
    let inner_generics = ref_generics.clone();

    let ref_generics_params = ref_generics.params.clone();
    let ref_generics_where = ref_generics.where_clause.clone();

    let ref_struct = ItemStruct {
        attrs: Vec::new(),
        vis: trt.vis,
        struct_token: syn::token::Struct {
            span: trt.trait_token.span,
        },
        ident: ref_ident.clone(),
        generics: ref_generics,
        fields: syn::Fields::Named(parse_quote! {
            {
                inner: ::grease::runtime::Trait<#impl_struct_name<#inner_generics>>,
                ctx: ::grease::runtime::Context
            }
        }),
        semi_token: None,
    };

    let ref_struct_impl_methods = funcs.iter().map(|(ident, rcvr, args, ret)| {
        let v = if *rcvr {
            quote! { grease_trait_self_value: ::grease::value::Value }
        } else {
            quote! {}
        };
        let arg_names = args.iter().map(|(name, _)| name);
        let args = args.iter().map(|(name, ty)| quote! { #name: #ty, });
        let call_v = if *rcvr {
            quote! { grease_trait_self_value.await?.as_ref(), }
        } else {
            quote! {}
        };
        quote! {
            pub async fn #ident(&mut self, #v #(#args),*) -> ::grease::error::Result<#ret> {
                (self.inner.as_ref().await?.#ident.clone().await.into_result()?.as_fn())(&self.ctx, #call_v #(#arg_names),*).await.into_result()
            }
        }
    });

    let ref_struct_impl = quote! {
        impl<#ref_generics_params> #ref_ident<#ref_generics_params>
        #ref_generics_where
        {
            #(#ref_struct_impl_methods)*
        }
    };

    TokenStream::from(quote! {
        #errs
        #impl_struct
        #ref_struct
        #ref_struct_impl
        #grease_trait
    })
}

fn impl_grease_trait(name: syn::Ident, generics: syn::Generics) -> proc_macro2::TokenStream {
    let params = generics.params;
    let param_reqs = params.clone().into_iter().map(|mut p| {
        if let syn::GenericParam::Type(ref mut tp) = &mut p {
            tp.bounds.push(parse_quote! {
                    ::grease::types::GreaseType
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
                    type_params.0.push(#ident::grease_type());
                }
            })
        }
        _ => None,
    });
    let namestr = syn::LitStr::new(&format!("{}", name), name.span());

    let where_clause = generics.where_clause;

    let imp_name = impl_name(&name);

    let expanded = quote! {
        impl<#(#param_reqs),*> ::grease::traits::GreaseTrait for #name<#(#param_args),*>
        #where_clause
        {
            type Impl = #imp_name;

            fn grease_trait() -> ::grease::traits::Trait {
                let mut type_params = ::grease::types::TypeParameters::default();
                #(#set_data)*
                ::grease::traits::Trait::with_data(::grease::traits::grease_trait_uuid(concat![module_path!(), "::", #namestr].as_bytes()), type_params.into())
            }

            fn create(inner: ::grease::runtime::Trait<Self::Impl>, ctx: &::grease::runtime::Context) -> Self {
                #name { inner, ctx: ctx.clone() }
            }
        }
    };

    expanded
}

fn grease_trait_impl(
    imp: syn::ItemImpl,
) -> Result<(syn::Type, syn::Type, proc_macro2::TokenStream)> {
    let mut errs = proc_macro2::TokenStream::new();

    let (trait_name, struct_name) = match imp.trait_ {
        None => return Err(syn::Error::new(imp.span(), "no trait for impl")),
        Some((_, mut v, _)) => {
            let orig = v.clone();
            if let Some(seg) = v.segments.last_mut() {
                seg.ident = impl_name(&seg.ident);
            }
            (orig, v)
        }
    };

    let ty = *imp.self_ty;

    let mut funcs = Vec::new();

    for item in imp.items {
        use syn::ImplItem::*;
        match item {
            Const(_) => errs.extend(
                syn::Error::new(item.span(), "constants not supported in grease traits")
                    .to_compile_error(),
            ),
            Type(_) => errs.extend(
                syn::Error::new(item.span(), "types not supported in grease traits")
                    .to_compile_error(),
            ),
            Macro(_) => errs.extend(
                syn::Error::new(item.span(), "macros not supported in grease traits")
                    .to_compile_error(),
            ),
            Verbatim(_) => errs.extend(
                syn::Error::new(item.span(), "unrecognized grease trait item").to_compile_error(),
            ),
            Method(m) => {
                let mut args = Vec::new();
                if m.sig.asyncness.is_none() {
                    errs.extend(
                        syn::Error::new(m.sig.span(), "grease trait functions must be async")
                            .to_compile_error(),
                    );
                }
                let mut inputs = m.sig.inputs.into_iter().peekable();
                let mut has_receiver = false;
                if let Some(syn::FnArg::Receiver(r)) = inputs.peek() {
                    if r.reference.is_none() || r.mutability.is_some() {
                        errs.extend(
                            syn::Error::new(r.span(), "self must be a borrowed reference")
                                .to_compile_error(),
                        );
                    }
                    has_receiver = true;
                }
                if has_receiver {
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

                funcs.push((m.sig.ident, has_receiver, args, ret, m.block));
            }
            _ => (),
        }
    }

    let impl_ty = ty.clone();
    let func_defs = funcs.iter().map(|(name, rcvr, args, ret, block)| {
        let mut fn_args = Vec::new();
        let mut self_bind = None;
        let mut block_tokens = block.into_token_stream();
        if *rcvr {
            fn_args.insert(
                0,
                quote! { grease_trait__erased: &'a ::grease::type_erase::Erased },
            );
            self_bind =
                Some(quote! { let grease_trait__self = unsafe { grease_trait__erased.as_ref::<#ty>() }; });
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
                                Ident(proc_macro2::Ident::new("grease_trait__self", id.span()))
                            } else {
                                Ident(id)
                            }
                        }
                        o => o,
                    })
                    .collect()
            }
            block_tokens = replace_self(block_tokens);
        }
        for (n, t) in args {
            let mut ty = t.clone();
            if let syn::Type::Reference(tr) = &mut ty {
                tr.lifetime = Some(parse_quote! { 'a });
            }
            fn_args.push(quote! { #n: #t });
        }

        let ret = ret.clone();

        quote! {
            extern "C" fn #name<'a>(CONTEXT: &'a ::grease::runtime::Context, #(#fn_args),*)
                -> ::grease::future::BoxFuture<'a, ::grease::error::RResult<#ret>> {
                ::grease::future::BoxFuture::new(async move {
                    #self_bind
                    (move || -> ::grease::error::Result<#ret> {
                        Ok(#block_tokens)
                    })().into()
                })
            }
        }
    });

    let func_sets = funcs.iter().map(|(name, _, _, _, _)| {
        quote! {
            #name: ::grease::future::BoxSharedFuture::new(async { ::grease::error::RResult::ROk(::grease::closure::FnPtr::from_fn(#name as *const _)) })
        }
    });

    Ok((
        parse_quote! { #trait_name },
        impl_ty,
        quote! {
            {
                #errs

                #(#func_defs)*

                #struct_name {
                    #(#func_sets),*
                }
            }
        },
    ))
}

#[proc_macro]
pub fn grease_traits_fn(ts: TokenStream) -> TokenStream {
    fn parse(s: syn::parse::ParseStream) -> Result<TokenStream> {
        if s.is_empty() {
            return Ok(TokenStream::new());
        }
        use syn::parse::discouraged::Speculative;
        let o = s.fork();
        let mut ts = match o.parse::<syn::ItemImpl>() {
            Ok(imp) => {
                s.advance_to(&o);
                let (trt, ty, imp) = grease_trait_impl(imp)?;
                quote! { traits.add_impl::<#trt>(<#ty as ::grease::types::GreaseType>::grease_type(), #imp); }
            }
            Err(_) => s.parse::<proc_macro2::TokenTree>().unwrap().into(),
        };
        ts.extend(proc_macro2::TokenStream::from(parse(s)?));
        Ok(ts.into())
    }

    use syn::parse::Parser;
    let ts = match parse.parse(ts) {
        Ok(ts) => proc_macro2::TokenStream::from(ts),
        Err(e) => return e.to_compile_error().into(),
    };

    let ts = TokenStream::from(quote! {
        pub fn traits(traits: &mut ::grease::runtime::Traits) {
            #ts
        }
    });
    dbg!(ts.to_string());
    ts
}
