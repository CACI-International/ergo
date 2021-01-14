extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use std::collections::HashSet;
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
    grease_trait_definition(parse_macro_input!(item as _))
}

#[proc_macro]
pub fn grease_trait_impl(item: TokenStream) -> TokenStream {
    match do_grease_trait_impl(parse_macro_input!(item as _)) {
        Ok(v) => v.2,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

fn impl_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}Impl", name), name.span())
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum ReceiverType {
    Owned,
    Reference,
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

fn grease_trait_definition(trt: ItemTrait) -> TokenStream {
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
                let rcvr = if let Some(syn::FnArg::Receiver(r)) = inputs.peek() {
                    if r.reference.is_none() {
                        ReceiverType::Owned
                    } else if r.mutability.is_none() {
                        ReceiverType::Reference
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

                funcs.push((m.sig.ident, rcvr, args, ret));
            }
            _ => (),
        }
    }

    let ref_ident = trt.ident;

    // Construct fields for impl struct
    let fields = funcs.iter().map(|(ident, rcvr, args, ret)| {
        let mut fn_args = Vec::new();
        if rcvr == &ReceiverType::Reference {
            fn_args.push(quote! { &'a ::grease::value::Value });
            fn_args.push(quote! { &'a ::grease::types::Type });
            fn_args.push(quote! { &'a ::grease::type_erase::Erased });
        } else if rcvr == &ReceiverType::Owned {
            fn_args.push(quote! { ::grease::value::Value });
        }
        for (_, t) in args {
            let mut ty = t.clone();
            if let syn::Type::Reference(tr) = &mut ty {
                tr.lifetime = Some(parse_quote! { 'a });
            }
            fn_args.push(quote_spanned! { ty.span() => #ty });
        }

        let ret = if generic_types.contains(&quote! {#ret}.to_string()) {
            parse_quote! { ::grease::value::Value }
        } else {
            ret.clone()
        };

        syn::Field {
            attrs: Default::default(),
            vis: parse_quote! { pub },
            ident: Some(ident.clone()),
            colon_token: Some(Default::default()),
            ty: parse_quote! {
                    grease::closure::FnPtr<
                        for<'a> extern "C" fn(&'a ::grease::runtime::Context, #(#fn_args),*)
                            -> ::grease::future::BoxFuture<'a, ::grease::error::RResult<#ret>>
                    >
            },
        }
    });

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
                inner: ::grease::runtime::Trait<#impl_struct_name #type_generics>,
                ctx: ::grease::runtime::Context
            }
        }),
        semi_token: None,
    };

    let ref_struct_impl_methods = funcs.iter().map(|(ident, rcvr, args, ret)| {
        let v = if rcvr != &ReceiverType::None {
            quote! { mut grease_trait_self_value: ::grease::value::Value, }
        } else {
            quote! {}
        };
        let arg_names = args.iter().map(|(name, _)| name);
        let args = args.iter().map(|(name, ty)| quote! { #name: #ty });
        let call_v = if rcvr == &ReceiverType::Reference {
            quote! { &grease_trait_self_value, grease_trait_self_value.grease_type().await?, grease_trait_self_value.clone().await?.as_ref(), }
        } else if rcvr == &ReceiverType::Owned {
            quote! { grease_trait_self_value, }
        } else {
            quote! {}
        };
        quote! {
            pub async fn #ident(&mut self, #v #(#args),*) -> ::grease::error::Result<#ret> {
                (self.inner.as_ref().await?.#ident.as_fn())(&self.ctx, #call_v #(#arg_names),*).await.into_result()
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
        #grease_trait
    })
}

fn impl_grease_trait(name: syn::Ident, mut generics: syn::Generics) -> proc_macro2::TokenStream {
    for p in generics.params.iter_mut() {
        if let syn::GenericParam::Type(ref mut tp) = p {
            tp.bounds.push(parse_quote! {
                    ::grease::types::GreaseType
            });
        }
    }
    let set_data = generics.params.iter().filter_map(|e| match e {
        syn::GenericParam::Type(tp) => {
            let ident = tp.ident.clone();
            Some(quote! {
                {
                    type_params.0.push(#ident::grease_type());
                }
            })
        }
        _ => None,
    });
    let namestr = syn::LitStr::new(&format!("{}", name), name.span());

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let imp_name = impl_name(&name);

    let expanded = quote! {
        impl #impl_generics ::grease::traits::GreaseTrait for #name #type_generics
        #where_clause
        {
            type Impl = #imp_name #type_generics;

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

fn do_grease_trait_impl(
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
                let rcvr = if let Some(syn::FnArg::Receiver(r)) = inputs.peek() {
                    if r.reference.is_none() {
                        ReceiverType::Owned
                    } else if r.mutability.is_none() {
                        ReceiverType::Reference
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

                funcs.push((m.sig.ident, rcvr, args, ret, m.block));
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
    let func_defs = funcs.iter().map(|(name, rcvr, args, ret, block)| {
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
                            Ident(proc_macro2::Ident::new("grease_trait__self", id.span()))
                        } else {
                            Ident(id)
                        }
                    }
                    o => o,
                })
                .collect()
        }

        if rcvr == &ReceiverType::Reference {
            fn_args.insert(0, quote! { SELF_VALUE: &'a ::grease::value::Value });
            fn_args.insert(
                1,
                quote! { SELF_TYPE: &'a ::grease::types::Type }
            );
            fn_args.insert(
                2,
                quote! { grease_trait__erased: &'a ::grease::type_erase::Erased },
            );
            self_bind = Some(if untyped {
                quote! { let grease_trait__self = grease_trait__erased; }
            } else {
                quote! { let grease_trait__self = unsafe { grease_trait__erased.as_ref::<#ty>() }; }
            });
            block_tokens = replace_self(block_tokens);
        } else if rcvr == &ReceiverType::Owned {
            fn_args.insert(
                0,
                quote! { grease_trait__erased: ::grease::value::Value },
            );
            self_bind = Some(if untyped {
                quote! { let grease_trait__self = grease_trait__erased; }
            } else {
                quote! { let grease_trait__self = grease_trait__erased.typed::<#ty,_,_>(|_| async move { panic!("incorrect type") }).await.unwrap(); }
            });
            block_tokens = replace_self(block_tokens);
        }

        for (n, t) in args {
            let mut ty = t.clone();
            if let syn::Type::Reference(tr) = &mut ty {
                tr.lifetime = Some(parse_quote! { 'a });
            }
            fn_args.push(quote! { #n: #ty });
        }

        let ret = ret.clone();

        quote_spanned! { name.span() =>
            #[allow(improper_ctypes_definitions)]
            extern "C" fn #name #impl_generics(#[allow(non_snake_case,unused)] CONTEXT: &'a ::grease::runtime::Context, #(#fn_args),*)
                -> ::grease::future::BoxFuture<'a, ::grease::error::RResult<#ret>>
            #where_clause
            {
                ::grease::future::BoxFuture::new(async move {
                    #self_bind
                    let res: grease::Result<#ret> = (move || async move { Ok(#block_tokens) })().await;
                    res.into()
                })
            }
        }
    });

    let func_sets = funcs.iter().map(|(name, _, _, _, _)| {
        quote_spanned! { name.span() =>
            #name: unsafe { ::grease::closure::FnPtr::new(#name #type_generics) }
        }
    });

    let field_sets = func_sets.chain(generic_args.iter().enumerate().map(|(n, t)| {
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
                let (trt, ty, imp) = do_grease_trait_impl(imp)?;
                quote! { traits.add_impl_for_type::<#ty, #trt>(#imp); }
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

    TokenStream::from(quote! {
        pub fn traits(traits: &mut ::grease::runtime::Traits) {
            #ts
        }
    })
}
