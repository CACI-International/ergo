//! Match expression for Value.

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Arm, Expr, Token};

pub fn match_value(ts: TokenStream) -> TokenStream {
    fn parse(s: syn::parse::ParseStream) -> syn::Result<(bool, Expr, Vec<Arm>)> {
        let by_ref = s.parse::<Option<Token![&]>>()?.is_some();
        let e = s.parse::<Expr>()?;
        s.parse::<Token![,]>()?;

        let mut cases = vec![];
        while !s.is_empty() {
            cases.push(s.parse::<Arm>()?);
        }
        Ok((by_ref, e, cases))
    }

    let ts = proc_macro2::TokenStream::from(ts);
    let span = ts.span();
    let ts = ts.into();
    let (by_ref, e, mut cases) = parse_macro_input!(ts with parse);

    let else_case = match cases.pop() {
        None => {
            return syn::parse::Error::new(span, "no else case provided")
                .to_compile_error()
                .into();
        }
        Some(arm) => match &arm.pat {
            syn::Pat::Wild(_) | syn::Pat::Ident(_) => arm,
            p => {
                return syn::parse::Error::new(
                    p.span(),
                    "final case pattern must be a wildcard or identifier",
                )
                .to_compile_error()
                .into();
            }
        },
    };

    fn get_type_cases(
        pat: syn::Pat,
        body: syn::Expr,
    ) -> Result<Vec<(syn::Path, syn::Pat, syn::Expr)>, syn::parse::Error> {
        use syn::Pat::*;
        match pat {
            Path(p) => Ok(vec![(p.path.clone(), Path(p), body)]),
            Struct(p) => Ok(vec![(p.path.clone(), Struct(p), body)]),
            TupleStruct(p) => Ok(vec![(p.path.clone(), TupleStruct(p), body)]),
            Or(p) => Ok(p
                .cases
                .into_iter()
                .map(|p| get_type_cases(p, body.clone()))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect()),
            Ident(p) => {
                if let Some((_,pat)) = &p.subpat {
                    Ok(get_type_cases(pat.as_ref().clone(), body)?
                        .into_iter().map(|(path, _, expr)| (path, Ident(p.clone()), expr)).collect())
                } else {
                    Err(syn::parse::Error::new(
                        p.span(),
                        "only 'path', 'struct', 'tuple struct', 'or', and 'subpattern' patterns are allowed",
                    ))
                }
            }
            other => Err(syn::parse::Error::new(
                other.span(),
                "only 'path', 'struct', 'tuple struct', 'or', and 'subpattern' patterns are allowed",
            )),
        }
    }

    let type_cases = cases
        .into_iter()
        .map(|arm| get_type_cases(arm.pat, *arm.body))
        .collect::<Result<Vec<_>, _>>();
    let type_cases = match type_cases {
        Ok(cases) => cases.into_iter().flatten().collect::<Vec<_>>(),
        Err(e) => return e.to_compile_error().into(),
    };

    let get_bind_val = if by_ref {
        quote! { as_ref }
    } else {
        quote! { to_owned }
    };

    let type_cases = type_cases.into_iter().map(|(tp, bind, body)| {
        quote! {
            #tp => {
                let ergo_match_value_typed = ergo_match_value_e.as_type::<#tp>().unwrap();
                let #bind = ergo_match_value_typed.#get_bind_val();
                #body
            }
        }
    });

    TokenStream::from(quote_spanned! { span=>
        {
            let ergo_match_value_e: ergo_runtime::value::Value = #e;
            match ergo_match_value_e.ergo_type() {
                None => match ergo_match_value_e { #else_case },
                Some(ergo_match_value_tp) => ergo_runtime::match_type!(ergo_match_value_tp => {
                    #(#type_cases ,)*
                    => match ergo_match_value_e { #else_case }
                })
            }
        }
    })
}
