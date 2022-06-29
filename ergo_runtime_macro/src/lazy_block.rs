use quote::quote;

/// A lazy block which can provide an identity, captures, etc.
#[derive(Default)]
pub struct LazyBlock {
    pub id: proc_macro2::TokenStream,
    pub use_captures: proc_macro2::TokenStream,
    pub contains: Captures,
    pub doc: String,
    pub body: proc_macro2::TokenStream,
}

#[derive(Default)]
pub struct Captures {
    contains: Vec<syn::Ident>,
}

impl Captures {
    /// Returns a tuple with (capture binding name, capture creation statement, capture binding statement).
    pub fn into_components(
        self,
    ) -> (
        proc_macro2::TokenStream,
        proc_macro2::TokenStream,
        proc_macro2::TokenStream,
    ) {
        let name = quote! { __lazy_captures };
        let contains = self.contains;
        let group = quote! {
            let (#name, __lazy_captures_witness) = ergo_runtime::value::lazy::CapturesWitness::witness((#(#contains,)*));
        };
        let ungroup = quote! {
            let (#(#contains,)*) = __lazy_captures_witness.check(#name);
        };
        (name, group, ungroup)
    }
}

impl syn::parse::Parse for LazyBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut ret = LazyBlock::default();
        let mut overwrite_depends = false;
        let mut eval_for_id = false;
        let mut id = proc_macro2::TokenStream::default();
        let mut depends = proc_macro2::TokenStream::default();

        let mut doc_strip_leading_ws = None;
        let mut doc_str = String::new();

        for attr in syn::Attribute::parse_inner(input)? {
            if attr.path.is_ident("doc") {
                match attr.parse_meta()? {
                    syn::Meta::NameValue(syn::MetaNameValue {
                        lit: syn::Lit::Str(s),
                        ..
                    }) => {
                        let s = s.value();
                        match (doc_strip_leading_ws, s.find(|c: char| !c.is_whitespace())) {
                            (None, None) => (),
                            (None, Some(offset)) => {
                                doc_strip_leading_ws = Some(offset);
                                doc_str.push_str(unsafe { s.get_unchecked(offset..) });
                            }
                            (Some(_), None) => doc_str.push('\n'),
                            (Some(a), Some(b)) => {
                                doc_str.push('\n');
                                doc_str.push_str(unsafe { s.get_unchecked(std::cmp::min(a, b)..) });
                            }
                        }
                    }
                    _ => (),
                }
            } else if attr.path.is_ident("contains") {
                ret.contains.contains.extend(attr.parse_args_with(
                    syn::punctuated::Punctuated::<syn::Ident, syn::token::Comma>::parse_terminated,
                )?);
            } else if attr.path.is_ident("depends") {
                depends = attr.parse_args()?;
            } else if attr.path.is_ident("id") {
                id = attr.parse_args()?;
                overwrite_depends = true;
            } else if attr.path.is_ident("exclude_contains_from_id") {
                overwrite_depends = true;
            } else if attr.path.is_ident("eval_for_id") {
                eval_for_id = true;
            } else {
                return Err(syn::Error::new_spanned(attr, "unsupported inner attribute"));
            }
        }

        ret.body = input.parse()?;

        ret.doc = doc_str.trim().to_owned();

        ret.use_captures = if overwrite_depends {
            quote! { false }
        } else {
            quote! { true }
        };

        let id = if id.is_empty() {
            if depends.is_empty() {
                quote! { 0 }
            } else {
                quote! { ergo_runtime::depends![#depends] }
            }
        } else {
            id
        };
        let id = if eval_for_id {
            quote! { ergo_runtime::value::IdInfo::new(#id).eval_for_id(true) }
        } else {
            id
        };
        ret.id = id;

        Ok(ret)
    }
}
