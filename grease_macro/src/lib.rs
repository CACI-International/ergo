extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, LitStr};

/// Create a literal item name.
///
/// Item names must contain only ascii alphanumeric characters.
#[proc_macro_hack]
pub fn item_name(ts: TokenStream) -> TokenStream {
    let input = parse_macro_input!(ts as LitStr);

    let s = input.value();
    if !s.chars().all(|c| c.is_ascii_alphanumeric()) {
        quote_spanned! {
            input.span() => {
                compile_error!("literals may only have ascii alphanumeric characters");
            }
        }
    } else {
        quote! {
            unsafe { &*(#s as *const str as *const ::grease::ItemName) }
        }
    }
    .into()
}
