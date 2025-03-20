use quote::{format_ident, quote, ToTokens};
use syn::parse_quote;

use crate::data::tokens::{TokenVariant, TokensData};

pub fn generate_structs(data: &TokensData) -> impl Iterator<Item = syn::Item> + '_ {
    data.variants
        .iter()
        .map(|variant| generate_struct(&data.ident, &data.visibility, variant))
        .map(syn::Item::Struct)
}

fn generate_struct(
    tokens_ident: &String,
    visibility: &proc_macro2::TokenStream,
    variant: &TokenVariant,
) -> syn::ItemStruct {
    let TokenVariant { ident, fields, .. } = variant;
    let fields_stream = fields.to_token_stream();

    let ident = format_ident!("{}{}", tokens_ident, ident);

    let trailing_semi = match fields {
        syn::Fields::Unit => quote! { ; },
        _ => quote! {},
    };

    parse_quote! {
        #visibility struct #ident #fields_stream #trailing_semi
    }
}
