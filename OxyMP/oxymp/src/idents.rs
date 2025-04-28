use quote::{format_ident, IdentFragment};

pub fn token_struct<A: IdentFragment, B: IdentFragment>(
    tokens_ident: &A,
    variant_ident: &B,
) -> syn::Ident {
    format_ident!("{}{}", tokens_ident, variant_ident)
}

pub fn numeric(num: usize) -> syn::Ident {
    format_ident!("_{}", num)
}
