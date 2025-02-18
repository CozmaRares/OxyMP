mod data;

use quote::quote;

use data::parse_attributes;

pub fn derive_rd_impl(input: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse2(input)?;
    let data = parse_attributes(&ast)?;

    eprintln!("{:#?}", data);

    Ok(quote! {})
}
