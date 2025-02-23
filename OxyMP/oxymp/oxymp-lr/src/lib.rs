use quote::quote;

pub fn derive_lr_impl(input: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse2(input)?;

    eprintln!("{:#?}", ast);

    Ok(quote! {})
}
