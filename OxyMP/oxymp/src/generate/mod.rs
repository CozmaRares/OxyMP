use crate::data::Data;

pub fn generate(data: Data) -> syn::Result<proc_macro2::TokenStream> {
    let module = data.initial_module;

    Ok(q! { #module })
}
