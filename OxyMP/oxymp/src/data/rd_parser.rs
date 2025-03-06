#[derive(Debug)]
pub struct RDParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub grammar: Vec<String>,
}
