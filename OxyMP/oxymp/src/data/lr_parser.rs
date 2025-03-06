#[derive(Debug)]
pub struct LRParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub grammar: Vec<String>,
}
