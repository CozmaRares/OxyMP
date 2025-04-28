#[derive(Debug)]
pub struct LRParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: String,
    pub grammar: Vec<String>,
}
