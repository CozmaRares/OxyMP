#[derive(Debug)]
pub struct RDParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: String,
    pub grammar: Vec<String>,
}
