#[derive(Debug)]
pub struct GrammarData {
    pub span: proc_macro2::Span,
    pub rule: proc_macro2::TokenStream,
}

