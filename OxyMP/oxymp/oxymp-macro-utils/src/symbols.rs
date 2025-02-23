use quote::{format_ident, quote};

#[derive(PartialEq, Eq, Hash)]
pub enum Symbol {
    Str,

    Result,
    Ok,
    Err,

    Option,
    None,
    Some,

    Box,
    Vec,

    VecMacro,

    UtilLexRule,
    UtilLexerData,
    UtilLexResult,
    UtilLexError,
}

impl From<Symbol> for proc_macro2::TokenStream {
    fn from(symbol: Symbol) -> proc_macro2::TokenStream {
        symbol.to_token_stream()
    }
}

impl Symbol {
    pub fn to_token_stream(&self) -> proc_macro2::TokenStream {
        let path: Vec<&str> = match self {
            Symbol::Result => ["core", "result", "Result"].to_vec(),
            Symbol::Ok => ["core", "result", "Result", "Ok"].to_vec(),
            Symbol::Err => ["core", "result", "Result", "Err"].to_vec(),
            Symbol::Option => ["core", "option", "Option"].to_vec(),
            Symbol::None => ["core", "option", "Option", "None"].to_vec(),
            Symbol::Some => ["core", "option", "Option", "Some"].to_vec(),
            Symbol::Vec => ["std", "vec", "Vec"].to_vec(),
            Symbol::VecMacro => ["std", "vec"].to_vec(),
            Symbol::Box => ["std", "boxed", "Box"].to_vec(),
            Symbol::Str => ["core", "primitive", "str"].to_vec(),
            Symbol::UtilLexRule => ["oxymp_util", "lexer", "LexRule"].to_vec(),
            Symbol::UtilLexerData => ["oxymp_util", "lexer", "LexerData"].to_vec(),
            Symbol::UtilLexResult => ["oxymp_util", "lexer", "LexResult"].to_vec(),
            Symbol::UtilLexError => ["oxymp_util", "lexer", "LexError"].to_vec(),
        };
        let path = path.iter().map(|s| format_ident!("{}", s));
        quote! { ::#(#path)::* }
    }
}
