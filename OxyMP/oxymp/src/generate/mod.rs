mod lexer;
mod tokens;
mod rd_parser;

use crate::data::Data;

pub fn generate(data: Data) -> syn::Result<proc_macro2::TokenStream> {
    let mut module = data.initial_module;
    let (brace, mut items) = module.content.expect("module should have content");

    items.extend(tokens::generate(&data.tokens));
    items.extend(lexer::generate(&data.tokens, data.lexers)?);
    items.extend(rd_parser::generate(&data.tokens, data.rd_parsers)?);

    module.content = Some((brace, items));
    Ok(q! { #module })
}
