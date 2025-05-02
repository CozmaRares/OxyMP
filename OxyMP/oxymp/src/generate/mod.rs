mod lexer;
mod tokens;

#[cfg(feature = "rd")]
mod rd_parser;

use crate::data::Data;

pub fn generate(data: Data) -> syn::Result<proc_macro2::TokenStream> {
    let mut module = data.initial_module;
    let Some((brace, mut items)) = module.content else {
        unreachable!("module without content")
    };

    items.extend(tokens::generate(&data.tokens));
    items.extend(lexer::generate(&data.tokens, data.lexers)?);

    #[cfg(feature = "rd")]
    items.extend(rd_parser::generate(&data.tokens, data.rd_parsers)?);

    module.content = Some((brace, items));
    Ok(q! { #module })
}
