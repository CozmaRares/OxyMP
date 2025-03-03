mod processor;
mod tokens;

use processor::{process_item, ItemProcessResult, ItemProcessor};
use syn::spanned::Spanned;
pub use tokens::*;

pub struct LexerData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub skip_patterns: Vec<String>,
}

pub enum ParserKind {
    #[cfg(feature = "rd")]
    RD,
    #[cfg(feature = "lr")]
    LR,
}

pub struct ParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub grammar: Vec<String>,
    pub kind: ParserKind,
}

pub enum AttributeData {
    Tokens(TokensData),
    Lexers(LexerData),
    Parsers(ParserData),
}

pub struct MacroData {
    pub tokens: TokensData,
    pub lexers: Vec<LexerData>,
    pub parsers: Vec<ParserData>,
}

pub fn parse_module(
    items: Vec<syn::Item>,
    mod_ident: &syn::Ident,
) -> syn::Result<(MacroData, Vec<syn::Item>)> {
    let mut modified_items: Vec<syn::Item> = Vec::new();
    let mut token_data: Option<TokensData> = None;
    let mut lexer_data: Vec<LexerData> = Vec::new();
    let mut parser_data: Vec<ParserData> = Vec::new();

    for item in items {
        let res = process_item(TokensProcessor, &item);

        match res {
            ItemProcessResult::Ignore => {}
            ItemProcessResult::Ok(data, modified_item) => {
                if token_data.is_none() {
                    token_data = Some(data);
                } else {
                    return Err(syn::Error::new(
                        mod_ident.span(),
                        format!(
                            "Module marked with #[oxymp] must contain at most one enum marked with #[oxymp::{}].",
                            TokensProcessor::get_target()
                        )
                    ));
                }

                modified_items.push(modified_item);
                continue;
            }
            ItemProcessResult::Err(error) => return Err(error),
        }
    }

    if token_data.is_none() {
        return Err(syn::Error::new(
            mod_ident.span(),
            "Module marked with #[oxymp] must contain at least one enum marked with #[oxymp::Tokens].",
        ));
    }

    //let mut token_target: Option<syn::ItemEnum> = None;
    //
    //for target in attribute_targets {
    //    match target {
    //        AttributeTarget::Tokens(item) => {
    //            token_target = Some(item);
    //        }
    //        _ => {}
    //    }
    //}
    //
    //let Some(token_target) = token_target else {
    //    return Err(syn::Error::new(
    //        mod_ident.span(),
    //        "Module marked with #[oxymp] must contain at least one enum marked with #[oxymp::Tokens].",
    //    ));
    //};
    //
    //other_items.push(syn::Item::Enum(token_target));

    Ok((
        MacroData {
            tokens: token_data.unwrap(),
            lexers: lexer_data,
            parsers: parser_data,
        },
        modified_items,
    ))
}
