mod lexer;
mod processor;
mod tokens;

#[cfg(feature = "rd")]
mod rd_parser;

#[cfg(feature = "lr")]
mod lr_parser;

use processor::{process_item, ItemProcessResult, ItemProcessor};

use syn::spanned::Spanned;
pub use tokens::*;

pub use lexer::*;

#[cfg(feature = "rd")]
pub use rd_parser::*;

#[cfg(feature = "lr")]
pub use lr_parser::*;

use crate::utils::{get_item_attrs, has_attr_starting_with, pretty_print_attr_path};

#[derive(Debug)]
pub struct MacroData {
    pub tokens: TokensData,
    pub lexers: Vec<LexerData>,

    #[cfg(feature = "rd")]
    pub rd_parsers: Vec<RDParserData>,

    #[cfg(feature = "lr")]
    pub lr_parsers: Vec<LRParserData>,
}

pub fn process_module(
    items: Vec<syn::Item>,
    mod_ident: &syn::Ident,
) -> syn::Result<(MacroData, Vec<syn::Item>)> {
    let mut final_items: Vec<syn::Item> = Vec::new();
    let mut token_data: Option<TokensData> = None;
    let mut lexer_data: Vec<LexerData> = Vec::new();

    #[cfg(feature = "rd")]
    let mut rd_parser_data: Vec<RDParserData> = Vec::new();

    #[cfg(feature = "lr")]
    let mut lr_parser_data: Vec<LRParserData> = Vec::new();

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
                            "Module marked with #[oxymp] must contain at most one {} marked with #[oxymp::{}].",
                            TokensProcessor::get_expected_variant(),
                            TokensProcessor::get_target()
                        )
                    ));
                }

                final_items.push(modified_item);
                continue;
            }
            ItemProcessResult::Err(error) => return Err(error),
        }

        let res = process_item(LexerProcessor, &item);
        match res {
            ItemProcessResult::Ignore => {}
            ItemProcessResult::Ok(data, modified_item) => {
                lexer_data.push(data);
                final_items.push(modified_item);
                continue;
            }
            ItemProcessResult::Err(error) => return Err(error),
        }

        let Some(attrs) = get_item_attrs(&item) else {
            continue;
        };

        if let Some(attr) = has_attr_starting_with(attrs, "oxymp") {
            return Err(syn::Error::new(
                attr.span(),
                format!(
                    "Attribute #[{}], containing 'oxymp', is not supported. Make sure you spelled it correctly, or have enabled the corresponding feature.",
                    pretty_print_attr_path(attr.path())
                )
            ));
        }
    }

    if token_data.is_none() {
        return Err(syn::Error::new(
            mod_ident.span(),
            format!(
                "Module marked with #[oxymp] must contain at least one {} marked with #[oxymp::{}].",
                TokensProcessor::get_expected_variant(),
                TokensProcessor::get_target()
            )
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

            #[cfg(feature = "rd")]
            rd_parsers: rd_parser_data,

            #[cfg(feature = "lr")]
            lr_parsers: lr_parser_data,
        },
        final_items,
    ))
}
