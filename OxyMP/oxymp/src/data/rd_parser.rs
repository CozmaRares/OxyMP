use quote::ToTokens;

use super::{grammar::GrammarData, helpers::process_module};

const ATTRIBUTES: &[&str] = &["grammar"];

#[derive(Debug)]
pub struct RDParserData {
    pub visibility: proc_macro2::TokenStream,
    pub grammar_rules: Vec<GrammarData>,
}

pub(super) fn process_rd_parser(item: syn::Item) -> syn::Result<(syn::ItemMod, RDParserData)> {
    let (item, mut found_attrs) = process_module(item, ATTRIBUTES)?;

    let grammar_attrs = found_attrs
        .remove("grammar")
        .expect("grammar attr should be set");

    let grammar_rules = grammar_attrs
        .into_iter()
        .map(|(span, rule)| GrammarData { span, rule })
        .collect();

    let visibility = item.vis.to_token_stream();

    Ok((
        item,
        RDParserData {
            visibility,
            grammar_rules,
        },
    ))
}
