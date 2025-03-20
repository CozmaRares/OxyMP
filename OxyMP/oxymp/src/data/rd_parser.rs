use quote::ToTokens;
use syn::spanned::Spanned;

use super::processor::ItemProcessor;

#[derive(Debug)]
pub struct RDParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: String,
    pub grammar_rules: Vec<proc_macro2::TokenStream>,
}

pub(super) struct RDParserProcessor;

impl ItemProcessor<RDParserData, syn::ItemStruct> for RDParserProcessor {
    fn get_target() -> &'static str {
        "RDParser"
    }

    fn get_expected_variant() -> &'static str {
        "struct"
    }

    fn get_variant(item: &syn::Item) -> Option<syn::ItemStruct> {
        match item {
            syn::Item::Struct(item) => Some(item.clone()),
            _ => None,
        }
    }

    fn extract_data(
        mut item: syn::ItemStruct,
        makrer_attr_idx: usize,
    ) -> syn::Result<(RDParserData, syn::Item)> {
        let visibility = item.vis.to_token_stream();
        let ident = item.ident.clone();

        let mut grammar_rules = Vec::new();
        let mut attributes = Vec::new();

        item.attrs.remove(makrer_attr_idx);

        for attr in item.attrs {
            let path = attr.path();

            if path.is_ident("grammar") {
                let grammar_attr = parse_grammar_attr(attr)?;
                grammar_rules.push(grammar_attr);
            } else {
                attributes.push(attr);
            }
        }

        item.attrs = attributes;

        Ok((
            RDParserData {
                ident: ident.to_string(),
                visibility,
                grammar_rules,
            },
            syn::Item::Struct(item),
        ))
    }
}

fn parse_grammar_attr(attr: syn::Attribute) -> syn::Result<proc_macro2::TokenStream> {
    match attr.meta {
        syn::Meta::List(meta_list) => Ok(meta_list.tokens),
        _ => Err(syn::Error::new(
            attr.span(),
            "Invalid grammar attribute. Please specify it as `#[grammar(rule)]`",
        )),
    }
}
