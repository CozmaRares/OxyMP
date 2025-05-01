use quote::ToTokens;

use super::helpers::{get_item_ds_span, TRAILING_TOKENS_ERR};

#[derive(Debug)]
pub struct LexerData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub skip_patterns: Vec<syn::LitStr>,
}

pub(super) fn process_lexer(item: syn::Item) -> syn::Result<(syn::ItemMod, LexerData)> {
    let syn::Item::Mod(mut item) = item else {
        return Err(syn::Error::new(
            get_item_ds_span(&item),
            "Item must be a module.",
        ));
    };

    let visibility = item.vis.to_token_stream();
    let ident = item.ident.clone();

    let mut skip_patterns = Vec::new();
    let mut attributes = Vec::new();

    for attr in item.attrs {
        let path = attr.path();

        if path.is_ident("skip") {
            let skip_attr: SkipAttr = attr.parse_args()?;
            skip_patterns.push(skip_attr.0);
        } else {
            attributes.push(attr);
        }
    }

    item.attrs = attributes;

    Ok((
        item,
        LexerData {
            ident,
            visibility,
            skip_patterns,
        },
    ))
}

struct SkipAttr(syn::LitStr);

impl syn::parse::Parse for SkipAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SkipAttr> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), TRAILING_TOKENS_ERR));
        }

        Ok(SkipAttr(pattern))
    }
}
