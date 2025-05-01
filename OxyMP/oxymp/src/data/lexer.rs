use quote::ToTokens;
use syn::spanned::Spanned;

use super::helpers::{get_item_ds_span, TRAILING_TOKENS_ERR};

#[derive(Debug)]
pub struct LexerData {
    pub visibility: proc_macro2::TokenStream,
    pub skip_patterns: Vec<syn::LitStr>,
}

pub(super) fn process_lexer(item: syn::Item) -> syn::Result<(syn::ItemMod, LexerData)> {
    let syn::Item::Mod(mut item) = item else {
        return Err(syn::Error::new(
            get_item_ds_span(&item),
            "Item must be a module.",
        ));
    };

    match &item.content {
        Some((_, items)) if items.is_empty() => Ok(()),
        Some((_, items)) => {
            let span = items.first().expect("has 1 item").span();
            Err((span, "No items allowed in lexer module."))
        }
        None => Err((item.ident.span(), "Missing `{}`.")),
    }
    .map_err(|(span, msg)| {
        let msg = format!("Lexer module must have an empty content block. {}", msg);
        syn::Error::new(span, msg)
    })?;

    let visibility = item.vis.to_token_stream();

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
