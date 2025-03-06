use quote::ToTokens;

use super::processor::ItemProcessor;

#[derive(Debug)]
pub struct LexerData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub skip_patterns: Vec<String>,
}

pub struct LexerProcessor;

impl ItemProcessor<LexerData, syn::ItemStruct> for LexerProcessor {
    fn get_target() -> &'static str {
        "Lexer"
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
    ) -> syn::Result<(LexerData, syn::Item)> {
        let visibility = item.vis.to_token_stream();
        let ident = item.ident.clone();

        let mut skip_patterns = Vec::new();
        let mut attributes = Vec::new();

        item.attrs.remove(makrer_attr_idx);

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
            LexerData {
                ident,
                visibility,
                skip_patterns,
            },
            syn::Item::Struct(item),
        ))
    }
}

struct SkipAttr(String);

impl syn::parse::Parse for SkipAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SkipAttr> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected tokens. Please remove them.",
            ));
        }

        Ok(SkipAttr(pattern.value()))
    }
}
