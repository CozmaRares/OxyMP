use crate::utils::FoldErrors;

use super::helpers::{process_module_helper, TRAILING_TOKENS_ERR};

#[derive(Debug)]
pub struct LexerData {
    pub skip_patterns: Vec<syn::LitStr>,
}

const ATTRIBUTES: &[&str] = &["skip"];

pub(super) fn process_lexer(item: syn::Item) -> syn::Result<(syn::ItemMod, LexerData)> {
    let (item, mut found_attrs) = process_module_helper(item, ATTRIBUTES)?;

    let skip_patterns = found_attrs
        .remove("skip")
        .unwrap_or_default()
        .into_iter()
        .map(|(_, toks)| syn::parse2::<SkipAttr>(toks).map(|res| res.0))
        .collect_errors()?;

    Ok((item, LexerData { skip_patterns }))
}

struct SkipAttr(syn::LitStr);

impl syn::parse::Parse for SkipAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SkipAttr> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            let msg = format!("{}.", TRAILING_TOKENS_ERR);
            return Err(syn::Error::new(input.span(), msg));
        }

        Ok(SkipAttr(pattern))
    }
}
