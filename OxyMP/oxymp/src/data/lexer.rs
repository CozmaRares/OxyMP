use syn::spanned::Spanned;

use crate::utils::{multiple_attribute_error, FoldErrors};

use super::helpers::{process_module_helper, TRAILING_TOKENS_ERR};

#[derive(Debug)]
pub struct LexerData {
    pub skip_patterns: Vec<syn::LitStr>,
    pub user_error_ident: syn::Ident,
}

const ATTRIBUTES: &[&str] = &["skip", "error"];

pub(super) fn process_lexer(item: syn::Item) -> syn::Result<(syn::ItemMod, LexerData)> {
    let (item, mut found_attrs) = process_module_helper(item, ATTRIBUTES)?;

    let skip_patterns = found_attrs
        .remove("skip")
        .unwrap_or_default()
        .into_iter()
        .map(|(_, toks)| syn::parse2::<SkipAttr>(toks).map(|res| res.0))
        .collect_errors()?;

    let mut error_attrs = found_attrs.remove("error").unwrap_or_default();

    let error_ident = match error_attrs.len() {
        0 => Err(syn::Error::new(
            item.ident.span(),
            "Missing error attribute. A lexer must one error attribute.",
        )),
        1 => {
            let toks = error_attrs.pop().unwrap().1;
            syn::parse2::<ErrorAttr>(toks).map(|res| res.0)
        }
        _ => {
            let mut spans: Vec<_> = error_attrs.iter().map(|(span, _)| *span).collect();
            spans.insert(0, item.ident.span());

            let primary = "Lexer must have exactly one error attribute.";
            let additional = "Error attribute defined here.";

            Err(multiple_attribute_error(spans, primary, additional))
        }
    }?;

    Ok((
        item,
        LexerData {
            skip_patterns,
            user_error_ident: error_ident,
        },
    ))
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

struct ErrorAttr(syn::Ident);

impl syn::parse::Parse for ErrorAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<ErrorAttr> {
        let path: syn::Path = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), TRAILING_TOKENS_ERR));
        }

        if path.segments.len() != 1 {
            let msg = "Error structure must be a single identifier. Make sure it is accessible in the module's scope";
            return Err(syn::Error::new(path.span(), msg));
        }

        let ident = path.segments.first().unwrap().ident.clone();

        Ok(ErrorAttr(ident))
    }
}
