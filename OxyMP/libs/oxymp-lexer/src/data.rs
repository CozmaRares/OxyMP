use proc_macro2::Span;
use quote::ToTokens;
use syn::spanned::Spanned;

#[derive(Debug)]
pub struct MacroData {
    pub visibility: proc_macro2::TokenStream,
    pub skip_patterns: Vec<String>,
    pub tokens_enum: syn::Ident,
}

pub fn parse_attributes(ast: &syn::DeriveInput) -> syn::Result<MacroData> {
    let attrs = &ast.attrs;

    let mut skip_patterns = Vec::new();
    let mut tokens_enum = None;

    for attr in attrs {
        if attr.path().is_ident("skip") {
            let skip_attr: SkipAttr = attr.parse_args()?;
            skip_patterns.push(skip_attr.0);
        } else if attr.path().is_ident("tokens") {
            if tokens_enum.is_some() {
                return Err(syn::Error::new(
                    attr.span(),
                    "Multiple token enums specified. Please specify only one.",
                ));
            }
            tokens_enum = Some(attr.parse_args()?);
        }
    }

    let tokens_enum = match tokens_enum {
        Some(v) => v,
        None => {
            return Err(syn::Error::new(
                Span::call_site(),
                "No token enum specified. Please specify it with `#[tokens(path::to::Tokens)]`",
            ))
        }
    };

    return Ok(MacroData {
        skip_patterns,
        tokens_enum,
        visibility: ast.vis.to_token_stream(),
    });
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
