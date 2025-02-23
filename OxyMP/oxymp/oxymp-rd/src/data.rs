use change_case::snake_case;
use proc_macro2::Span;
use quote::{format_ident, ToTokens};
use syn::spanned::Spanned;

#[derive(Debug)]
pub struct MacroData {
    pub parser_ident: proc_macro2::Ident,
    pub visibility: proc_macro2::TokenStream,
    pub tokens_enum: syn::Ident,
    pub tokens_mod: syn::Ident,
    pub grammar_rules: Vec<String>,
}

pub fn parse_attributes(ast: &syn::DeriveInput) -> syn::Result<MacroData> {
    let attrs = &ast.attrs;

    let mut grammar_rules = Vec::new();
    let mut tokens_enum = None;

    for attr in attrs {
        if attr.path().is_ident("grammar") {
            let grammar_attr: GrammarAttr = attr.parse_args()?;
            grammar_rules.push(grammar_attr.0);
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

    let tokens_enum: syn::Ident = match tokens_enum {
        Some(v) => v,
        None => {
            return Err(syn::Error::new(
                Span::call_site(),
                "No token enum specified. Please specify it with `#[tokens(path::to::Tokens)]`",
            ))
        }
    };

    let tokens_mod = tokens_enum.to_string();
    let tokens_mod = snake_case(&tokens_mod);
    let tokens_mod = format_ident!("{}", tokens_mod);

    Ok(MacroData {
        parser_ident: ast.ident.clone(),
        visibility: ast.vis.to_token_stream(),
        tokens_enum,
        tokens_mod,
        grammar_rules,
    })
}

struct GrammarAttr(String);

impl syn::parse::Parse for GrammarAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<GrammarAttr> {
        let rule: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected tokens. Please remove them.",
            ));
        }

        Ok(GrammarAttr(rule.value()))
    }
}
