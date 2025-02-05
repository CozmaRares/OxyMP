use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

pub fn derive_lexer_impl(input: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse2(input)?;
    let data = parse_attributes(&ast.attrs)?;

    eprintln!("{:#?}", data);

    Ok(quote! {})
}

#[derive(Debug)]
struct MacroData {
    skip_patterns: Vec<String>,
    token_enum: syn::Path,
}

fn parse_attributes(attrs: &[syn::Attribute]) -> syn::Result<MacroData> {
    let mut skip_patterns = Vec::new();
    let mut token_enum = None;

    for attr in attrs {
        if attr.path().is_ident("skip") {
            let skip_attr: SkipAttr = attr.parse_args()?;
            skip_patterns.push(skip_attr.0);
        } else if attr.path().is_ident("tokens") {
            if token_enum.is_some() {
                return Err(syn::Error::new(
                    attr.span(),
                    "Multiple token enums specified. Please specify only one.",
                ));
            }
            token_enum = Some(attr.parse_args()?);
        }
    }

    if None == token_enum {
        return Err(syn::Error::new(
            Span::call_site(),
            "No token enum specified. Please specify it with `#[tokens(path::to::Tokens)]`",
        ));
    }

    return Ok(MacroData {
        skip_patterns,
        token_enum: token_enum.unwrap(),
    });
}

struct SkipAttr(String);

impl syn::parse::Parse for SkipAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
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
