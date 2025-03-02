use quote::ToTokens;
use syn::spanned::Spanned;

use crate::attributes::*;

pub enum TokenPattern {
    Exact {
        pattern: String,
    },
    Regex {
        pattern: String,
        transform: syn::Path,
    },
}

impl TokenPattern {
    fn parse_exact(input: syn::parse::ParseStream) -> syn::Result<TokenPattern> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected remaining tokens after parsing the attribute. Please consider removing any trailing tokens."
            ));
        }

        Ok(TokenPattern::Exact {
            pattern: pattern.value(),
        })
    }

    fn parse_regex(input: syn::parse::ParseStream) -> syn::Result<TokenPattern> {
        let regex: syn::LitStr = input.parse()?;
        let _comma: syn::Token![,] = input.parse()?;
        let transform: syn::Path = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected remaining tokens after parsing the attribute. Please consider removing any trailing tokens."
            ));
        }

        Ok(TokenPattern::Regex {
            pattern: regex.value(),
            transform,
        })
    }

    fn from_attr(attr: &syn::Attribute) -> syn::Result<Option<TokenPattern>> {
        let path = attr.path();
        if path.segments.len() != 1 {
            return Ok(None);
        }
        let path_ident = path.segments.first().unwrap().ident.to_string();

        match path_ident.as_str() {
            "exact" => attr.parse_args_with(TokenPattern::parse_exact).map(Some),
            "regex" => attr.parse_args_with(TokenPattern::parse_regex).map(Some),
            _ => Ok(None), // ignore attr
        }
    }
}

pub struct TokensData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub patterns: Vec<TokenPattern>,
}

// TODO: initialize these in only in one place
//const TOKENS_MARKER: syn::Attribute = parse_quote!(#[oxymp::Tokens]);

impl TokensData {
    pub fn new(mut item: syn::ItemEnum) -> syn::Result<(Self, syn::ItemEnum)> {
        if item.variants.is_empty() {
            return Err(syn::Error::new(
                item.ident.span(),
                "The enum has no variants defined. Please define at least one variant.",
            ));
        }

        let visibility = item.vis.to_token_stream();
        let ident = item.ident.clone();

        let mut patterns = Vec::new();
        let mut variants = Vec::new();

        for mut variant in item.variants {
            let syn::Variant {
                attrs,
                ident,
                fields,
                ..
            } = variant;

            let mut pattern = None;
            let mut other_attrs = Vec::new();

            // TODO: check fields
            for attr in attrs {
                let parsed_pattern = TokenPattern::from_attr(&attr)?;

                match parsed_pattern {
                    None => other_attrs.push(attr),
                    Some(parsed_pattern) => {
                        if pattern.is_some() {
                            return Err(syn::Error::new(
                                ident.span(),
                                "The variant already has a pattern defined. Consider using only one attribute or creating another variant."
                            ));
                        }

                        pattern = Some(parsed_pattern);
                    }
                }
            }

            let Some(pattern) = pattern else {
                return Err(syn::Error::new(
                    ident.span(),
                    "The variant has no pattern defined. Please define one.",
                ));
            };
            patterns.push(pattern);

            variant.ident = ident;
            variant.attrs = other_attrs;
            variant.fields = fields;
            variants.push(variant);
        }

        item.variants = variants.into_iter().collect();

        Ok((
            TokensData {
                ident,
                visibility,
                patterns,
            },
            item,
        ))
    }
}

pub struct LexerData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub skip_patterns: Vec<String>,
}

impl LexerData {
    pub fn new(item: syn::ItemStruct) -> syn::Result<(Self, syn::ItemStruct)> {
        todo!()
    }
}

pub enum ParserKind {
    #[cfg(feature = "rd")]
    RD,
    #[cfg(feature = "lr")]
    LR,
}

pub struct ParserData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub grammar: Vec<String>,
    pub kind: ParserKind,
}

impl ParserData {
    pub fn new(item: syn::ItemStruct) -> syn::Result<(Self, syn::ItemStruct)> {
        todo!()
    }
}

pub enum AttributeData {
    Tokens(TokensData),
    Lexers(LexerData),
    Parsers(ParserData),
}

pub struct MacroData {
    pub tokens: TokensData,
    pub lexers: Vec<LexerData>,
    pub parsers: Vec<ParserData>,
}

pub fn parse_module(
    items: Vec<syn::Item>,
    mod_ident: &syn::Ident,
) -> syn::Result<(MacroData, Vec<syn::Item>)> {
    let mut other_items: Vec<syn::Item> = Vec::new();
    let mut attribute_targets: Vec<AttributeTarget> = Vec::new();

    for item in items {
        match AttributeTarget::new(item) {
            ItemParseResult::Ok(target) => {
                attribute_targets.push(target);
            }
            ItemParseResult::Ignore(item) => {
                other_items.push(item);
            }
            ItemParseResult::Err(error) => return Err(error),
        }
    }

    let mut token_target: Option<syn::ItemEnum> = None;

    for target in attribute_targets {
        match target {
            AttributeTarget::Tokens(item) => {
                token_target = Some(item);
            }
            _ => {}
        }
    }

    let Some(token_target) = token_target else {
        return Err(syn::Error::new(
            mod_ident.span(),
            "Module marked with #[oxymp] must contain at least one enum marked with #[oxymp::Tokens].",
        ));
    };

    let (tokens_data, token_target) = TokensData::new(token_target)?;

    other_items.push(syn::Item::Enum(token_target));

    Ok((
        MacroData {
            tokens: tokens_data,
            lexers: vec![],
            parsers: vec![],
        },
        other_items,
    ))
}
