use quote::ToTokens;
use syn::spanned::Spanned;

use super::processor::ItemProcessor;
use crate::utils::capitalize;

#[derive(Debug)]
pub enum TokenPattern {
    Exact {
        pattern: String,
    },
    Regex {
        pattern: String,
        transform: syn::Path,
    },
}

#[inline]
fn generate_err(err: syn::Error, correct_format: &str) -> syn::Error {
    let span = err.span();
    let msg = err.to_string();
    let msg = format!("{}. The correct format is '{}'", msg, correct_format);
    let msg = capitalize(msg);
    syn::Error::new(span, msg)
}

fn exact_err(err: syn::Error) -> syn::Error {
    const FORMAT: &str = r#"#[exact("your exact string")]"#;
    generate_err(err, FORMAT)
}

fn regex_err(err: syn::Error) -> syn::Error {
    const FORMAT: &str = r#"#[regex("your regex", ::path::to::function)]"#;
    generate_err(err, FORMAT)
}

impl TokenPattern {
    fn parse_exact(
        input: syn::parse::ParseStream,
    ) -> syn::Result<(TokenPattern, proc_macro2::Span)> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected remaining tokens after parsing the attribute. Please consider removing any trailing tokens"
            ))?;
        }

        Ok((
            TokenPattern::Exact {
                pattern: pattern.value(),
            },
            pattern.span(),
        ))
    }

    fn parse_regex(
        input: syn::parse::ParseStream,
    ) -> syn::Result<(TokenPattern, proc_macro2::Span)> {
        let regex: syn::LitStr = input.parse()?;
        let _comma: syn::Token![,] = input.parse()?;
        let transform: syn::Path = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected remaining tokens after parsing the attribute. Please consider removing any trailing tokens"
            ));
        }

        Ok((
            TokenPattern::Regex {
                pattern: regex.value(),
                transform,
            },
            regex.span(),
        ))
    }

    fn from_attr(attr: &syn::Attribute) -> syn::Result<Option<(TokenPattern, proc_macro2::Span)>> {
        let path = attr.path();
        if path.segments.len() != 1 {
            return Ok(None);
        }
        let path_ident = path.segments.first().unwrap().ident.to_string();

        match path_ident.as_str() {
            "exact" => attr
                .parse_args_with(TokenPattern::parse_exact)
                .map(Some)
                .map_err(exact_err),
            "regex" => attr
                .parse_args_with(TokenPattern::parse_regex)
                .map(Some)
                .map_err(regex_err),
            _ => Ok(None), // ignore attr
        }
    }
}

#[derive(Debug)]
pub struct TokenVariant {
    pub ident: syn::Ident,
    pub pattern: TokenPattern,
    pub pattern_span: proc_macro2::Span,
    pub fields: syn::Fields,
}

#[derive(Debug)]
pub struct TokensData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub variants: Vec<TokenVariant>,
}

pub(super) struct TokensProcessor;

impl ItemProcessor<TokensData, syn::ItemEnum> for TokensProcessor {
    fn get_target() -> &'static str {
        "Tokens"
    }

    fn get_expected_variant() -> &'static str {
        "enum"
    }

    fn get_variant(item: &syn::Item) -> Option<syn::ItemEnum> {
        match item {
            syn::Item::Enum(item) => Some(item.clone()),
            _ => None,
        }
    }

    fn extract_data(
        mut item: syn::ItemEnum,
        makrer_attr_idx: usize,
    ) -> syn::Result<(TokensData, syn::Item)> {
        if item.variants.is_empty() {
            return Err(syn::Error::new(
                item.ident.span(),
                "The enum has no variants defined. Please define at least one variant.",
            ));
        }

        let visibility = item.vis.to_token_stream();
        let item_ident = item.ident.clone();

        let mut variants = Vec::new();
        let mut modified_variants = Vec::new();

        for mut variant in item.variants {
            let syn::Variant {
                attrs,
                ident,
                fields,
                ..
            } = variant;

            let mut pattern = None;
            let mut pattern_span = None;
            let mut other_attrs = Vec::new();

            for attr in attrs {
                let result = TokenPattern::from_attr(&attr)?;

                match result {
                    None => other_attrs.push(attr),
                    Some((parsed_pattern, span)) => {
                        if pattern.is_some() {
                            return Err(syn::Error::new(
                                ident.span(),
                                "The variant already has a pattern defined. Consider using only one attribute or creating another variant."
                            ));
                        }

                        pattern = Some(parsed_pattern);
                        pattern_span = Some(span);
                    }
                }
            }

            let Some(pattern) = pattern else {
                return Err(syn::Error::new(
                    ident.span(),
                    "The variant has no pattern defined. Please define one.",
                ));
            };

            match (&pattern, &fields) {
                (TokenPattern::Exact{..}, syn::Fields::Unit) => Ok(()),
                (TokenPattern::Exact{..}, _) => Err(syn::Error::new(
                    fields.span(),
                    "Exact tokens can't contain any data. Consider removing any associated data for this token variant.",
                )),
                (TokenPattern::Regex{..}, syn::Fields::Unit) => Err(syn::Error::new(
                    ident.span(),
                    "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."
                )),
                (TokenPattern::Regex{..}, syn::Fields::Named(fields_named)) => {
                    if fields_named.named.is_empty() {
                        Err(syn::Error::new(
                            ident.span(),
                            "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."
                        ))
                    } else {
                        Ok(())
                    }
                }
                (TokenPattern::Regex{..}, syn::Fields::Unnamed(fields_unnamed)) => {
                    if fields_unnamed.unnamed.is_empty() {
                        Err(syn::Error::new(
                            ident.span(),
                            "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."
                        ))
                    } else {
                        Ok(())
                    }
                }
            }?;

            variants.push(TokenVariant {
                ident: ident.clone(),
                pattern,
                pattern_span: pattern_span.expect("pattern_span should always be Some"),
                fields: fields.clone(),
            });

            variant.ident = ident;
            variant.attrs = other_attrs;
            variant.fields = fields;
            modified_variants.push(variant);
        }

        item.variants = modified_variants.into_iter().collect();
        item.attrs.remove(makrer_attr_idx);

        Ok((
            TokensData {
                ident: item_ident,
                visibility,
                variants,
            },
            syn::Item::Enum(item),
        ))
    }
}
