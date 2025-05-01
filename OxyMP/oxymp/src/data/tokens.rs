use proc_macro2::Span;
use quote::ToTokens;
use syn::spanned::Spanned;

use crate::utils::capitalize;

use super::helpers::{get_item_ds_span, AttrError, TRAILING_TOKENS_ERR};

enum TokenError {
    NotEnum(Span),
    NoVariants(Span),
    NoPattern(Span),

    MultiplePatterns {
        variant_span: Span,
        pattern_spans: Vec<Span>,
    },
    Exact(syn::Error),
    Regex(syn::Error),

    ExactData(Span),
    RegexNoData(Span),
    RegexUnnamedNot1(Span),

    AttrSyntax(AttrError),
    Multi(Vec<TokenError>),
}

impl From<AttrError> for TokenError {
    fn from(value: AttrError) -> TokenError {
        TokenError::AttrSyntax(value)
    }
}

impl From<Vec<TokenError>> for TokenError {
    fn from(value: Vec<TokenError>) -> TokenError {
        TokenError::Multi(value)
    }
}

const EXACT_ATTR_FORMAT: &str = r#"#[exact("your exact string")]"#;
const REGEX_ATTR_FORMAT: &str = r#"#[regex("your regex", ::path::to::function)]"#;

#[inline]
fn generate_err(err: syn::Error, correct_format: &str) -> syn::Error {
    let span = err.span();
    let msg = err.to_string();
    let msg = format!("{}. The correct format is '{}'", msg, correct_format);
    let msg = capitalize(msg);
    syn::Error::new(span, msg)
}

impl From<TokenError> for syn::Error {
    fn from(value: TokenError) -> Self {
        match value {
            TokenError::NotEnum(span) => {
                let msg = "Item marked with #[oxymp::Tokens] must be an enum.";
                syn::Error::new(span, msg)
            }
            TokenError::NoVariants(span) => {
                let msg = "The enum has no variants defined. Define at least one variant.";
                syn::Error::new(span, msg)
            }
            TokenError::NoPattern(span) => {
                let msg = format!(
                            "The variant has no pattern defined. Define on of the following attributes:\n\n{}\nor\n\n{}",
                            EXACT_ATTR_FORMAT, REGEX_ATTR_FORMAT
                        );
                syn::Error::new(span, msg)
            }

            TokenError::MultiplePatterns {
                variant_span,
                pattern_spans,
            } => {
                let mut error = Self::new(
                    variant_span,
                    "Variant is makred with multiple pattern attributes.",
                );

                pattern_spans
                    .into_iter()
                    .map(|span| Self::new(span, "pattern attribute used here"))
                    .for_each(|attr_err| error.combine(attr_err));

                error
            }
            TokenError::Exact(err) => generate_err(err, EXACT_ATTR_FORMAT),
            TokenError::Regex(err) => generate_err(err, REGEX_ATTR_FORMAT),

            TokenError::ExactData(span) => {
                let msg = "Exact tokens can't contain any data. Remove any associated data for this token variant.";
                syn::Error::new(span, msg)
            }
            TokenError::RegexNoData(span) => {
                let msg = "Regex tokens must contain some data. Make sure that the variant contains some data.";
                syn::Error::new(span, msg)
            }
            TokenError::RegexUnnamedNot1(span) => {
                let msg = "Regex tokens must only contain one unnamed field.";
                syn::Error::new(span, msg)
            }

            TokenError::AttrSyntax(e) => e.into(),
            TokenError::Multi(errs) => {
                let mut iter = errs.into_iter().map(|err| err.into());
                let first = iter.next().expect("must have at least one error");

                iter.fold(first, |mut acc, err| {
                    acc.combine(err);
                    acc
                })
            }
        }
    }
}

// "The enum has no variants defined. Define at least one variant.",
//
// "Exact tokens can't contain any data. Consider removing any associated data for this token variant.",
// "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."

#[derive(Debug)]
pub enum TokenPattern {
    Exact {
        pattern: syn::LitStr,
    },
    Regex {
        pattern: syn::LitStr,
        transform: syn::Path,
    },
}

impl TokenPattern {
    fn span(&self) -> Span {
        match self {
            TokenPattern::Exact { pattern } => pattern.span(),
            TokenPattern::Regex { pattern, .. } => pattern.span(),
        }
    }
}

impl TokenPattern {
    fn parse_exact(input: syn::parse::ParseStream) -> syn::Result<TokenPattern> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), TRAILING_TOKENS_ERR))?;
        }

        Ok(TokenPattern::Exact { pattern })
    }

    fn parse_regex(input: syn::parse::ParseStream) -> syn::Result<TokenPattern> {
        let regex: syn::LitStr = input.parse()?;
        let _comma: syn::Token![,] = input.parse()?;
        let transform: syn::Path = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), TRAILING_TOKENS_ERR));
        }

        Ok(TokenPattern::Regex {
            pattern: regex,
            transform,
        })
    }

    fn from_attr(attr: &syn::Attribute) -> Result<Option<TokenPattern>, TokenError> {
        let path = attr.path();
        if path.segments.len() != 1 {
            return Ok(None);
        }
        let segment = path.segments.first().unwrap();
        if segment.arguments != syn::PathArguments::None {
            return Err(AttrError::PathArg(segment.span()).into());
        }
        let path_ident = segment.ident.to_string();

        match path_ident.as_str() {
            "exact" => attr
                .parse_args_with(TokenPattern::parse_exact)
                .map(Some)
                .map_err(TokenError::Exact),
            "regex" => attr
                .parse_args_with(TokenPattern::parse_regex)
                .map(Some)
                .map_err(TokenError::Regex),
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct TokenVariant {
    pub ident: syn::Ident,
    pub pattern: TokenPattern,
    pub fields: syn::Fields,
}

#[derive(Debug)]
pub struct TokensData {
    pub visibility: proc_macro2::TokenStream,
    pub ident: syn::Ident,
    pub variants: Vec<TokenVariant>,
}

pub(super) fn process_tokens(item: &mut syn::Item) -> syn::Result<TokensData> {
    process_tokens_impl(item).map_err(|e| e.into())
}

fn process_tokens_impl(item: &mut syn::Item) -> Result<TokensData, TokenError> {
    let syn::Item::Enum(item) = item else {
        return Err(TokenError::NotEnum(get_item_ds_span(item)));
    };

    if item.variants.is_empty() {
        return Err(TokenError::NoVariants(item.ident.span()));
    }

    let mut errors = Vec::new();
    let mut variants = Vec::new();

    for variant in &mut item.variants {
        let mut patterns = Vec::new();

        let mut had_error = false;

        loop {
            match remove_first_pattern_attr(&mut variant.attrs) {
                Err(err) => {
                    errors.push(err);
                    had_error = true;
                    break;
                }
                Ok(None) => {
                    break;
                }
                Ok(Some(pattern)) => patterns.push(pattern),
            };
        }

        if had_error {
            continue;
        }

        match patterns.len() {
            0 => {
                errors.push(TokenError::NoPattern(variant.ident.span()));
                continue;
            }
            1 => {}
            _ => {
                errors.push(TokenError::MultiplePatterns {
                    variant_span: variant.ident.span(),
                    pattern_spans: patterns.iter().map(|pattern| pattern.span()).collect(),
                });
                continue;
            }
        }

        let pattern = patterns.pop().expect("has one pattern");

        if let Some(err) = ensure_correct_fields(&pattern, &variant.fields, &variant.ident) {
            errors.push(err);
            continue;
        }

        variants.push(TokenVariant {
            ident: variant.ident.clone(),
            pattern,
            fields: variant.fields.clone(),
        });
    }

    if !errors.is_empty() {
        return Err(errors.into());
    }

    let visibility = item.vis.to_token_stream();
    let item_ident = item.ident.clone();

    Ok(TokensData {
        ident: item_ident,
        visibility,
        variants,
    })
}

fn remove_first_pattern_attr(
    attrs: &mut Vec<syn::Attribute>,
) -> Result<Option<TokenPattern>, TokenError> {
    for (idx, attr) in attrs.iter().enumerate() {
        let result = TokenPattern::from_attr(&attr)?;

        if let Some(pattern) = result {
            attrs.remove(idx);
            return Ok(Some(pattern));
        }
    }

    Ok(None)
}

fn ensure_correct_fields(
    pattern: &TokenPattern,
    fields: &syn::Fields,
    ident: &syn::Ident,
) -> Option<TokenError> {
    match (&pattern, &fields) {
        (TokenPattern::Exact { .. }, syn::Fields::Unit) => None,

        (TokenPattern::Exact { .. }, _) => Some(TokenError::ExactData(fields.span())),

        (TokenPattern::Regex { .. }, syn::Fields::Unit) => {
            Some(TokenError::RegexNoData(ident.span()))
        }

        (TokenPattern::Regex { .. }, syn::Fields::Named(_)) => {
            Some(TokenError::RegexUnnamedNot1(ident.span()))
        }
        (TokenPattern::Regex { .. }, syn::Fields::Unnamed(fields_unnamed)) => {
            match fields_unnamed.unnamed.len() != 1 {
                false => None,
                true => Some(TokenError::RegexUnnamedNot1(ident.span())),
            }
        }
    }
}
