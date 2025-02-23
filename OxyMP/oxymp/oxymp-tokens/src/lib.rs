#![allow(non_snake_case)]

mod info;

use change_case::snake_case;
use oxymp_macro_utils::{symbols::Symbol, AttributeParseError};
use quote::{format_ident, quote, ToTokens};
use syn::spanned::Spanned;

use info::*;

// TODO: add token debug info

const DERIVE_ATTRIBUTE: &str = "#[derive(Tokens)]";

pub fn derive_tokens_impl(
    input: proc_macro2::TokenStream,
) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse2(input)?;

    let visibility = ast.vis.to_token_stream();

    let mod_ident = ast.ident.to_string();
    let mod_ident = snake_case(&mod_ident);
    let mod_ident = format_ident!("{}", mod_ident);

    let syn::Data::Enum(enum_data) = ast.data else {
        let ds_keyword_span = match ast.data {
            syn::Data::Struct(data_struct) => data_struct.struct_token.span,
            syn::Data::Union(data_union) => data_union.union_token.span,
            syn::Data::Enum(_) => unreachable!(),
        };

        return Err(syn::Error::new(
            ds_keyword_span,
            format!(
                "Tokens can only be derived for enums. Make sure that `{}` is applied to an enum.",
                DERIVE_ATTRIBUTE
            ),
        ));
    };

    if enum_data.variants.is_empty() {
        return Err(syn::Error::new(
            ast.ident.span(),
            format!(
                "The token enum must contain at least one variant. Consider adding a variant and anotating it with `{}` or `{}`.",
                EXACT_TOKEN_FORMAT,
                REGEX_TOKEN_FORMAT,
            )
        ));
    }

    let _Option = Symbol::Option.to_token_stream();
    let _None = Symbol::None.to_token_stream();

    let generated = enum_data
        .variants
        .iter()
        .map(|syn::Variant { ident, fields, .. }| {
            let token_struct = generate_token_struct(ident, fields);
            let check = generate_token_conversion_check(&ast.ident, ident, fields);
            let original_ident = &ast.ident;

            quote! {
                #visibility #token_struct

                impl #ident {
                    #[inline]
                    #visibility fn from_token(token: #original_ident) -> #_Option<Self> {
                        match token {
                            #check
                            _ => #_None,
                        }
                    }
                }
            }
        });

    let info = get_token_info(&enum_data.variants)?;
    let lex_rules = generate_lex_rules(&ast.ident, info);

    Ok(quote! {
        #visibility mod #mod_ident {
            use super::*;

            #(#generated)*
        }

        #lex_rules
    })
}

#[inline]
fn generate_token_struct(ident: &syn::Ident, fields: &syn::Fields) -> proc_macro2::TokenStream {
    match fields {
        syn::Fields::Unnamed(fields) => quote! { struct #ident #fields; },
        syn::Fields::Named(fields) => quote! { struct #ident #fields },
        syn::Fields::Unit => quote! { struct #ident; },
    }
}

fn generate_token_conversion_check(
    tokens_ident: &syn::Ident,
    variant_ident: &syn::Ident,
    variant_fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let _Some = Symbol::Some.to_token_stream();

    let check = match variant_fields {
        syn::Fields::Unnamed(fields_unnamed) => {
            let field_names = fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| format_ident!("field_{}", i));
            let field_names = quote! { #(#field_names),* };

            quote! {
                #tokens_ident::#variant_ident ( #field_names ) => #_Some(Self ( #field_names )),
            }
        }

        syn::Fields::Named(fields_named) => {
            let field_names = fields_named
                .named
                .iter()
                .filter_map(|field| field.ident.clone());
            let field_names = quote! { #(#field_names),* };

            quote! {
                #tokens_ident::#variant_ident { #field_names } => #_Some(Self { #field_names }),
            }
        }

        syn::Fields::Unit => quote! {
            #tokens_ident::#variant_ident => #_Some(Self),
        },
    };

    check
}

#[derive(Debug)]
enum TokenType {
    Exact,
    Regex,
}

enum AttributeError {
    Empty,
    MoreThan1(proc_macro2::Span),
    InvalidAttribute(proc_macro2::Span),
}

#[inline]
fn get_token_type(
    attrs: &[syn::Attribute],
) -> Result<(TokenType, &syn::Attribute), AttributeError> {
    if attrs.is_empty() {
        return Err(AttributeError::Empty);
    } else if attrs.len() > 1 {
        return Err(AttributeError::MoreThan1(attrs[1].path().span()));
    }

    let attr_path = attrs[0].path();

    if attr_path.is_ident("exact") {
        Ok((TokenType::Exact, &attrs[0]))
    } else if attr_path.is_ident("regex") {
        Ok((TokenType::Regex, &attrs[0]))
    } else {
        Err(AttributeError::InvalidAttribute(attr_path.span()))
    }
}

#[inline]
fn ensure_correct_attribute(
    attr_type: &TokenType,
    fields: &syn::Fields,
    ident: &syn::Ident,
) -> syn::Result<()> {
    match (attr_type, fields) {
        (TokenType::Exact, syn::Fields::Unit) => Ok(()),
        (TokenType::Exact, _) => Err(syn::Error::new(
            fields.span(),
            "Exact tokens can't contain any data. Consider removing any associated data for this token variant.",
        )),
        (TokenType::Regex, syn::Fields::Unit) => Err(syn::Error::new(
            ident.span(),
            "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."
        )),
        (TokenType::Regex, syn::Fields::Named(fields_named)) => {
            if fields_named.named.is_empty() {
                Err(syn::Error::new(
                    ident.span(),
                    "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."
                ))
            } else {
                Ok(())
            }
        }
        (TokenType::Regex, syn::Fields::Unnamed(fields_unnamed)) => {
            if fields_unnamed.unnamed.is_empty() {
                Err(syn::Error::new(
                    ident.span(),
                    "Regex tokens must contain some data. Make sure that the variant definition includes encapsulated data."
                ))
            } else {
                Ok(())
            }
        }
    }
}

#[inline]
fn add_correct_format(format: &'static str) -> impl FnOnce(syn::Error) -> syn::Error {
    |err| {
        let span = err.span();
        let reason = err.to_string();

        AttributeParseError::new(span, reason, format).into()
    }
}

fn get_token_info(
    variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> syn::Result<Vec<(String, TokenInfo)>> {
    let mut info = Vec::new();

    for variant in variants {
        let syn::Variant {
            attrs,
            ident,
            fields,
            ..
        } = variant;

        let res = get_token_type(attrs);
        let Ok((token_type, attr)) = res else {
            let err = res.err().unwrap();

            return match err {
                AttributeError::Empty => Err(syn::Error::new(
                    ident.span(),
                    format!(
                        "Tokens can only be derived form variants with attributes. Please annotate the variant with `{}` or `{}`.",
                        EXACT_TOKEN_FORMAT,
                        REGEX_TOKEN_FORMAT,
                    )
                )),
                AttributeError::MoreThan1(span) => {
                    Err(syn::Error::new(span, "Only one attribute is allowed. Consider using only one attribute or creating another variant."))
                }
                AttributeError::InvalidAttribute(span) => {
                    Err(syn::Error::new(span,
                    format!(
                        "The use of this attribute is not allowed. Please annotate the variant with `{}` or `{}`.",
                        EXACT_TOKEN_FORMAT,
                        REGEX_TOKEN_FORMAT,
                    )
                    ))
                }
            };
        };

        ensure_correct_attribute(&token_type, fields, ident)?;

        let token_info = match token_type {
            TokenType::Exact => TokenInfo::Exact(
                attr.parse_args()
                    .map_err(add_correct_format(EXACT_TOKEN_FORMAT))?,
            ),
            TokenType::Regex => TokenInfo::Regex(
                attr.parse_args()
                    .map_err(add_correct_format(REGEX_TOKEN_FORMAT))?,
            ),
        };

        info.push((ident.to_string(), token_info));
    }

    Ok(info)
}

fn generate_lex_rules(
    ident: &syn::Ident,
    info: Vec<(String, TokenInfo)>,
) -> proc_macro2::TokenStream {
    let rules = info
        .into_iter()
        .map(|(variant, info)| generate_rule(ident, variant, info));

    let _Vec = Symbol::Vec.to_token_stream();
    let _LexRule = Symbol::UtilLexRule.to_token_stream();
    let _vec = Symbol::VecMacro.to_token_stream();

    quote! {
        impl #ident {
            fn get_lex_rules() -> #_Vec<#_LexRule<Self>> {
                #_vec![ #(#rules),* ]
            }
        }
    }
}

#[inline]
fn generate_rule(
    token_ident: &syn::Ident,
    token_variant: String,
    token_info: TokenInfo,
) -> proc_macro2::TokenStream {
    let variant = format_ident!("{}", token_variant);

    let _LexRule = Symbol::UtilLexRule.to_token_stream();
    let _Box = Symbol::Box.to_token_stream();

    match token_info {
        TokenInfo::Exact(ExactToken { pattern }) => quote! {
           #_LexRule::new_exact(
               #pattern.to_string(),
               #_Box::new(|| #token_ident::#variant),
           )
        },
        TokenInfo::Regex(RegexToken { regex, transformer }) => quote! {
            #_LexRule::new_regex(
                #regex,
               #_Box::new(
                   |input, matched_size| #transformer(&input[..matched_size])
               )
            )
        },
    }
}
