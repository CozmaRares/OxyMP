mod info;

use change_case::snake_case;
use quote::{format_ident, quote, ToTokens};
use syn::spanned::Spanned;

use info::*;

// TODO: add user guidance for error messages
// TODO: add token debug info

pub fn derive_tokens_impl(
    input: proc_macro2::TokenStream,
) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse2(input)?;

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
            "Tokens can only be derived for enums. Make sure that #[derive(Tokens)] is applied to an enum.",
        ));
    };

    let generated = enum_data
        .variants
        .iter()
        .map(|syn::Variant { ident, fields, .. }| {
            let token_struct = generate_token_struct(ident, fields);
            let token_struct_impl = generate_token_struct_impl(&ast.ident, ident, fields);

            quote! {
                #token_struct
                #token_struct_impl
            }
        });

    let info = get_token_info(&enum_data.variants)?;
    let lex_rules = generate_lex_rules(&ast.ident, info);

    Ok(quote! {
        mod #mod_ident {
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

fn generate_token_struct_impl(
    original_ident: &syn::Ident,
    ident: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let check = match fields {
        syn::Fields::Unnamed(fields_unnamed) => {
            let field_names = fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| format_ident!("field_{}", i));
            let field_names = quote! { #(#field_names),* };

            quote! {
                #original_ident::#ident ( #field_names ) => ::std::option::Option::Some(Self ( #field_names )),
            }
        }

        syn::Fields::Named(fields_named) => {
            let field_names = fields_named
                .named
                .iter()
                .filter_map(|field| field.ident.clone());
            let field_names = quote! { #(#field_names),* };

            quote! {
                #original_ident::#ident { #field_names } => ::std::option::Option::Some(Self { #field_names }),
            }
        }

        syn::Fields::Unit => quote! {
            #original_ident::#ident => ::std::option::Option::Some(Self),
        },
    };

    quote! {
        impl #ident {
            #[inline]
            fn from_token(token: #original_ident) -> ::std::option::Option<Self> {
                match token {
                    #check
                    _ => ::std::option::Option::None,
                }
            }
        }
    }
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
        (TokenType::Regex, _) => Ok(()),
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
                    "Tokens can only be derived form variants with attributes. Please annotate the variant with `#[exact(\"...\")]` or `#[regex(\"...\", transform = ...)]`.",
                )),
                AttributeError::MoreThan1(span) => {
                    Err(syn::Error::new(span, "Only one attribute is allowed. Consider using only one attribute or creating another variant."))
                }
                AttributeError::InvalidAttribute(span) => {
                    Err(syn::Error::new(span, "This attribute is not handled. This is a bug. Please report it."))
                }
            };
        };

        ensure_correct_attribute(&token_type, fields, ident)?;

        let token_info: TokenInfo = syn::parse2(attr.to_token_stream())?;

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

    quote! {
        impl #ident {
            fn make_lex_rules() -> Vec<::oxymp_util::lexer::LexRule<Self>> {
                vec![ #(#rules),* ]
            }
        }
    }
}

fn generate_rule(
    token_ident: &syn::Ident,
    token_variant: String,
    token_info: TokenInfo,
) -> proc_macro2::TokenStream {
    let variant = format_ident!("{}", token_variant);

    match token_info {
        TokenInfo::Exact(ExactToken { pattern }) => quote! {
           ::oxymp_util::lexer::LexRule::new_exact(
               #pattern.to_string(),
               std::boxed::Box::new(|| #token_ident::#variant),
           )
        },
        TokenInfo::Regex(RegexToken { regex, transformer }) => quote! {
            ::oxymp_util::lexer::LexRule::new_regex(
                #regex,
               ::std::boxed::Box::new(
                   |state, matched_size| #transformer(state.current_n(matched_size))
               )
            )
        },
    }
}
