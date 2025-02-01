mod info;

use change_case::snake_case;
use quote::{format_ident, quote, ToTokens};
use syn::spanned::Spanned;

use info::*;

// TODO:add user guidance for error messages

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
            "Tokens can only be derived for enums",
        ));
    };

    let info = get_token_info(&enum_data.variants);

    eprintln!("{:#?}", info);

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

    Ok(quote! {
        mod #mod_ident {
            use super::*;

            #(#generated)*
        }
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
    attrs: &Vec<syn::Attribute>,
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
            "Exact tokens can't contain any fields. Consider removing any associated data for this token type.",
        )),

        (TokenType::Regex, syn::Fields::Unit) => Err(syn::Error::new(
            ident.span(),
            "Regex tokens must encapsulate data. Ensure that the token definition includes the necessary fields."
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
                    "Tokens can only be derived for enums with attributes",
                )),
                AttributeError::MoreThan1(span) => {
                    Err(syn::Error::new(span, "Only one attribute is allowed"))
                }
                AttributeError::InvalidAttribute(span) => {
                    Err(syn::Error::new(span, "Unhandled attribute"))
                }
            };
        };

        ensure_correct_attribute(&token_type, fields, ident)?;

        let token_info: TokenInfo = syn::parse2(attr.to_token_stream())?;

        info.push((ident.to_string(), token_info));
    }

    Ok(info)
}
