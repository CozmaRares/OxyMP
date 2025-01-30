use change_case::snake_case;
use quote::{format_ident, quote};

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

    eprintln!("{:#?}", enum_data.variants);

    Ok(quote! {
        mod #mod_ident {
            use super::*;

            #(#generated)*
        }
    })
}

// TODO: add check for fields based on the macro attribute
// exact -> no fields (unit)
// regex -> only 1 unnamed field

fn generate_token_struct(ident: &syn::Ident, fields: &syn::Fields) -> proc_macro2::TokenStream {
    match fields {
        syn::Fields::Named(fields_named) => quote! { struct #ident #fields_named },
        syn::Fields::Unnamed(fields_unnamed) => quote! { struct #ident #fields_unnamed; },
        syn::Fields::Unit => quote! { struct #ident; },
    }
}

fn generate_token_struct_impl(
    original_ident: &syn::Ident,
    ident: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let check = match fields {
        syn::Fields::Named(fields_named) => {
            let field_names = fields_named
                .named
                .iter()
                .filter_map(|field| field.ident.clone());
            let field_names = quote! { #(#field_names),* };

            quote! {
                #original_ident::#ident { #field_names } => Some(Self { #field_names }),
            }
        }
        syn::Fields::Unnamed(fields_unnamed) => {
            let field_names = fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| format_ident!("field_{}", i));
            let field_names = quote! { #(#field_names),* };

            quote! {
                #original_ident::#ident ( #field_names ) => Some(Self ( #field_names )),
            }
        }
        syn::Fields::Unit => quote! {
            #original_ident::#ident => Some(Self),
        },
    };

    quote! {
        impl #ident {
            fn from_token(token: #original_ident) -> Option<Self> {
                match token {
                    #check
                    _ => None,
                }
            }
        }
    }
}

/*

* mod to_snake_case(ast.ident) {
*   struct Number(i64);
*   struct Identifier(String);
*   struct ParenLeft;
*   ...
*
*   impl Number { fn from_token(token: &Token) -> Option<Self> { ... } }
*   ...
*
*   fn get_lex_rules() { ... }
* }

 */
