use quote::{format_ident, ToTokens};

use crate::{
    data::tokens::{TokenVariant, TokensData},
    utils::split_iter,
};

pub fn generate_structs(data: &TokensData) -> impl IntoIterator<Item = syn::Item> + '_ {
    let mut structs: Vec<_> = data
        .variants
        .iter()
        .map(|variant| generate_struct(&data.ident, &data.visibility, variant))
        .map(syn::Item::Struct)
        .collect();

    let impls: Vec<_> = data
        .variants
        .iter()
        .map(|variant| generate_try_from(&data.ident, variant))
        .map(syn::Item::Impl)
        .collect();

    structs.extend(impls);
    structs
}

fn generate_struct(
    tokens_ident: &String,
    visibility: &proc_macro2::TokenStream,
    variant: &TokenVariant,
) -> syn::ItemStruct {
    let TokenVariant {
        ident: variant_ident,
        fields,
        ..
    } = variant;

    let tokens_ident = format_ident!("{}", tokens_ident);
    let variant_ident = format_ident!("{}", variant_ident);
    let struct_ident = format_ident!("{}{}", tokens_ident, variant_ident);

    let pub_fields = match fields {
        syn::Fields::Named(fields_named) => {
            let fields = fields_named.named.iter().map(|f| {
                let stream = f.to_token_stream();
                q! { pub #stream }
            });
            q! { { #(#fields),* } }
        }
        syn::Fields::Unnamed(fields_unnamed) => {
            let fields = fields_unnamed.unnamed.iter().map(|f| {
                let stream = f.to_token_stream();
                q! { pub #stream }
            });
            q! { ( #(#fields),* ) }
        }
        syn::Fields::Unit => q! {},
    };

    let trailing_semi = if matches!(fields, syn::Fields::Named(_)) {
        q! {}
    } else {
        q! { ; }
    };

    pq! {
        #[derive(Debug)]
        #visibility struct #struct_ident #pub_fields #trailing_semi
    }
}

fn generate_try_from(tokens_ident: &String, variant: &TokenVariant) -> syn::ItemImpl {
    let TokenVariant {
        ident: variant_ident,
        fields,
        ..
    } = variant;

    let tokens_ident = format_ident!("{}", tokens_ident);
    let variant_ident = format_ident!("{}", variant_ident);
    let struct_ident = format_ident!("{}{}", tokens_ident, variant_ident);

    let (fields_stream, fields_cloned) = match fields {
        syn::Fields::Named(fields_named) => {
            let iter = fields_named.named.iter().map(|field| match &field.ident {
                Some(ident) => (ident.clone(), q! { #ident: #ident.clone() }),
                _ => unreachable!(),
            });
            let (idents, idents_cloned) = split_iter(iter);
            (q! { { #(#idents),* } }, q! { { #(#idents_cloned),* } })
        }
        syn::Fields::Unnamed(fields_unnamed) => {
            let iter = fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(idx, _)| format_ident!("_{}", idx))
                .map(|ident| (q! { #ident.clone() }, ident));
            let (idents_cloned, idents) = split_iter(iter);
            (q! { ( #(#idents),* ) }, q! { ( #(#idents_cloned),* ) })
        }
        syn::Fields::Unit => (q! {}, q! {}),
    };

    pq! {
        impl #struct_ident {
            fn try_from(value: #tokens_ident) -> Option<Self> {
                match value {
                    #tokens_ident::#variant_ident #fields_stream => Some(Self #fields_stream),
                    _ => None,
                }
            }

            fn try_from_ref(value: &#tokens_ident) -> Option<Self> {
                match value {
                    #tokens_ident::#variant_ident #fields_stream => Some(Self #fields_cloned),
                    _ => None,
                }
            }
        }
    }
}
