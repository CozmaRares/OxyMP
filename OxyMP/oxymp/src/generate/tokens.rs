#![allow(non_snake_case)]

use quote::ToTokens;

use crate::{
    data::tokens::{TokenVariant, TokensData},
    idents,
    symbols::*,
};

// TODO: consider a cleanup
pub fn generate(data: &TokensData) -> impl Iterator<Item = syn::Item> + '_ {
    let structs = data
        .variants
        .iter()
        .map(|variant| generate_struct(&data.ident, &data.visibility, variant))
        .map(syn::Item::Struct);

    let impls = data
        .variants
        .iter()
        .map(|variant| generate_try_from(&data.ident, variant))
        .map(syn::Item::Impl);

    structs.chain(impls)
}

fn generate_struct(
    tokens_ident: &syn::Ident,
    visibility: &proc_macro2::TokenStream,
    variant: &TokenVariant,
) -> syn::ItemStruct {
    let TokenVariant {
        ident: variant_ident,
        fields,
        ..
    } = variant;

    let struct_ident = idents::token_struct(tokens_ident, variant_ident);

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

    let _Debug = Derive::Debug.path();

    pq! {
        #[derive(#_Debug)]
        #visibility struct #struct_ident #pub_fields #trailing_semi
    }
}

fn generate_try_from(tokens_ident: &syn::Ident, variant: &TokenVariant) -> syn::ItemImpl {
    let TokenVariant {
        ident: variant_ident,
        fields,
        ..
    } = variant;

    let struct_ident = idents::token_struct(tokens_ident, variant_ident);

    // FIX: Clone could not be derived for the fields
    let (fields_stream, fields_cloned) = match fields {
        syn::Fields::Named(fields_named) => {
            let iter = fields_named.named.iter().map(|field| match &field.ident {
                Some(ident) => (ident.clone(), q! { #ident: #ident.clone() }),
                _ => unreachable!("named fields always have an ident"),
            });
            let (idents, idents_cloned) = split_iter(iter);

            (q! { { #(#idents),* } }, q! { { #(#idents_cloned),* } })
        }
        syn::Fields::Unnamed(fields_unnamed) => {
            let iter = fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(idx, _)| idents::numeric(idx))
                .map(|ident| (q! { #ident.clone() }, ident));
            let (idents_cloned, idents) = split_iter(iter);
            (q! { ( #(#idents),* ) }, q! { ( #(#idents_cloned),* ) })
        }
        syn::Fields::Unit => (q! {}, q! {}),
    };

    let _Option = Std::Option.path();
    let _Some = Std::Some.path();
    let _None = Std::None.path();

    pq! {
        impl #struct_ident {
            fn try_from_ref(value: &#tokens_ident) -> #_Option<Self> {
                match value {
                    #tokens_ident::#variant_ident #fields_stream => #_Some(Self #fields_cloned),
                    _ => #_None,
                }
            }
        }
    }
}

fn split_iter<I, A, B>(iter: I) -> (Vec<A>, Vec<B>)
where
    I: Iterator<Item = (A, B)>,
{
    let mut a = Vec::new();
    let mut b = Vec::new();
    for (ia, ib) in iter {
        a.push(ia);
        b.push(ib);
    }
    (a, b)
}
