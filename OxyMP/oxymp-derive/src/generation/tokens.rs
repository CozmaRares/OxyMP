#![allow(non_snake_case)]

use proc_macro2::Ident;
use quote::quote;

use crate::{
    data::MacroData,
    idents::tokens::{enum_ident, struct_ident},
    symbols::{get_def, Symbol},
    tokens::{ExactToken, RegexToken, TokenInfo},
};

pub fn generate_tokens(data: &MacroData) -> proc_macro2::TokenStream {
    let visibility = &data.visibility;
    let token_info = &data.tokens;

    let idents = token_info.iter().filter_map(generate_token_idents);

    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);
    let _Clone = get_def(Symbol::DeriveClone, data.simple_types);
    let _TokenDebugInfo = get_def(Symbol::UtilTokenDebugInfo, data.simple_types);

    let structs = idents
        .clone()
        .map(|idents| match idents {
            (_, struct_ident, None) => quote! {
                #visibility struct #struct_ident;
            },
            (_, struct_ident, Some(kind_ident)) => quote! {
                #visibility struct #struct_ident(#visibility #kind_ident);
            },
        })
        .map(|struct_def| {
            quote! {
                #[derive(#_Debug, #_Clone)]
                #struct_def
            }
        });

    let enum_entries = idents.map(|(enum_entry, struct_ident, _)| {
        quote! {
            #enum_entry(#struct_ident, #[cfg(debug_assertions)] #_TokenDebugInfo)
        }
    });

    quote! {
        #(#structs)*

        #[derive(#_Debug, #_Clone)]
        #visibility enum Token {
            #(#enum_entries),*
        }
    }
}

fn generate_token_idents(
    token: &TokenInfo,
) -> Option<(Ident, Ident, Option<&proc_macro2::TokenStream>)> {
    match token {
        TokenInfo::Exact(ExactToken { name, .. }) => {
            Some((enum_ident(name), struct_ident(name), None))
        }
        TokenInfo::Regex(RegexToken { name, kind, .. }) => {
            Some((enum_ident(name), struct_ident(name), Some(kind)))
        }
        TokenInfo::Ignore(_) => None,
    }
}
