use quote::format_ident;

use crate::{
    automata::dfa::{State as DFAState, Transition as DFATransition, DFA},
    data::{lexer::LexerData, tokens::TokensData},
    utils::split_iter,
};

pub fn generate(
    tokens_data: &TokensData,
    lexer_data: LexerData,
    dfa: DFA,
) -> syn::Result<[syn::Item; 2]> {
    let tokenize_impl =
        generate_tokenize_impl(&tokens_data.ident, &lexer_data.ident, lexer_data.visibility);

    let states = generate_states(&dfa);
    let transitions = generate_transitions(&dfa, tokens_data);

    let tokens_ident = &tokens_data.ident;
    let lexer_ident = &lexer_data.ident;

    let mod_ident = format_ident!("_{}", lexer_data.ident);
    let item_mod: syn::ItemMod = pq! {
        #[allow(non_snake_case)]
        mod #mod_ident {
            use super::*;

            type _internal_oxymp_Token = #tokens_ident;
            type _internal_oxymp_Lexer = #lexer_ident;

            #states
            #(#transitions)*
        }
    };

    Ok([syn::Item::Impl(tokenize_impl), syn::Item::Mod(item_mod)])
}

fn generate_states(dfa: &DFA) -> proc_macro2::TokenStream {
    let idents: Vec<_> = dfa
        .states()
        .iter()
        .map(|(state_id, _)| format_ident!("_{}", state_id))
        .collect();

    let state_structs = idents.iter().map(|ident| q! { struct #ident; });
    let transition_branches = idents
        .iter()
        .map(|ident| q! { _internal_oxymp_LexerState::#ident => #ident::transition(c), });

    q! {
        enum _internal_oxymp_LexerState {
            #(#idents),*
        }

        #(#state_structs)*

        impl _internal_oxymp_LexerState {
            fn transition(&self, c: char) -> Option<_internal_oxymp_LexerState> {
                match self {
                    #(#transition_branches)*
                }
            }
        }
    }
}

fn generate_transitions<'a>(
    dfa: &'a DFA,
    tokens_data: &'a TokensData,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    dfa.states().iter().map(|(state_id, state)| {
        let ident = format_ident!("_{}", state_id);

        let branches = state.transitions.iter().map(|(transition, target)| {
            let target = format_ident!("_{}", target);
            let target = q! { Some(_internal_oxymp_LexerState::#target) };
            match transition {
                DFATransition::Char(c) => q! { #c => #target, },
                DFATransition::Chars { start, end } => q! { #start..=#end => #target, },
            }
        });

        q! {
            impl #ident {
                fn transition(c: char) -> Option<_internal_oxymp_LexerState> {
                    match c {
                        #(#branches)*
                        _ => None,
                    }
                }
            }
        }
    })
}

fn generate_tokenize_impl(
    tokens_ident: &proc_macro2::Ident,
    lexer_ident: &proc_macro2::Ident,
    vis: proc_macro2::TokenStream,
) -> syn::ItemImpl {
    pq! {
        impl #lexer_ident {
            #vis fn tokenize(inp: &str) -> Vec<#tokens_ident> {
                todo!()
            }
        }
    }
}
