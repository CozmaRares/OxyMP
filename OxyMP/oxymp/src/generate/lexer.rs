use quote::format_ident;

use crate::{
    automata::dfa::{
        State as DFAState, StateKind as DFAStateKind, StateTag as DFAStateTag,
        Transition as DFATransition, DFA,
    },
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
    let methods = generate_state_methods(&dfa, tokens_data);

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
            #(#methods)*
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
    let accept_branches = idents
        .iter()
        .map(|ident| q! { _internal_oxymp_LexerState::#ident => #ident::accept(inp), });

    q! {
        #[derive(Debug)]
        pub(super) enum _internal_oxymp_LexerState {
            #(#idents),*
        }

        #(#state_structs)*

        impl _internal_oxymp_LexerState {
            pub(super) fn transition(&self, c: char) -> Option<_internal_oxymp_LexerState> {
                match self {
                    #(#transition_branches)*
                }
            }

            pub(super) fn accept(&self, inp: &str) -> Result<Option<_internal_oxymp_Token>, String> {
                match self {
                    #(#accept_branches)*
                }
            }
        }
    }
}

fn generate_state_methods<'a>(
    dfa: &'a DFA,
    tokens_data: &'a TokensData,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    dfa.states().iter().map(|(state_id, state)| {
        let transition_branches = state.transitions.iter().map(|(transition, target)| {
            let target = format_ident!("_{}", target);
            let target = q! { Some(_internal_oxymp_LexerState::#target) };
            match transition {
                DFATransition::Char(c) => q! { #c => #target, },
                DFATransition::Chars { start, end } => q! { #start..=#end => #target, },
            }
        });

        let tokens_ident = &tokens_data.ident;
        let accept = match &state.kind {
            DFAStateKind::NotAccepting => {
                // TODO: return a proper error that implements Error
                // also specify what characters were expected
                // include current slice in the input, maybe max 10 chars, maybe also include
                // before chars and have a ^ below the unexpected char
                q! { Err(format!("Unexpected character '{}'", inp.chars().next().unwrap())) }
            }
            DFAStateKind::Accepting(tag) => match tag {
                DFAStateTag::Token { variant, priority } => {
                    let variant_ident = format_ident!("{}", variant);
                    q! { Ok(Some(#tokens_ident::#variant_ident)) }
                }
                DFAStateTag::Skip { lexer, pattern } => q! { Ok(None) },
            },
        };

        let ident = format_ident!("_{}", state_id);

        q! {
            impl #ident {
                fn transition(c: char) -> Option<_internal_oxymp_LexerState> {
                    match c {
                        #(#transition_branches)*
                        _ => None,
                    }
                }

                fn accept(inp: &str) -> Result<Option<_internal_oxymp_Token>, String> {
                    #accept
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
    let mod_ident = format_ident!("_{}", lexer_ident);

    pq! {
        impl #lexer_ident {
            #vis fn tokenize(inp: &str) -> Result<Vec<#tokens_ident>, String> {
                let mut toks = Vec::new();
                let mut state = _Lexer::_internal_oxymp_LexerState::_1;
                let mut match_start = 0;
                let mut iter = inp.chars().enumerate().peekable();

                while let Some((current_index, c)) = iter.peek() {
                    match state.transition(*c) {
                        Some(new_state) => {
                            state = new_state;
                            iter.next();
                        }
                        None => {
                            if let Some(tok) = state.accept(&inp[match_start..*current_index])? {
                                toks.push(tok);
                            }
                            state = _Lexer::_internal_oxymp_LexerState::_1;
                            match_start = *current_index;
                        }
                    }
                }

                if let Some(tok) = state.accept(&inp[match_start..])? {
                    toks.push(tok);
                }

                Ok(toks)
            }
        }
    }
}
