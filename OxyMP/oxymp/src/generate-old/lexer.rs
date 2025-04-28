use std::collections::HashMap;

use quote::format_ident;

use crate::{
    automata::dfa::{
        State as DFAState, StateKind as DFAStateKind, StateTag as DFAStateTag,
        Transition as DFATransition, DFA,
    },
    data::{
        lexer::LexerData,
        tokens::{TokenPattern, TokensData},
    },
};

pub fn generate_error_struct() -> Vec<syn::Item> {
    let expected_enum: syn::ItemEnum = pq! {
        #[derive(Debug)]
        pub enum LexerExpected {
            Char(char),
            CharRange { start: char, end: char },
        }
    };

    let expected_display: syn::ItemImpl = pq! {
        impl std::fmt::Display for LexerExpected {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let print_char = |f: &mut std::fmt::Formatter<'_>, c: &char| {
                    if c.is_whitespace() {
                        write!(f, "whitespace(code {})", *c as usize)
                    }
                    else {
                        write!(f, "'{}", c)
                    }
                };

                match self {
                    LexerExpected::Char(c) => print_char(f, c),
                    LexerExpected::CharRange { start, end } => {
                        print_char(f, start)?;
                        write!(f, "..=")?;
                        print_char(f, end)
                    }
                }
            }
        }
    };

    let error_enum: syn::ItemEnum = pq! {
        // TODO: add location
        // needs refactor of accept fns
        // accept and reject
        #[derive(Debug)]
        pub enum LexerError {
            Native {
                unexpected_char: char,
                expected: Vec<LexerExpected>,
            },
            UserTransform(Box<dyn std::error::Error>),
        }
    };

    let error_error: syn::ItemImpl = pq! {
        impl std::error::Error for LexerError {}
    };

    let error_display: syn::ItemImpl = pq! {
        impl std::fmt::Display for LexerError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    LexerError::Native {
                        unexpected_char,
                        expected,
                    } => {
                        let expected = expected
                            .iter()
                            .map(|expected| format!("{}", expected))
                            .collect::<Vec<_>>()
                            .join(", ");
                        write!(
                            f,
                            "Unexpected character '{}'\nExpected: {}",
                            unexpected_char, expected
                        )
                    }
                    LexerError::UserTransform(e) => write!(f, "User transform error: {}", e),
                }
            }
        }
    };

    vec![
        syn::Item::Enum(expected_enum),
        syn::Item::Impl(expected_display),
        syn::Item::Enum(error_enum),
        syn::Item::Impl(error_error),
        syn::Item::Impl(error_display),
    ]
}

pub fn generate(
    tokens_data: &TokensData,
    lexer_data: LexerData,
    dfa: DFA,
) -> syn::Result<[syn::Item; 2]> {
    let tokenize_impl =
        generate_tokenize_impl(&tokens_data.ident, &lexer_data.ident, lexer_data.visibility);

    let states = generate_states(&dfa);

    let tokens_ident = &tokens_data.ident;
    let token_transforms = tokens_data
        .variants
        .iter()
        .map(|variant| {
            let variant_ident = variant.ident.clone();
            let ident = variant.ident.to_string();

            let transform = match &variant.pattern {
                TokenPattern::Exact { .. } => q! { #tokens_ident::#variant_ident },
                TokenPattern::Regex { transform, .. } => q! {
                    #transform(inp)
                        .map_err(|e| LexerError::UserTransform(Box::new(e)))?
                },
            };

            (ident, transform)
        })
        .collect();

    let methods = generate_state_methods(&dfa, &tokens_data.ident, &token_transforms);

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
        .map(|ident| q! { _internal_oxymp_LexerState::#ident => #ident::accept(inp, c), });

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

            pub(super) fn accept(&self, inp: &str, c: char) -> Result<Option<_internal_oxymp_Token>, LexerError> {
                match self {
                    #(#accept_branches)*
                }
            }
        }
    }
}

fn generate_state_methods<'a>(
    dfa: &'a DFA,
    tokens_ident: &'a syn::Ident,
    token_transforms: &'a HashMap<String, proc_macro2::TokenStream>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    dfa.states().iter().map(move |(state_id, state)| {
        let transition_branches = state.transitions.iter().map(|(transition, target)| {
            let target = format_ident!("_{}", target);
            let target = q! { Some(_internal_oxymp_LexerState::#target) };
            match transition {
                DFATransition::Char(c) => q! { #c => #target, },
                DFATransition::Chars { start, end } => q! { #start..=#end => #target, },
            }
        });

        let accept = match &state.kind {
            DFAStateKind::NotAccepting => {
                let expected = state
                    .transitions
                    .iter()
                    .map(|(transition, _)| match transition {
                        DFATransition::Char(c) => q! {
                            LexerExpected::Char(#c)
                        },
                        DFATransition::Chars { start, end } => q! {
                            LexerExpected::CharRange{ start: #start, end: #end }
                        },
                    });

                q! {
                    Err(
                        LexerError::Native {
                            unexpected_char: current_char,
                            expected: vec![ #(#expected),* ]
                        }
                    )
                }
            }
            DFAStateKind::Accepting(tag) => match tag {
                DFAStateTag::Token { variant, priority } => {
                    let transform = token_transforms
                        .get(variant)
                        .expect("token pattern should always have an transform");
                    q! { Ok(Some(#transform)) }
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

                fn accept(inp: &str, current_char: char) -> Result<Option<_internal_oxymp_Token>, LexerError> {
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
            #vis fn tokenize(inp: &str) -> Result<Vec<#tokens_ident>, LexerError> {
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
                            if let Some(tok) = state.accept(&inp[match_start..*current_index], *c)? {
                                toks.push(tok)
                            }
                            state = _Lexer::_internal_oxymp_LexerState::_1;
                            match_start = *current_index;
                        }
                    }
                }

                if inp.len() > 0 {
                    if let Some(tok) = state.accept(&inp[match_start..], inp.chars().last().unwrap())? {
                        toks.push(tok)
                    }
                }

                Ok(toks)
            }
        }
    }
}
