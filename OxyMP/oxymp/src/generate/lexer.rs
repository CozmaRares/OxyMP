use std::collections::HashMap;

use quote::format_ident;

use crate::{
    automata::{
        dfa::{
            self, StateKind as DFAStateKind, StateTag as DFAStateTag, Transition as DFATransition,
            DFA,
        },
        nfa::{self, combine, NFA},
    },
    data::{
        lexer::LexerData,
        tokens::{TokenPattern, TokensData},
    },
    utils::combine_errors,
};

pub fn generate(
    tokens_data: &TokensData,
    lexers: Vec<(syn::ItemMod, LexerData)>,
) -> syn::Result<Vec<syn::Item>> {
    let mut lexer_cache = LexerCache::new();
    let mut items = Vec::new();
    let mut errors = Vec::new();

    for (item_mod, lexer_data) in lexers {
        let res = generate_one(tokens_data, lexer_data, item_mod, &mut lexer_cache);
        match res {
            Ok(inner_items) => items.extend(inner_items),
            Err(err) => errors.push(err),
        }
    }

    if errors.is_empty() {
        Ok(items)
    } else {
        Err(combine_errors(errors))
    }
}

fn generate_one(
    tokens_data: &TokensData,
    lexer_data: LexerData,
    mut item_mod: syn::ItemMod,
    cache: &mut LexerCache,
) -> syn::Result<Vec<syn::Item>> {
    let mut items = Vec::new();

    let dfa = generate_lexer_dfa(tokens_data, &lexer_data.skip_patterns, cache)?;

    let (error_idents, error_items) = cache.get_error_idents();
    items.extend(error_items);

    let lexer_items = generate_mod(tokens_data, lexer_data, dfa, error_idents)?;
    let (brace, _) = item_mod.content.expect("module should have content");
    item_mod.content = Some((brace, lexer_items));
    items.push(syn::Item::Mod(item_mod));
    Ok(items)
}

struct LexerCache {
    error_idents: Option<ErrorIdents>,
    tokens_nfa: Option<NFA>,
}

impl LexerCache {
    fn new() -> LexerCache {
        LexerCache {
            error_idents: None,
            tokens_nfa: None,
        }
    }

    fn get_error_idents(&mut self) -> (&ErrorIdents, Vec<syn::Item>) {
        let mut items = Vec::new();

        if self.error_idents.is_none() {
            let ErrorItem {
                idents: error_idents,
                items: error_items,
            } = generate_error_ds();
            items.extend(error_items);
            self.error_idents = Some(error_idents);
        }

        (
            self.error_idents
                .as_ref()
                .expect("error idents should be set"),
            items,
        )
    }

    fn get_tokens_nfa(&mut self, tokens_data: &TokensData) -> syn::Result<&NFA> {
        if self.tokens_nfa.is_none() {
            let nfa = generate_tokens_nfa(tokens_data)?;
            self.tokens_nfa = Some(nfa);
        }
        let nfa = self.tokens_nfa.as_ref().expect("tokens nfa should be set");
        Ok(nfa)
    }
}

struct ErrorIdents {
    expected_ident: syn::Ident,
    error_ident: syn::Ident,
}

struct ErrorItem {
    idents: ErrorIdents,
    items: Vec<syn::Item>,
}

#[allow(non_snake_case)]
fn generate_error_ds() -> ErrorItem {
    let LexerExpected = format_ident!("__Lexer_Common_Expected_{}", rand::random::<u64>());
    let LexerError = format_ident!("__Lexer_Common_Error_{}", rand::random::<u64>());

    let expected_enum: syn::ItemEnum = pq! {
        #[derive(Debug)]
        pub enum #LexerExpected {
            Char(char),
            CharRange { start: char, end: char },
        }
    };

    let expected_display: syn::ItemImpl = pq! {
        impl std::fmt::Display for #LexerExpected {
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
                    #LexerExpected::Char(c) => print_char(f, c),
                    #LexerExpected::CharRange { start, end } => {
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
        pub enum #LexerError {
            Native {
                unexpected_char: char,
                expected: Vec<#LexerExpected>,
            },
            UserTransform(Box<dyn std::error::Error>),
        }
    };

    let error_error: syn::ItemImpl = pq! {
        impl std::error::Error for #LexerError {}
    };

    let error_display: syn::ItemImpl = pq! {
        impl std::fmt::Display for #LexerError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #LexerError::Native {
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
                    #LexerError::UserTransform(e) => write!(f, "User transform error: {}", e),
                }
            }
        }
    };

    ErrorItem {
        idents: ErrorIdents {
            expected_ident: LexerExpected,
            error_ident: LexerError,
        },
        items: vec![
            syn::Item::Enum(expected_enum),
            syn::Item::Impl(expected_display),
            syn::Item::Enum(error_enum),
            syn::Item::Impl(error_error),
            syn::Item::Impl(error_display),
        ],
    }
}

fn generate_tokens_nfa(tokens_data: &TokensData) -> syn::Result<NFA> {
    let mut token_nfas = Vec::new();

    for (idx, variant) in tokens_data.variants.iter().enumerate() {
        let pattern = match &variant.pattern {
            TokenPattern::Exact { pattern } => pattern.value(),
            TokenPattern::Regex { pattern, .. } => pattern.value(),
        };

        let nfa = nfa::compile(
            &pattern,
            nfa::StateTag::Token {
                variant: variant.ident.to_string(),
                priority: idx,
            },
        )
        .map_err(|e| {
            syn::Error::new(
                variant.pattern.span(),
                format!(
                    "Error while compiling regex pattern for token variant '{}'\n{}",
                    variant.ident, e
                ),
            )
        })?;

        token_nfas.push(nfa);
    }

    Ok(nfa::combine(token_nfas))
}

fn generate_lexer_dfa(
    tokens_data: &TokensData,
    skip_patterns: &[syn::LitStr],
    cache: &mut LexerCache,
) -> syn::Result<DFA> {
    let token_nfa = cache.get_tokens_nfa(tokens_data)?;

    let mut nfas = Vec::new();

    for pattern_lit in skip_patterns {
        let pattern = pattern_lit.value();
        let nfa = nfa::compile(
            &pattern,
            nfa::StateTag::Skip {
                pattern: pattern.clone(),
            },
        )
        .map_err(|e| {
            syn::Error::new(
                pattern_lit.span(),
                format!(
                    "Error while compiling regex pattern for skip pattern '{}'\n{}",
                    pattern, e
                ),
            )
        })?;

        nfas.push(nfa);
    }

    nfas.push(token_nfa.clone());
    let nfa = nfa::combine(nfas);
    let dfa = dfa::compile(nfa);
    Ok(dfa)
}

fn generate_mod(
    tokens_data: &TokensData,
    lexer_data: LexerData,
    dfa: DFA,
    error_idents: &ErrorIdents,
) -> syn::Result<Vec<syn::Item>> {
    let tokenize_fn = generate_tokenize_fn(lexer_data.visibility);

    let states = generate_states(&dfa);

    let token_transforms = tokens_data
        .variants
        .iter()
        .map(|variant| {
            let variant_ident = variant.ident.clone();
            let ident = variant.ident.to_string();

            let transform = match &variant.pattern {
                TokenPattern::Exact { .. } => q! { Token::#variant_ident },
                TokenPattern::Regex { transform, .. } => q! {
                    super::#transform(inp)
                        .map(Token::#variant_ident)
                        .map_err(|e| Error::UserTransform(Box::new(e)))?
                },
            };

            (ident, transform)
        })
        .collect();

    let methods = generate_state_methods(&dfa, &token_transforms);

    let tokens_ident = &tokens_data.ident;

    let ErrorIdents {
        expected_ident,
        error_ident,
    } = error_idents;

    let items = items! {
        type Token = super::#tokens_ident;
        pub type Error = super::#error_ident;
        pub type LexerExpected = super::#expected_ident;

        #states
        #(#methods)*
        #tokenize_fn
    };
    Ok(items)
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
        .map(|ident| q! { State::#ident => #ident::transition(c), });
    let accept_branches = idents
        .iter()
        .map(|ident| q! { State::#ident => #ident::accept(inp, c), });

    q! {
        #[derive(Debug)]
        enum State {
            #(#idents),*
        }

        #(#state_structs)*

        impl State {
            fn transition(&self, c: char) -> Option<State> {
                match self {
                    #(#transition_branches)*
                }
            }

            fn accept(&self, inp: &str, c: char) -> Result<Option<Token>, Error> {
                match self {
                    #(#accept_branches)*
                }
            }
        }
    }
}

fn generate_state_methods<'a>(
    dfa: &'a DFA,
    token_transforms: &'a HashMap<String, proc_macro2::TokenStream>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    dfa.states().iter().map(move |(state_id, state)| {
        let transition_branches = state.transitions.iter().map(|(transition, target)| {
            let target = format_ident!("_{}", target);
            let target = q! { Some(State::#target) };
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
                        Error::Native {
                            unexpected_char: current_char,
                            expected: vec![ #(#expected),* ]
                        }
                    )
                }
            }
            DFAStateKind::Accepting(tag) => match tag {
                DFAStateTag::Token { variant, .. } => {
                    let transform = token_transforms
                        .get(variant)
                        .expect("token pattern should always have an transform");
                    q! { Ok(Some(#transform)) }
                }
                DFAStateTag::Skip { .. } => q! { Ok(None) },
            },
        };

        let ident = format_ident!("_{}", state_id);

        q! {
            impl #ident {
                fn transition(c: char) -> Option<State> {
                    match c {
                        #(#transition_branches)*
                        _ => None,
                    }
                }

                fn accept(inp: &str, current_char: char) -> Result<Option<Token>, Error> {
                    #accept
                }
            }
        }
    })
}

fn generate_tokenize_fn(vis: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    q! {
        #vis fn tokenize(inp: &str) -> Result<Vec<Token>, Error> {
            let mut toks = Vec::new();
            let mut state = State::_1;
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
                        state = State::_1;
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
