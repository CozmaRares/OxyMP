#![allow(non_snake_case)]

use std::collections::HashMap;

use quote::format_ident;

use crate::{
    automata::{
        dfa::{
            self, StateKind as DFAStateKind, StateTag as DFAStateTag, Transition as DFATransition,
            DFA,
        },
        nfa::{self, NFA},
    },
    data::{
        lexer::LexerData,
        tokens::{TokenPattern, TokensData},
    },
    range::Range,
    symbols::*,
    utils::FoldErrors,
};

pub fn generate(
    tokens_data: &TokensData,
    lexers: Vec<(syn::ItemMod, LexerData)>,
) -> syn::Result<Vec<syn::Item>> {
    let mut lexer_cache = LexerCache::new();

    let items = lexers
        .into_iter()
        .map(|(item_mod, lexer_data)| {
            generate_one(tokens_data, lexer_data, item_mod, &mut lexer_cache)
        })
        .collect_errors()?;

    Ok(items.into_iter().flatten().collect())
}

fn generate_one(
    tokens_data: &TokensData,
    lexer_data: LexerData,
    mut item_mod: syn::ItemMod,
    cache: &mut LexerCache,
) -> syn::Result<Vec<syn::Item>> {
    let mut items = Vec::new();

    let dfa = generate_lexer_dfa(tokens_data, lexer_data.skip_patterns, cache)?;

    let (error_idents, error_items) = cache.get_error_idents();
    items.extend(error_items);

    let lexer_items = generate_mod(tokens_data, dfa, error_idents, lexer_data.user_error_ident)?;
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

fn generate_error_ds() -> ErrorItem {
    let LexerExpected = format_ident!("__Lexer_Common_Expected_{}", rand::random::<u64>());
    let LexerError = format_ident!("__Lexer_Common_Error_{}", rand::random::<u64>());

    let _char = Core::Char.path();
    let _Display = Trait::Display.path();
    let _Error = Trait::Error.path();
    let _Formatter = Std::Formatter.path();
    let _FmtResult = Std::FmtResult.path();
    let _write = Macro::Write.path();
    let _usize = Core::Usize.path();
    let _Vec = Std::Vec.path();
    let _format = Macro::Format.path();
    let _writeln = Macro::Writeln.path();
    let _Debug = Derive::Debug.path();
    let _String = Std::String.path();

    let items = items! {
        #[derive(#_Debug)]
        pub enum #LexerExpected {
            Char(#_char),
            CharRange { start: #_char, end: #_char },
        }

        impl #_Display for #LexerExpected {
            fn fmt(&self, f: &mut #_Formatter<'_>) -> #_FmtResult {
                let print_char = |f: &mut #_Formatter<'_>, c: &#_char| {
                    if c.is_whitespace() {
                        #_write!(f, "whitespace(code {})", *c as #_usize)
                    }
                    else {
                        #_write!(f, "'{}'", c)
                    }
                };

                match self {
                    #LexerExpected::Char(c) => print_char(f, c),
                    #LexerExpected::CharRange { start, end } => {
                        print_char(f, start)?;
                        #_write!(f, "..=")?;
                        print_char(f, end)
                    }
                }
            }
        }

        #[derive(#_Debug)]
        pub enum #LexerError<T> where T: #_Error {
            Native {
                unexpected_char: #_char,
                expected: #_Vec<#LexerExpected>,
                source: #_String,
                offset: #_usize,
            },
            UserTransform(T),
        }

        impl<T: #_Error> #_Error for #LexerError<T> {}
        impl<T: #_Error> #_Display for #LexerError<T> {
            fn fmt(&self, f: &mut #_Formatter<'_>) -> #_FmtResult {
                match self {
                    #LexerError::Native {
                        unexpected_char,
                        expected,
                        source,
                        offset,
                    } => {
                        let expected = expected
                            .iter()
                            .map(|expected| #_format!("{}", expected))
                            .collect::<#_Vec<_>>()
                            .join(", ");
                        #_writeln!(
                            f,
                            "Unexpected character '{}'\nExpected: {}",
                            unexpected_char, expected
                        );

                        // FIX: will not work for multi-column width characters
                        #_writeln!(f, "{}", source)?;
                        for _ in 0..*offset {
                            #_write!(f, " ")?;
                        }
                        #_writeln!(f, "^ --- here")
                    }
                    #LexerError::UserTransform(e) => #_writeln!(f, "User transform error: {}", e),
                }
            }
        }
    };

    ErrorItem {
        idents: ErrorIdents {
            expected_ident: LexerExpected,
            error_ident: LexerError,
        },
        items,
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
    skip_patterns: Vec<syn::LitStr>,
    cache: &mut LexerCache,
) -> syn::Result<DFA> {
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

    let token_nfa = cache.get_tokens_nfa(tokens_data)?;
    nfas.push(token_nfa.clone());

    let nfa = nfa::combine(nfas);
    let dfa = dfa::compile(nfa);
    Ok(dfa)
}

fn generate_mod(
    tokens_data: &TokensData,
    dfa: DFA,
    error_idents: &ErrorIdents,
    user_error_ident: syn::Ident,
) -> syn::Result<Vec<syn::Item>> {
    let tokenize_fn = generate_tokenize_fn();

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
                        .map(Token::#variant_ident)?
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
        type UserError = super::#user_error_ident;
        pub type Error = super::#error_ident<UserError>;
        pub type LexerExpected = super::#expected_ident;

        #states
        #(#methods)*
        #tokenize_fn
    };
    Ok(items)
}

fn generate_match_branches<'a>(
    idents: &'a [syn::Ident],
    method: &str,
    args: &'a [&str],
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    let method = format_ident!("{}", method);
    idents.iter().map(move |ident| {
        let args = args.into_iter().map(|arg| format_ident!("{}", arg));
        q! { State::#ident => #ident::#method(#(#args),*), }
    })
}

fn generate_states(dfa: &DFA) -> proc_macro2::TokenStream {
    let idents: Vec<_> = dfa
        .states()
        .iter()
        .map(|(state_id, _)| format_ident!("_{}", state_id))
        .collect();

    let state_structs = idents.iter().map(|ident| q! { struct #ident; });

    let transition_branches = generate_match_branches(&idents, "transition", &["c"]);
    let is_accepting_branches = generate_match_branches(&idents, "is_accepting", &[]);
    let accept_branches = generate_match_branches(&idents, "accept", &["inp"]);
    let reject_branches = generate_match_branches(&idents, "reject", &["inp", "c", "offset"]);

    let _Option = Std::Option.path();
    let _char = Core::Char.path();
    let _str = Core::Str.path();
    let _Result = Std::Result.path();
    let _Debug = Derive::Debug.path();
    let _usize = Core::Usize.path();
    let _Clone = Derive::Clone.path();

    q! {
        #(#state_structs)*

        #[derive(#_Debug, #_Clone)]
        enum State {
            #(#idents),*
        }

        impl State {
            fn transition(&self, c: #_char) -> #_Option<State> {
                match self {
                    #(#transition_branches)*
                }
            }

            fn is_accepting(&self) -> bool {
                match self {
                    #(#is_accepting_branches)*
                }
            }

            fn accept(&self, inp: &#_str) -> Result<#_Option<Token>, UserError> {
                match self {
                    #(#accept_branches)*
                }
            }

            fn reject(&self, inp: &#_str, c: #_char, offset: #_usize) -> Error {
                match self {
                    #(#reject_branches)*
                }
            }
        }
    }
}

fn generate_state_methods<'a>(
    dfa: &'a DFA,
    token_transforms: &'a HashMap<String, proc_macro2::TokenStream>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    let _Some = Std::Some.path();
    let _Err = Std::Err.path();
    let _vec = Macro::Vec.path();
    let _Ok = Std::Ok.path();
    let _None = Std::None.path();
    let _char = Core::Char.path();
    let _Option = Std::Option.path();
    let _str = Core::Str.path();
    let _Result = Std::Result.path();
    let _unreachable = Macro::Unreachable.path();
    let _String = Std::String.path();
    let _usize = Core::Usize.path();

    dfa.states().iter().map(move |(state_id, state)| {
        let transition_branches = state
            .transitions
            .iter()
            .map(|(DFATransition(range), target)| {
                let target = format_ident!("_{}", target);
                let target = q! { #_Some(State::#target) };
                match range {
                    Range::One(c) => q! { #c => #target, },
                    Range::Multi { start, end } => q! { #start..=#end => #target, },
                }
            });

        let is_accepting_impl = match &state.kind {
            DFAStateKind::NotAccepting => q! { false },
            DFAStateKind::Accepting(_) => q! { true },
        };

        let accept_impl = match &state.kind {
            DFAStateKind::NotAccepting => q! { #_unreachable!("The 'accept' method should not be called on a DFA state that is not accepting. This is most likely a bug in the lexer.") },
            DFAStateKind::Accepting(tag) => match tag {
                DFAStateTag::Token { variant, .. } => {
                    let transform = token_transforms
                        .get(variant)
                        .expect("token pattern should always have an transform");
                    q! { #_Ok(#_Some(#transform)) }
                }
                DFAStateTag::Skip { .. } => q! { #_Ok(#_None) },
            }
        };

        let reject_impl = match &state.kind {
            DFAStateKind::Accepting(..) => q! { #_unreachable!("The 'reject' method should not be called on a DFA state that is accepting. This is most likely a bug in the lexer.") },
            DFAStateKind::NotAccepting => {
                let expected =
                    state
                        .transitions
                        .iter()
                        .map(|(DFATransition(range), _)| match range {
                            Range::One(c) => q! {
                                LexerExpected::Char(#c)
                            },
                            Range::Multi { start, end } => q! {
                                LexerExpected::CharRange{ start: #start, end: #end }
                            },
                        });

                // TODO: select just the last part of the input
                q! {
                    Error::Native {
                        unexpected_char: c,
                        expected: #_vec![ #(#expected),* ],
                        source: #_String::from(inp),
                        offset,
                    }
                }
            }
        };

        let ident = format_ident!("_{}", state_id);

        q! {
            impl #ident {
                fn transition(c: #_char) -> #_Option<State> {
                    match c {
                        #(#transition_branches)*
                        _ => #_None,
                    }
                }

                fn is_accepting() -> bool {
                    #is_accepting_impl
                }

                fn accept(inp: &#_str) -> #_Result<#_Option<Token>, UserError> {
                    #accept_impl
                }

                fn reject(inp: &#_str, c: #_char, offset: #_usize) -> Error {
                    #reject_impl
                }
            }
        }
    })
}

fn generate_tokenize_fn() -> proc_macro2::TokenStream {
    let _str = Core::Str.path();
    let _Result = Std::Result.path();
    let _Ok = Std::Ok.path();
    let _Some = Std::Some.path();
    let _Vec = Std::Vec.path();
    let _None = Std::None.path();
    let _Err = Std::Err.path();

    q! {
        pub fn tokenize(inp: &#_str) -> #_Result<#_Vec<Token>, Error> {
            let mut toks = #_Vec::new();
            let mut state = State::_1;
            let mut match_start = 0;
            let mut iter = inp.chars().enumerate().peekable();

            while iter.peek().is_some() {
                let mut last_accepting_state = #_None;
                let mut last_accepting_pos = match_start;
                let mut temp_state = state.clone();
                let mut temp_pos = iter.peek().map(|(pos, _)| *pos).unwrap_or(inp.len());

                let mut temp_iter = iter.clone();
                while let #_Some((pos, ch)) = temp_iter.peek() {
                    if let #_Some(new_state) = temp_state.transition(*ch) {
                        temp_state = new_state;
                        temp_pos = *pos + 1;

                        if temp_state.is_accepting() {
                            last_accepting_state = #_Some(temp_state.clone());
                            last_accepting_pos = temp_pos;
                        }

                        temp_iter.next();
                    } else {
                        break;
                    }
                }

                if let #_Some(accepting_state) = last_accepting_state {
                    while iter.peek().map(|(pos, _)| *pos).unwrap_or(inp.len()) < last_accepting_pos
                    {
                        iter.next();
                    }

                    if let #_Some(tok) = accepting_state
                        .accept(&inp[match_start..last_accepting_pos])
                        .map_err(Error::UserTransform)?
                    {
                        toks.push(tok);
                    }

                    state = State::_1;
                    match_start = last_accepting_pos;
                } else {
                    let (pos, ch) = iter.peek().expect("was previously checked to be Some");
                    // TODO: get last line of input & a slice where the error is
                    return #_Err(state.reject(&inp, *ch, *pos));
                }
            }

            if match_start < inp.len() {
                if let #_Some(tok) = state
                    .accept(&inp[match_start..])
                    .map_err(Error::UserTransform)?
                {
                    toks.push(tok);
                }
            }

            #_Ok(toks)
        }
    }
}
