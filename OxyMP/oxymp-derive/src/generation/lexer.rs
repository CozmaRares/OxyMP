#![allow(non_snake_case)]

use quote::quote;

use crate::{
    data::MacroData,
    idents::tokens,
    symbols::{get_def, Symbol},
    tokens::{ExactToken, IgnorePattern, RegexToken, TokenInfo},
};

pub fn generate_constructor(data: &MacroData) -> proc_macro2::TokenStream {
    let visibility = &data.visibility;
    let token_info = &data.tokens;

    let rules = token_info
        .iter()
        .enumerate()
        .map(|(idx, tok)| generate_rule(tok, data.simple_types, idx));

    let has_tier = rules.clone().any(|(tier, _rule)| tier.is_some());

    let rules = rules.map(|(tier, rule)| match tier {
        None => quote! {
            builder.add_rule(#rule);
        },
        Some(tier) => quote! {
            builder.add_tiered_rule(#tier, #rule);
        },
    });

    let _Lexer = get_def(Symbol::UtilLexer, data.simple_types);
    let _LexerBuilder = get_def(Symbol::UtilLexerBuilder, data.simple_types);

    let builder_tokens = if has_tier {
        quote! {
            #_LexerBuilder::default()
        }
    } else {
        quote! {
            #_LexerBuilder::<Token>::default()
        }
    };

    quote! {
        #visibility fn create_lexer() -> #_Lexer<Token> {
            let mut builder = #builder_tokens;
            #(#rules)*
            builder.build()
        }
    }
}

fn generate_rule(
    tok: &TokenInfo,
    simple: bool,
    id: usize,
) -> (&Option<proc_macro2::TokenStream>, proc_macro2::TokenStream) {
    let _Box = get_def(Symbol::Box, simple);
    let _LexRule = get_def(Symbol::UtilLexRule, simple);
    let _TokenHandler = get_def(Symbol::UtilTokenHandler, simple);
    let _TokenMatcher = get_def(Symbol::UtilTokenMatcher, simple);
    let _TokenDebugInfo = get_def(Symbol::UtilTokenDebugInfo, simple);

    match tok {
        TokenInfo::Exact(ExactToken {
            name,
            pattern,
            tier,
        }) => {
            let enum_ident = tokens::enum_ident(name);
            let struct_ident = tokens::struct_ident(name);

            (
                tier,
                quote! {
                    #_LexRule::new(
                        #_TokenMatcher::Exact(#pattern.to_string()),
                        #_TokenHandler::Pattern(
                            #_Box::new(|state, matched_size| Token::#enum_ident(
                                #struct_ident,
                                #[cfg(debug_assertions)] #_TokenDebugInfo {
                                    lex_rule: #name.to_string(),
                                    offset: state.current_offset(),
                                    matched_size,
                                    matched_string: state.current_n(matched_size).to_string(),
                                }
                            ))
                        ),
                        #[cfg(debug_assertions)] #id,
                    )
                },
            )
        }
        TokenInfo::Regex(RegexToken {
            name,
            regex,
            transformer_fn,
            tier,
            ..
        }) => {
            let enum_ident = tokens::enum_ident(name);
            let struct_ident = tokens::struct_ident(name);

            (
                tier,
                quote! {
                    #_LexRule::new(
                        #_TokenMatcher::regex(#regex),
                        #_TokenHandler::Regex(
                            #_Box::new(
                                |state, matched_size|
                                    #transformer_fn(state.current_n(matched_size))
                                        .map(|t| Token::#enum_ident(
                                            #struct_ident(t),
                                            #[cfg(debug_assertions)] #_TokenDebugInfo {
                                                lex_rule: #name.to_string(),
                                                offset: state.current_offset(),
                                                matched_size,
                                                matched_string: state.current_n(matched_size).to_string(),
                                            }
                                        ))
                            )
                        ),
                        #[cfg(debug_assertions)] #id,
                    )
                },
            )
        }
        TokenInfo::Ignore(IgnorePattern { regex, tier }) => (
            tier,
            quote! {
                #_LexRule::new(
                    #_TokenMatcher::regex(#regex),
                    #_TokenHandler::Ignore,
                    #[cfg(debug_assertions)] #id,
                )
            },
        ),
    }
}
