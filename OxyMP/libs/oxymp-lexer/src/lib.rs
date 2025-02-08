#![allow(unused)]
#![allow(non_snake_case)]

use oxymp_macro_utils::symbols::Symbol;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

// TODO: get tokens from fields
// TODO: ensure strict lexer structure: `struct Lexer(LexerData<Tokens>)`

pub fn derive_lexer_impl(input: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse2(input)?;
    let data = parse_attributes(&ast)?;

    let constructor = generate_constructor(&data);
    let tokenize = generate_tokenize_method(&data);

    let ident = &ast.ident;

    Ok(quote! {
        impl #ident {
            #constructor
            #tokenize
        }

    })
}

#[derive(Debug)]
struct MacroData {
    visibility: proc_macro2::TokenStream,
    skip_patterns: Vec<String>,
    tokens_enum: syn::Path,
}

fn parse_attributes(ast: &syn::DeriveInput) -> syn::Result<MacroData> {
    let attrs = &ast.attrs;

    let mut skip_patterns = Vec::new();
    let mut tokens_enum = None;

    for attr in attrs {
        if attr.path().is_ident("skip") {
            let skip_attr: SkipAttr = attr.parse_args()?;
            skip_patterns.push(skip_attr.0);
        } else if attr.path().is_ident("tokens") {
            if tokens_enum.is_some() {
                return Err(syn::Error::new(
                    attr.span(),
                    "Multiple token enums specified. Please specify only one.",
                ));
            }
            tokens_enum = Some(attr.parse_args()?);
        }
    }

    if None == tokens_enum {
        return Err(syn::Error::new(
            Span::call_site(),
            "No token enum specified. Please specify it with `#[tokens(path::to::Tokens)]`",
        ));
    }

    return Ok(MacroData {
        skip_patterns,
        tokens_enum: tokens_enum.unwrap(),
        visibility: ast.vis.to_token_stream(),
    });
}

struct SkipAttr(String);

impl syn::parse::Parse for SkipAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<SkipAttr> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected tokens. Please remove them.",
            ));
        }

        Ok(SkipAttr(pattern.value()))
    }
}

fn generate_constructor(data: &MacroData) -> proc_macro2::TokenStream {
    let MacroData {
        visibility,
        tokens_enum,
        ..
    } = data;

    let _LexerData = Symbol::UtilLexerData.to_token_stream();

    quote! {
        #visibility  fn new() -> Self {
            Self(#_LexerData::new(#tokens_enum::get_lex_rules()))
        }
    }
}

// TODO: from utils, export the paths for each DS used in the macros
// each module should export its own DS

fn generate_tokenize_method(data: &MacroData) -> proc_macro2::TokenStream {
    let MacroData {
        skip_patterns,
        tokens_enum,
        visibility,
    } = data;

    let _LexResult = Symbol::UtilLexResult.to_token_stream();
    let _LexError = Symbol::UtilLexError.to_token_stream();
    let _Vec = Symbol::Vec.to_token_stream();
    let _str = Symbol::Str.to_token_stream();
    let _Some = Symbol::Some.to_token_stream();
    let _None = Symbol::None.to_token_stream();
    let _Ok = Symbol::Ok.to_token_stream();
    let _Err = Symbol::Err.to_token_stream();

    quote! {
        #visibility fn tokenize(&self, input: &#_str) -> #_LexResult<#_Vec<#tokens_enum>> {
            let mut tokens = #_Vec::new();
            let mut input = input;
            let rules = &self.0.rules;

            while !input.is_empty() {
                let mut was_consumed = false;

                for rule in rules {
                    let matched_size = match rule.matches(input) {
                       #_None => continue,
                       #_Some(size) => size,
                    };

                    if let #_Ok((token, remaining)) = rule.consume(input, matched_size) {
                        tokens.push(token);
                        was_consumed = true;
                        input = remaining;
                        break;
                    }
                }
                if !was_consumed {
                    return #_Err(#_LexError::unknown(input));
                }
            }
            #_Ok(tokens)
        }
    }
}
