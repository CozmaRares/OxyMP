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

    quote! {
        #visibility  fn new() -> Self {
            Self(::oxymp_util::lexer::LexerData::new(#tokens_enum::get_lex_rules()))
        }
    }
}

fn generate_tokenize_method(data: &MacroData) -> proc_macro2::TokenStream {
    let MacroData {
        skip_patterns,
        tokens_enum,
        visibility,
    } = data;

    quote! {
        #visibility fn tokenize(&self, input: &str) -> ::oxymp_util::lexer::LexResult<::std::vec::Vec<#tokens_enum>> {
            let mut tokens = ::std::vec::Vec::new();
            let mut input = input;
            let rules = &self.0.rules;

            while !input.is_empty() {
                let mut was_consumed = false;

                for rule in rules {
                    let matched_size = match rule.matches(input) {
                       ::std::option::Option::None => continue,
                       ::std::option::Option::Some(size) => size,
                    };

                    if let ::std::result::Result::Ok((token, remaining)) = rule.consume(input, matched_size) {
                        tokens.push(token);
                        was_consumed = true;
                        input = remaining;
                        break;
                    }
                }
                if !was_consumed {
                    return Err(::oxymp_util::lexer::LexError::unknown(input));
                }
            }
            Ok(tokens)
        }
    }
}
