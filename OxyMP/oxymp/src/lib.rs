// TODO: remove
#![allow(unused)]

// TODO: uncomment
// All APIs need docs!
// #![deny(missing_docs)]

// TODO: change all unwraps to expect where no explanation is needed

#[macro_use]
mod macros;

mod automata;
mod data;
mod generate;
mod grammar;
mod utils;

use syn::spanned::Spanned;

use crate::{
    automata::{dfa, nfa},
    data::process_module,
};

fn oxymp_impl(item: proc_macro::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let mut item_mod: syn::ItemMod = match syn::parse(item) {
        Ok(o) => o,
        Err(e) => {
            return Err(syn::Error::new(
                e.span(),
                "Could not parse module. Make sure the attribute is applied to a module.",
            ))
        }
    };

    let Some((brace, items)) = item_mod.content else {
        return Err(syn::Error::new(
            item_mod.span(),
            format!("Module marked with #[{OXYMP_ATTR}] must have content."),
        ));
    };

    let (data, mut items) = process_module(items, &item_mod.ident)?;

    items.extend(generate::tokens::generate_structs(&data.tokens));

    let mut token_nfas = Vec::new();

    // TODO: cache DFAs
    for (idx, variant) in data.tokens.variants.iter().enumerate() {
        let pattern = match &variant.pattern {
            data::tokens::TokenPattern::Exact { pattern } => pattern.as_str(),
            data::tokens::TokenPattern::Regex { pattern, transform } => pattern.as_str(),
        };

        let nfa = nfa::compile(
            pattern,
            nfa::StateTag::Token {
                variant: variant.ident.clone(),
                priority: idx,
            },
        )
        .map_err(|e| {
            syn::Error::new(
                variant.pattern_span,
                format!(
                    "Error while compiling regex pattern for token variant '{}'\n{}",
                    variant.ident, e
                ),
            )
        })?;

        token_nfas.push(nfa);
    }

    let mut lexer_dfas = Vec::new();

    for lexer_data in &data.lexers {
        let mut skip_nfas = Vec::new();

        for (pattern, span) in &lexer_data.skip_patterns {
            let nfa = nfa::compile(
                pattern,
                nfa::StateTag::Skip {
                    lexer: lexer_data.ident.clone(),
                    pattern: pattern.to_string(),
                },
            )
            .map_err(|e| {
                syn::Error::new(
                    *span,
                    format!(
                        "Error while compiling regex pattern for skip pattern '{}'\n{}",
                        pattern, e
                    ),
                )
            })?;

            skip_nfas.push(nfa);
        }

        skip_nfas.extend(token_nfas.clone());

        let lexer_nfa = nfa::combine(skip_nfas);
        let lexer_dfa = dfa::compile(lexer_nfa);
        eprintln!("{:#?}", lexer_dfa);
        lexer_dfas.push(lexer_dfa);
    }

    #[cfg(feature = "rd")]
    {
        let parsers = data
            .rd_parsers
            .into_iter()
            .map(|rd_data| generate::rd::generate_mod(&data.tokens, rd_data))
            .collect::<Result<Vec<syn::Item>, syn::Error>>()?;
        items.extend(parsers);
    }

    item_mod.content = Some((brace, items));

    Ok(q! { #item_mod })
}

pub(crate) const OXYMP_ATTR: &str = "oxymp";

#[proc_macro_attribute]
pub fn oxymp(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if !attr.is_empty() {
        let msg = "Invalid 'oxymp' attribute. The attribute must not have any arguments, such as `#[oxymp::oxymp]` or `#[oxymp]`.";
        let toks = proc_macro2::TokenStream::from(attr);
        let span = toks.span();
        return syn::Error::new(span, msg).to_compile_error().into();
    }

    match oxymp_impl(item) {
        Ok(tokens) => tokens,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
