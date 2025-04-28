// TODO: remove
#![allow(unused)]

// TODO: uncomment
// All APIs need docs!
// #![deny(missing_docs)]

// TODO: change all unwraps to expect where no explanation is needed
// TODO: create a 'symbols' module with all the symbols used for the std lib

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

    if !data.lexers.is_empty() {
        items.extend(generate::lexer::generate_error_struct());
    }

    // TODO: cache DFAs
    for (idx, variant) in data.tokens.variants.iter().enumerate() {
        let pattern = match &variant.pattern {
            data::tokens::TokenPattern::Exact { pattern } => pattern.as_str(),
            data::tokens::TokenPattern::Regex { pattern, transform } => pattern.as_str(),
        };

        let nfa = nfa::compile(
            pattern,
            nfa::StateTag::Token {
                variant: variant.ident.to_string(),
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

    for lexer_data in data.lexers {
        let mut skip_nfas = Vec::new();

        for pattern_lit in &lexer_data.skip_patterns {
            let pattern = pattern_lit.value();
            let nfa = nfa::compile(
                &pattern,
                nfa::StateTag::Skip {
                    lexer: lexer_data.ident.to_string(),
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

            skip_nfas.push(nfa);
        }

        let nfas = [skip_nfas, token_nfas.clone()].concat();
        let nfa = nfa::combine(nfas);
        let dfa = dfa::compile(nfa);

        let generated_items = generate::lexer::generate(&data.tokens, lexer_data, dfa)?;
        items.extend(generated_items);
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
