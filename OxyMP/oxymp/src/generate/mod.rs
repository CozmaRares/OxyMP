mod tokens;

use crate::data::Data;

pub fn generate(data: Data) -> syn::Result<proc_macro2::TokenStream> {
    let mut module = data.initial_module;
    let Some((brace, mut items)) = module.content else {
        unreachable!("module without content")
    };

    items.extend(tokens::generate_structs(&data.tokens));

    module.content = Some((brace, items));
    Ok(q! { #module })
}

// let mut token_nfas = Vec::new();
//
// if !data.lexers.is_empty() {
//     items.extend(generate::lexer::generate_error_struct());
// }
//
// // TODO: cache DFAs
// for (idx, variant) in data.tokens.variants.iter().enumerate() {
//     let pattern = match &variant.pattern {
//         data::tokens::TokenPattern::Exact { pattern } => pattern.as_str(),
//         data::tokens::TokenPattern::Regex { pattern, transform } => pattern.as_str(),
//     };
//
//     let nfa = nfa::compile(
//         pattern,
//         nfa::StateTag::Token {
//             variant: variant.ident.to_string(),
//             priority: idx,
//         },
//     )
//     .map_err(|e| {
//         syn::Error::new(
//             variant.pattern_span,
//             format!(
//                 "Error while compiling regex pattern for token variant '{}'\n{}",
//                 variant.ident, e
//             ),
//         )
//     })?;
//
//     token_nfas.push(nfa);
// }
//
// for lexer_data in data.lexers {
//     let mut skip_nfas = Vec::new();
//
//     for pattern_lit in &lexer_data.skip_patterns {
//         let pattern = pattern_lit.value();
//         let nfa = nfa::compile(
//             &pattern,
//             nfa::StateTag::Skip {
//                 lexer: lexer_data.ident.to_string(),
//                 pattern: pattern.clone(),
//             },
//         )
//         .map_err(|e| {
//             syn::Error::new(
//                 pattern_lit.span(),
//                 format!(
//                     "Error while compiling regex pattern for skip pattern '{}'\n{}",
//                     pattern, e
//                 ),
//             )
//         })?;
//
//         skip_nfas.push(nfa);
//     }
//
//     let nfas = [skip_nfas, token_nfas.clone()].concat();
//     let nfa = nfa::combine(nfas);
//     let dfa = dfa::compile(nfa);
//
//     let generated_items = generate::lexer::generate(&data.tokens, lexer_data, dfa)?;
//     items.extend(generated_items);
// }
//
// #[cfg(feature = "rd")]
// {
//     let parsers = data
//         .rd_parsers
//         .into_iter()
//         .map(|rd_data| generate::rd::generate_mod(&data.tokens, rd_data))
//         .collect::<Result<Vec<syn::Item>, syn::Error>>()?;
//     items.extend(parsers);
// }
//
// item_mod.content = Some((brace, items));
