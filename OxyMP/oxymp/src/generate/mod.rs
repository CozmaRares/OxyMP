mod lexer;
mod tokens;

use lexer::LexerCache;

use crate::data::Data;

pub fn generate(data: Data) -> syn::Result<proc_macro2::TokenStream> {
    let mut module = data.initial_module;
    let Some((brace, mut items)) = module.content else {
        unreachable!("module without content")
    };

    items.extend(tokens::generate_structs(&data.tokens));

    let mut lexer_cache = LexerCache::new();

    for (item_mod, lexer_data) in data.lexers.into_iter() {
        items.extend(lexer::generate(
            &data.tokens,
            lexer_data,
            item_mod,
            &mut lexer_cache,
        )?);
    }

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
