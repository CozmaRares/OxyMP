mod data;
mod generate;
mod grammar;
mod utils;

use quote::quote;
use syn::spanned::Spanned;

use crate::{data::process_module, utils::OXYMP_ATTR};

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

    let grammars = data
        .rd_parsers
        .into_iter()
        .map(|rdp| grammar::parse_grammar(&data.tokens, rdp.grammar_rules))
        .collect::<Result<Vec<_>, _>>()?;
    eprintln!("{:#?}", grammars);

    items.extend(generate::tokens::generate_structs(&data.tokens));

    item_mod.content = Some((brace, items));

    Ok(quote! {
        #item_mod
    })
}

#[proc_macro_attribute]
pub fn oxymp(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match oxymp_impl(item) {
        Ok(tokens) => tokens,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
