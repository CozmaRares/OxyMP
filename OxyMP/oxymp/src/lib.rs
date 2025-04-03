#[macro_use]
mod utils;

mod data;
mod generate;
mod grammar;

use syn::spanned::Spanned;

use crate::data::process_module;

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

// TODO: macro_rules! to generate these 2 things
pub(crate) const OXYMP_ATTR: &str = "oxymp";

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
