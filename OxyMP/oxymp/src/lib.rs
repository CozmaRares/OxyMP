// DOC: include this
// All the credit goes to
// experiment & starting point - https://github.com/faassen/outer_macro_pattern
// the *only* video explaining the outer macro pattern  - https://www.youtube.com/watch?v=aEWbZxNCH0A

mod data;
mod generate;
mod utils;

use quote::quote;
use syn::spanned::Spanned;

use crate::{data::process_module, generate::generate, utils::OXYMP_ATTR};

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
            format!("Module marked with #[{OXYMP_ATTR}] must have content.",),
        ));
    };

    let (data, mut items) = process_module(items, &item_mod.ident)?;

    eprintln!("{:#?}", data);

    let generated_items = generate(data);
    items.extend(generated_items);

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
