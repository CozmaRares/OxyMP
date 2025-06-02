// TODO: uncomment
// All APIs need docs!
// #![deny(missing_docs)]

// TODO: change all unwraps to expect where no explanation is needed
// TODO: reorganize the order of functions and data structures in all the modules
// TODO: use assert! more often
// TODO: use Vec::with_capacity whenever possible

#[macro_use]
mod macros;

#[cfg(debug_assertions)]
#[macro_use]
mod dbg_macros;

mod automata;
mod data;
mod generate;
mod grammar;
mod idents;
mod range;
mod symbols;
mod utils;

use data::process_module;
use generate::generate;
use syn::spanned::Spanned;

#[proc_macro_attribute]
pub fn oxymp(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if !attr.is_empty() {
        let msg = "Invalid 'oxymp' attribute. The attribute must not have any arguments, such as `#[oxymp]`,`#[oxymp::oxymp]` or `#[::oxymp::oxymp]`.";
        let toks = proc_macro2::TokenStream::from(attr);
        let span = toks.span();
        return syn::Error::new(span, msg).to_compile_error().into();
    }

    match syn::parse::<syn::ItemMod>(item)
        .and_then(process_module)
        .and_then(generate)
    {
        Ok(tokens) => tokens,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
