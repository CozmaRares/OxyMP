use oxymp_lexer::derive_lexer_impl;
use oxymp_tokens::derive_tokens_impl;
use quote::quote;

#[cfg(feature = "rd")]
use oxymp_rd::derive_rd_impl;

#[cfg(feature = "lr")]
use oxymp_lr::derive_lr_impl;

#[proc_macro_derive(Tokens, attributes(exact, regex))]
pub fn derive_tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_tokens_impl(input.into()) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[proc_macro_derive(Lexer, attributes(tokens, skip))]
pub fn derive_lexer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_lexer_impl(input.into()) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[cfg(feature = "rd")]
#[proc_macro_derive(RDParser, attributes(grammar, tokens, grammar_tokens))]
pub fn derive_rd(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_rd_impl(input.into()) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[cfg(feature = "lr")]
#[proc_macro_derive(LRParser, attributes(grammar, tokens, grammar_tokens))]
pub fn derive_lr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_lr_impl(input.into()) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

// All the credit goes to
// experiment & starting point - https://github.com/faassen/outer_macro_pattern
// the *only* video explaining the outer macro pattern  - https://www.youtube.com/watch?v=aEWbZxNCH0A
use syn::{parse_macro_input, spanned::Spanned};

#[proc_macro_attribute]
pub fn outer(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_mod = parse_macro_input!(item as syn::ItemMod);
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    eprintln!("{:#?}", &item_mod.content);

    if let Some((_, items)) = &item_mod.content {
        for item in items {
            match item {
                syn::Item::Struct(item_struct) => {
                    if has_inner_attribute(&item_struct.attrs) {
                        structs.push(item_struct);
                    }
                }
                syn::Item::Fn(item_fn) => {
                    if has_inner_attribute(&item_fn.attrs) {
                        functions.push(item_fn);
                    }
                }
                _ => {}
            }
        }
        // just print the structs and functions for now
        for item_struct in &structs {
            eprintln!("struct: {:#?}", item_struct.ident);
        }
        for item_fn in &functions {
            eprintln!("function: {:#?}", item_fn.sig.ident);
        }
    } else {
        // marking a module with #[outer] only makes sense if it has content
        // generate proper syn error with span
        return syn::Error::new(
            item_mod.span(),
            "module marked with #[outer] must have content",
        )
        .to_compile_error()
        .into();
    }

    quote! {
        #item_mod
    }
    .into()
}

fn has_inner_attribute(attributes: &[syn::Attribute]) -> bool {
    for attribute in attributes {
        if attribute.path().is_ident("inner") {
            return true;
        }
    }
    false
}

/// Mark a struct or function with #[inner] to be found by the #[outer] macro
#[proc_macro_attribute]
pub fn inner(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}
