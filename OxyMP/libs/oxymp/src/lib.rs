use oxymp_lexer::derive_lexer_impl;
use oxymp_tokens::derive_tokens_impl;

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
