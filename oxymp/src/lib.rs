// All APIs need docs!
// #![deny(missing_docs)]
// TODO: rustdoc compatible documentation
// TODO: see if you can include structs only when generating docs
// to be used to document the generated data structures

// TODO: reorganize the order of functions and data structures in all the modules

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

/// A procedural macro attribute used to generate a lexer and recursive descent
/// parser.
///
/// This macro simplifies the process of creating a parser for a language by:
///
/// * Generating a lexer based on regular expressions and exact matches defined
/// within a `Tokens` enum.
/// * Generating a recursive descent parser based on a grammar defined via the
/// `grammar` attribute.
///
/// The `oxymp` attribute must be applied to a Rust module (`mod`).
/// This module should contain the definitions for:
///
/// * Only one token enum marked with `#[oxymp::Tokens]`.
/// * Zero or more lexer modules marked with `#[oxymp::Lexer]`.
/// * Zero or more recursive descent parser modules marked with
/// `#[oxymp::RDParser]`.
///
/// **Example**
///
/// ```rust
/// #[oxymp::oxymp]
/// mod language {
///     use std::num::ParseFloatError;
///
///     pub fn match_number(input: &str) -> Result<f64, ParseFloatError> {
///         input.parse()
///     }
///
///     #[derive(Debug)]
///     #[oxymp::Tokens]
///     pub enum Tok {
///         #[regex(r"\d+(\.\d+)?", match_number)]
///         Number(f64),
///
///         #[exact(r"\+")]
///         Plus,
///
///         #[exact("-")]
///         Minus,
///     }
///
///     impl std::fmt::Display for Tok {
///         fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
///             match self {
///                 Tok::Number(n) => write!(f, "{}", n),
///                 Tok::Plus => write!(f, "+"),
///                 Tok::Minus => write!(f, "-"),
///             }
///         }
///     }
///
///     #[oxymp::Lexer]
///     #[skip(r"[ \t]+")]
///     #[error(ParseFloatError)]
///     pub mod lexer {}
///
///     #[oxymp::RDParser]
///     #[grammar(T = Number (r"\+" | "-") Number)]
///     pub mod rd_parser {}
/// }
/// use language::*;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let input = "1 + 2";
///     println!("{:#?}", input);
///
///     let tokens = lexer::tokenize(input)?;
///     println!("{:#?}", tokens);
///
///     let (_inp, ast) = rd_parser::T(tokens.into())?;
///     println!("{:#?}", ast);
///
///     Ok(())
/// }
/// ```
///
/// **Token Definition (`#[oxymp::Tokens]`)**
///
/// The `Tokens` enum defines the set of tokens that the lexer will recognize.
/// Each variant of the enum represents a different token type. The following
/// attributes can be applied to token variants:
///
/// * `#[regex(pattern, match_function)]`:  Matches the token based on a
///   regular expression. The `pattern` is a regular expression string.
///   The `match_function` is a function that takes a `&str` (the matched
///   text) as input and returns a `Result<T, E>`, where `T` is the type of
///   the token's value and `E` is type that implements the `std::error::Error`
///   trait.
/// * `#[exact(pattern)]`: Matches the token exactly against the given
///   `pattern` string.
///
/// A module marked with `#[oxymp]` must contain only one token enum.
///
/// An enum marked with `#[oxymp::Tokens]` must implement the `std::fmt::Display`
/// trait.
///
/// **Lexer Module (`#[oxymp::Lexer]`)**
///
/// The `Lexer` module configures the lexer. The following attributes can be
/// applied to the `Lexer` module:
///
/// * `#[skip(pattern)]`: Specifies a regular expression that matches text
///   to be skipped (e.g., whitespace).
/// * `#[error(error_type)]`: Specifies the error type to use when tokenizing.
///
/// The lexer will generate:
///
/// * An `Error` eunm that is used to report errors during tokenization. It has
///   two variants:
///   * `Native` => the error was caused by the lexer itself when an unexpedted
///   character was encountered.
///   * `UserMatcher` => the error was caused by functions defined by the user
///   for a `#[regex(...)]` token definition.
/// * A `tokenize` function that takes a `&str` as input and returns a
///   `Result<Vec<Token>, Error>`, where `Token` is the token enum defined with
///   `#[oxymp::Tokens]` and `Error` is the lexer's Error type.
///
/// A module marked with `#[oxymp]` *may* contain *zero* or *more* lexer modules.
///
/// **Parser Module (`#[oxymp::RDParser]`)**
///
/// The `RDParser` module configures the recursive descent parser.
///
/// To define a production rule, the following attribute can be applied to the
/// `RDParser` module: `#[grammar(ProductionName = ...)]`
///
/// * The `ProductionName` is the name of the production (e.g., `T` in the example).
/// * The right-hand side of the production specifies the sequence of tokens and/or
/// other productions that make up the production.
///   * *Token names* used in the grammar *must* match the variant names in the
///   `Tokens` enum or, in case of `#[exact(pattern)]` definition, the *pattern*
///   can also be used.
///   * A sequence can be indicated by `A B C`, where `A`, `B`, and `C` are tokens
///   or productions.
///   * Choice can be indicated by `sequence A | sequence B`.
///   * Optionality can be indicated by `sequence?`.
///
/// Token names used in the grammar
/// *must* match the variant names in the `Tokens` enum. `Choice` can be indicated
/// by `sequence A | sequence B`.
///
/// The parser module will generate:
///
/// * An `Input` struct that is used to pass the input to the parser. The
/// `From<Vec<Token>>` trait is implemented for this struct.
/// * An `ErrorKind` eunm that has the following variants:
///   * `UnexpectedEOf` => the parser encountered the end of input.
///   * `UnexpectedToken` => the parser encountered an unexpected token.
///   * `ChoiceFailed` => the parser failed to match a choice production.
///     * `rule_name` => the name of the rule that failed.
///     * `choice_idx` => the index of the choice that failed (starting from 1).
///     * `causes` => a vector of errors that were encountered while trying to
///       parse each branch of the choice.
/// * An `Error` struct that is used to report errors during parsing. It has:
///   * `kind` => the error kind.
///   * `input` => the input that caused the error.
///   * `cursor` => the position of the token that caused the error.
///   * `expected` => a vector of expected token names.
/// * A function for each production rule defined with the `grammar` attribute.
///   These functions take a slice of tokens as input and return a
///   `Result<(/* remaining tokens */, AST node), Error>`, where `AST node` is a
///   struct generated based on the right-hand side of the grammar rule, and `Error`
///   is the parser's Error type.
/// * Generated AST structures:
///   * Rule productions are wrapped in a `Box` to avoid recursive types.
///   * Tokens are converted to the corresponding enum variants prepended with
///   the name of the tokens enum, e.g., `Tok::Number(n)` becomes `TokNumber(n)`.
///   * Sequences are converted to a tuple of the corresponding AST nodes.
///   * Option productions are wrapped in an `Option`.
///   * Choice productions generate an `enum` with each branch as a variant.
///     * The `enum` is prepended with the name of the rule and appended with its
///     index in the definition , e.g., choice `1` of rule `T` becomes `TChoice1`.
///     * Each branch of the choice is represented by its index in the definition,
///     e.g., branch `1` becomes `_1`.
///     * Combining the two, the first branch of the first choice of rule `T`
///     becomes `TChoice1::_1(...)`.
///
/// A module marked with `#[oxymp]` *may* contain *zero* or *more* recursive
/// descent parser modules.
///
/// A great tool to see the generated code is [dtolnay's cargo expand](https://github.com/dtolnay/cargo-expand)
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
