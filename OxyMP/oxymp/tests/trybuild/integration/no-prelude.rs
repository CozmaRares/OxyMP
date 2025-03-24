#![no_implicit_prelude]
#![allow(non_camel_case_types)]

extern crate oxymp;

/// This test checks that the generated code does not use anything re-exported by prelude.
///
/// This test also rewrites all primitives (still usable through ::core::primitive::*) since
/// Rust allows them to be rewritten and a user of the library may want to rewrite them.

type bool = ();
type char = ();
type f32 = ();
type f64 = ();
type i8 = ();
type i16 = ();
type i32 = ();
type i64 = ();
type i128 = ();
type isize = ();
type str = ();
type u8 = ();
type u16 = ();
type u32 = ();
type u64 = ();
type u128 = ();
type usize = ();

#[oxymp::oxymp]
mod language {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number { value: i64 },

        #[exact("while")]
        While,

        #[regex("[a-z]+", match_ident)]
        Identifier(::std::string::String),
    }

    #[oxymp::Lexer]
    #[skip(r"\s+")]
    struct Lexer;

    #[oxymp::RDParser]
    #[grammar(EWh = While "(" E ")" E)]
    #[grammar(EIf = If "(" E ")" E Else E)]
    #[grammar(EEq = Identifier "=" E)]
    #[grammar(EPl1 = "+" T EPl1?)]
    #[grammar(EPl = T EPl1?)]
    #[grammar(E = EWh | EIf | EEq | EPl)]
    #[grammar(T = Number | Identifier | "(" E ")")]
    struct Parser;
}

fn main() {}
