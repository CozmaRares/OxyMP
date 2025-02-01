#![allow(unused)]

use oxymp_new::{Lexer, RDParser, Tokens};
use oxymp_util::lexer::{LexError, LexResult};

use std::num::ParseIntError;

pub fn match_number(matched: &str) -> LexResult<i64> {
    matched.parse().map_err(|_| LexError::unparsable(matched))
}

pub fn match_ident(matched: &str) -> LexResult<String> {
    if matched.len() == 1 {
        Ok(matched.to_string())
    } else {
        Err(LexError::unparsable(matched))
    }
}

#[derive(Tokens)]
pub enum Tok {
    #[regex(
        r"\d+(.\d+)?",
        transform = match_number
        //tier = DefaultTokenTier::High
    )]
    Number { value: i64 },

    #[exact("while")]
    While,

    #[exact("(")]
    ParenLeft,

    #[exact(")")]
    ParenRight,

    #[exact("if")]
    If,

    #[exact("else")]
    Else,

    #[regex("[a-z]+", transform = match_ident)]
    Identifier(String),

    #[exact("=")]
    Equal,

    #[exact("+")]
    Plus,
}

//#[derive(Lexer)]
//#[tokens(Tok)]
//#[skip(regex = r"\s+")]
//pub struct Lexer;
//
//#[derive(RDParser)]
//#[tokens(Tok)]
//#[grammar_tokens(
//    While, ParenLeft, ParenRight, If, Else, Identifier, Equal, Plus, Number
//)]
//#[grammar = "EWh ::= While '(' E ')' E"]
//#[grammar = "EIf ::= If '(' E ')' E Else E"]
//#[grammar = "EEq ::= Identifier '=' E"]
//#[grammar = "EPl1 ::= '+' T EPl1?"]
//#[grammar = "EPl ::= T EPl1?"]
//#[grammar = "E ::= EWh | EIf | EEq | EPl"]
//#[grammar = "T ::= Number | Identifier | '(' E ')'"]
//pub struct Parser;
