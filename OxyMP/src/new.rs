#![allow(unused)]

use oxymp::{Lexer, RDParser, Tokens};
use oxymp_util::lexer::{LexError, LexResult, LexerData};

pub fn match_number(matched: &str) -> LexResult<Tok> {
    matched
        .parse()
        .map_err(|_| LexError::unparsable(matched))
        .map(|v| Tok::Number { value: v })
}

pub fn match_ident(matched: &str) -> LexResult<Tok> {
    if matched.len() == 1 {
        Ok(Tok::Identifier(matched.to_string()))
    } else {
        Err(LexError::unparsable(matched))
    }
}

#[derive(Debug, Tokens)]
pub enum Tok {
    #[regex(r"\d+(.\d+)?", match_number)]
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

    #[regex("[a-z]+", match_ident)]
    Identifier(String),

    #[exact("=")]
    Equal,

    #[exact("+")]
    Plus,
}

#[derive(Lexer)]
#[tokens(Tok)]
#[skip(r"\s+")]
pub struct Lexer(LexerData<Tok>);
// TODO: lexer is broken for some reason
// 1+2 -> unknown

#[derive(RDParser)]
#[tokens(Tok)]
#[grammar("EWh ::= While '(' E ')' E")]
#[grammar("EIf ::= If '(' E ')' E Else E")]
#[grammar("EEq ::= Identifier '=' E")]
#[grammar("EPl1 ::= '+' T EPl1?")]
#[grammar("EPl ::= T EPl1?")]
#[grammar("E ::= EWh | EIf | EEq | EPl")]
#[grammar("T ::= Number | Identifier | '(' E ')'")]
pub struct Parser;

