use oxymp::Tokens;
use oxymp_util::lexer::{LexError, LexResult};

fn match_number(matched: &str) -> LexResult<Tok> {
    matched
        .parse()
        .map_err(|_| LexError::unparsable(matched))
        .map(|v| Tok::Number { value: v })
}

fn match_ident(matched: &str) -> LexResult<Tok> {
    if matched.len() == 1 {
        Ok(Tok::Identifier(matched.to_string()))
    } else {
        Err(LexError::unparsable(matched))
    }
}

#[derive(Tokens)]
enum Tok {
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

fn main() {}
