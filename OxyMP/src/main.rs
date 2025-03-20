#[oxymp::oxymp]
mod language {
    use oxymp_util::lexer::{LexError, LexResult};

    pub fn match_number(matched: &str) -> LexResult<Tok> {
        matched
            .parse()
            .map_err(|_| LexError::unparsable(matched))
            .map(|v| Tok::Number { value: v })
    }

    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number { value: i64 },

        #[exact("(")]
        ParenLeft,

        #[exact(")")]
        ParenRight,

        #[exact("+")]
        Plus,

        #[exact("-")]
        Minus,
    }

    #[oxymp::Lexer]
    #[skip(r"\s+")]
    pub struct Lexer;

    #[oxymp::RDParser]
    #[grammar(E = T T1?)]
    #[grammar(T1 = ("+" | "-") E)]
    #[grammar(T = Number | "(" E ")")]
    pub struct RDParser;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    Ok(())
}
